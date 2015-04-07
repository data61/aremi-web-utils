{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module APVI.LiveSolar (
    -- Web interface
    APVILiveSolar,
    makeLiveSolarServer,
    --Types
    SvgBS(..),
    CsvBS(..),
    AppState(..),
    --
    updateRef,
    every,
    isErr,
    initialiseLiveSolar,
    -- Lenses
    contributionGraphs,
    performanceGraphs,
    contributionCSV,
    performanceCSV
    ) where


import           Data.List                                 (intercalate, sortBy)
import           Data.Maybe                                (maybeToList)
-- import           Data.Monoid                               ((<>))
import           Data.Ord                                  (comparing)

import           Control.Applicative
import           Control.Arrow                             (second)
import           Control.Monad                             (forM_)

-- import Data.Default (Default(..))

import           Data.ByteString                           ()
import qualified Data.ByteString                           as S
import           Data.ByteString.Lazy                      (ByteString)
import qualified Data.ByteString.Lazy                      as BSL

import           Data.Text                                 (Text, unpack)
import qualified Data.Text                                 as T

import           Data.Text.Encoding                        (decodeUtf8',
                                                            encodeUtf8)


import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H

import           Data.Csv                                  (defaultEncodeOptions,
                                                            encodeByNameWith,
                                                            toField)
import qualified Data.Csv                                  as Csv
-- import           Data.Vector                               (Vector)
import qualified Data.Vector                               as V

import           Control.Lens                              as L
import           Data.Aeson                                (Value)
import qualified Data.Aeson                                as A
import           Data.Aeson.Lens                           as AL
import           Data.Text.Lens


-- Chart stuff
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy

import           Data.Time.Calendar                        (Day)
import           Data.Time.Clock                           (NominalDiffTime,
                                                            UTCTime, addUTCTime,
                                                            diffUTCTime,
                                                            getCurrentTime)
import           Data.Time.Format                          (formatTime,
                                                            parseTime)
import           Data.Time.LocalTime                       (LocalTime (..),
                                                            TimeZone,
                                                            ZonedTime (..),
                                                            getZonedTime,
                                                            timeZoneName,
                                                            utcToLocalTime)
import           System.Locale                             (defaultTimeLocale)

import qualified System.Log.Logger                         as HSL
import           System.Log.Logger.TH                      (deriveLoggers)


import           Data.IORef                                (IORef, readIORef,
                                                            writeIORef)

import           Control.Monad.Catch                       (Handler (..),
                                                            SomeException,
                                                            catch, catches)
import           Control.Monad.IO.Class


import           Network.HTTP.Conduit                      (HttpException (StatusCodeException),
                                                            Manager,
                                                            Request (..),
                                                            httpLbs, parseUrl,
                                                            responseBody,
                                                            responseHeaders,
                                                            withManager)
import           Network.HTTP.Types.Status                 (Status (..),
                                                            status200)

import           Control.Concurrent
import           Control.Concurrent.Async                  (async,
                                                            mapConcurrently,
                                                            wait)
import           Control.Retry                             (fibonacciBackoff,
                                                            limitRetries,
                                                            retrying)
import           Data.IORef                                (newIORef)
import           Data.Time.Units                           hiding (Day)

import           Network.Wai                               (Application,
                                                            requestHeaderHost)
import Network.Wai.Util
import           Servant
import           Servant.Docs


$(deriveLoggers "HSL" [HSL.DEBUG, HSL.ERROR, HSL.WARNING])

newtype SvgBS = SvgBS {unSvg :: ByteString}
newtype CsvBS = CsvBS {unCsv :: ByteString}

type ETag = S.ByteString


data AppState = AppState {
      _timeFetched        :: !(Maybe ZonedTime)
    , _latestETag         :: !(Maybe ETag)
    , _contributionCSV    :: Text -> Maybe CsvBS
    , _contributionGraphs :: !(HashMap Text SvgBS)
    , _performanceCSV     :: Text -> Maybe CsvBS
    , _performanceGraphs  :: !(HashMap Text SvgBS)
}

$(makeLenses ''AppState)


instance Default AppState where
    def = AppState {
        _timeFetched        = Nothing,
        _latestETag         = Nothing,
        _contributionCSV    = const Nothing,
        _contributionGraphs = H.empty,
        _performanceCSV     = const Nothing,
        _performanceGraphs  = H.empty
    }

states :: [(Text, Int)]
states = [
    ("nsw",1),
    ("vic",2),
    ("qld",3),
    ("sa",4),
    ("tas",6),
    ("wa",5)
    ]

type APVILiveSolar =
    "performance" :>
        ("csv" :> Raw
        :<|> Capture "svgstate" Text :> "svg" :> Raw)
    :<|>
    "contribution" :>
        ("csv" :> Raw
        :<|> Capture "svgstate" Text :> "svg" :> Raw)


instance ToCapture (Capture "svgstate" Text) where
    toCapture _ = DocCapture "svgstate" $
        "Australian State name, currently supported are: all, "
        ++ intercalate ", " (map (T.unpack . fst) states)

makeLiveSolarServer :: IO (Either String (Server APVILiveSolar))
makeLiveSolarServer = do
    eref <- initialiseLiveSolar
    case eref of
        Left str -> return $ Left str
        Right ref -> return $ Right (
            (serveCSV ref performanceCSV
                :<|> serveSVG ref performanceGraphs)
            :<|> (serveCSV ref contributionCSV
                :<|> serveSVG ref contributionGraphs)
            )


initialiseLiveSolar :: IO (Either String (IORef AppState))
initialiseLiveSolar = do
    ref <- newIORef def

    success <- updateRef 20 ref
    if success
        then do
            _tid <- updateRef 10 ref `every` (5 :: Minute)
            return $ Right ref
        else return $ Left "Failed to initialise live solar data"


serveCSV :: IORef AppState -> Getter AppState (Text -> Maybe CsvBS) -> Application
serveCSV ref lns req respond = do
        current <- readIORef ref
        let mhost = requestHeaderHost req
        let makeCSV = case mhost of
                Nothing -> Nothing
                Just hbs -> case decodeUtf8' hbs of
                    Left _err -> Nothing
                    Right txt -> (current ^. lns) txt
        case makeCSV of
            Nothing -> error "TODO: fixme"
            Just (CsvBS bs) -> do
                respond =<< bytestring status200 [("Content-Type", "text/csv")] bs




serveSVG :: IORef AppState -> Getter AppState (HashMap Text SvgBS) -> Text -> Application
serveSVG ref lns stat _req respond = do
    current <- readIORef ref

    case H.lookup stat (current ^. lns) of
              Nothing -> error "TODO: fixme"
              Just (SvgBS bs) -> do
                    respond =<< bytestring status200 [("Content-Type", "image/svg+xml")] bs



-- Takes a number of retries and the current app state ref and attempts to contact APVI for the latest
-- data for today.
updateRef :: Int -> IORef AppState -> IO Bool
updateRef retries ref = flip catch (\e -> (warningM  . show $ (e :: SomeException)) >> return False) $ do
    now <- getZonedTime
    let day = localDay . zonedTimeToLocalTime $ now
        tz = zonedTimeZone now

    current <- readIORef ref
    ejsn <- withManager $ \m ->
            liftIO $ retrying (fibonacciBackoff 1000 <> limitRetries retries)
                     (\_ e -> return (isErr e))
                     $ fetchDate m day (_latestETag current)
    case ejsn of
        Err err -> errorM err >> return False
        NoChange -> debugM "update not necessary" >> return True
        NewData metag (fetched, jsn) -> do
            allPerfSvgs' <- async $ renderCharts (fetched, jsn) tz "performance"  _Double
            allContSvgs' <- async $ renderCharts (fetched, jsn) tz "contribution" (_String . unpacked . _Show)

            (allPerfSvgs,perfCSV) <- wait allPerfSvgs'
            (allContSvgs,contCSV) <- wait allContSvgs'

            let svgSize = foldl (\n (_,bs) -> n + BSL.length (unSvg bs)) 0 (allContSvgs ++ allPerfSvgs)

            debugM $ "Total SVG size: " ++ show svgSize

            let !newState = current
                    & contributionGraphs .~ H.fromList allContSvgs
                    & performanceCSV     .~ perfCSV
                    & contributionCSV    .~ contCSV
                    & performanceGraphs  .~ H.fromList allPerfSvgs
                    & timeFetched        .~ Just now
                    & latestETag         .~ (metag <|> current ^. latestETag)

            liftIO . writeIORef ref $ newState
            -- performMajorGC
            return True
    where
        renderCharts :: (UTCTime,Value) -> TimeZone -> Text
                     -> Prism' Value Double
                     -> IO ([(Text,SvgBS)],Text -> Maybe CsvBS)
        renderCharts (fetched,jsn) tz title lns = do
            let
                vals :: [Value]
                vals = jsn ^.. key title . values

                allStates :: [(Text,[Maybe (UTCTime,Double)])]
                allStates = map (\(name,_) -> (name, getTS lns name vals)) states

                titleStr :: String
                titleStr = "All states " ++ T.unpack title

                allTitle :: Text
                allTitle = T.pack $ formatTime defaultTimeLocale (titleStr ++ " (%%) %F %X %Z") fetched

                allChart :: Renderable ()
                allChart = createContributionChart tz allTitle allStates

                csvHeader :: Csv.Header
                csvHeader = V.fromList [encodeUtf8 title,"State","Time","Image"] :: V.Vector S.ByteString

                currentValues :: [(Text,Maybe (UTCTime, Double))]
                currentValues = map (second maximum) allStates

                namedRecords hst = map (\(state, Just (time, val))
                                    -> H.fromList [("State", toField $ lookup state states)
                                                  ,("Time", toField $ formatTime defaultTimeLocale "%FT%X" time)
                                                  ,(encodeUtf8 title, toField val)
                                                  ,("Image", toField $ T.concat ["<img src='http://",hst,"/"
                                                                                ,title,"/",state,"/svg'/>"])
                                                  ]

                                    )
                                    currentValues
                csv hst = Just $ CsvBS $ encodeByNameWith defaultEncodeOptions csvHeader (namedRecords hst)

            debugM $ "Rendering " ++ titleStr ++ " SVGs"
            (allsvg',_) <- liftIO $ renderableToSVGString allChart 500 300
            let !allsvg = SvgBS $ unchunkBS allsvg'

            ssvgs <- liftIO $ flip mapConcurrently states $ \(sname,_) -> do
                    let fullTitle = T.toUpper sname <> " " <> title <> " (%)"
                        chart = createContributionChart tz fullTitle [(sname,getTS lns sname vals)]
                    (ssvg',_) <- renderableToSVGString chart 500 300
                    let !ssvg = SvgBS $ unchunkBS ssvg'
                    return (sname, ssvg)
            debugM $ "Done rendering " ++ titleStr ++ " SVGs"



            return $ (("all",allsvg):ssvgs,csv)


unchunkBS :: ByteString -> ByteString
unchunkBS = BSL.fromStrict . BSL.toStrict



--- | Run an event every n time units. Does not guarantee that it will be run
--- each occurance of n time units if the action takes longer than n time to run.
--- It will run each action at the next block of n time (it can miss deadlines).
every :: TimeUnit t => IO a -> t -> IO (ThreadId,MVar ())
every act t = do
    mv <- newEmptyMVar

    now <- getCurrentTime
    let ms = toMicroseconds t
        ps = ms * 1000000
        d = toEnum . fromIntegral $ ps :: NominalDiffTime
        activations = iterate (addUTCTime d) now

    tid <- forkIO (run activations >> putMVar mv ())
    return (tid, mv)
    where
        run :: [UTCTime] -> IO ()
        run ts = do
            _ <- act
            now <- getCurrentTime
            case dropWhile (<= now) ts of
                (nxt:ts') ->
                    let delay = fromEnum (diffUTCTime nxt now) `div` 1000000
                    in threadDelay delay >> run ts'





data FetchResponse a
    = Err String
    | NoChange
    | NewData (Maybe ETag) a

isErr :: FetchResponse a -> Bool
isErr (Err _) = True
isErr _       = False

fetchDate :: Manager -> Day -> Maybe ETag -> IO (FetchResponse (UTCTime, Value))
fetchDate manager day metag = do
    let url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day
    initReq <- parseUrl url
    let req = initReq {
            requestHeaders = [
                ("Accept"          , "application/json, text/javascript, */*"),
                ("X-Requested-With", "XMLHttpRequest")
            ]
            ++ maybeToList ((,) "If-None-Match" <$> metag)
        }
    debugM $ "Fetching " ++  url

    debugM $ show req

    catches (do
        ts <- getCurrentTime
        rsp <- httpLbs req manager
        debugM $ show (rsp {responseBody = ()})
        case A.eitherDecode' (responseBody rsp) of
            Left str ->
                return $ Err str
            Right val ->
                return $ NewData (lookup "ETag" (responseHeaders rsp)) (ts, val)

        )
        [
            Handler $ \e -> case e of
                -- If we get a 306 Not Modified there's nothing more to do
                StatusCodeException (Status {statusCode = 304}) _ _ ->
                    return $ NoChange
                _ ->
                    return $ Err (show e),
            Handler $ \e -> return $ Err (show (e :: SomeException))
        ]



getTS :: Prism' Value a -> Text -> [Value] -> [Maybe (UTCTime, a)]
getTS f state objs =
    let timeParser = parseTime defaultTimeLocale "%FT%H:%M:%SZ"
        timeLens   = key "ts" . _String . unpacked . to timeParser . _Just
        stateLens  = key state . f
    in sortBy (comparing (fmap fst))
       . Prelude.map (\v -> (,) <$> v ^? timeLens <*> v ^? stateLens)
       $ objs

-- From http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/
colours :: [AlphaColour Double]
colours = map (opaque . sRGB24read) [
    "#5DA5DA",
    "#FAA43A",
    "#60BD68",
    "#F17CB0",
    "#B2912F",
    "#B276B2",
    "#DECF3F",
    "#4D4D4D",
    "#F15854"]

createContributionChart :: TimeZone -> Text -> [(Text,[Maybe (UTCTime,Double)])] -> Renderable ()
createContributionChart tz title vss =
 -- toFile def file $
 toRenderable $
    do
      layout_title                                  .= (unpack title)
      layout_background                             .= solidFillStyle (opaque white)
      layout_foreground                             .= (opaque black)
      -- layout_left_axis_visibility . axis_show_ticks .= False
      layout_legend . _Just . legend_orientation    .= LOCols 1
      layout_legend . _Just . legend_margin         .= 10
      layout_y_axis . laxis_title                   .= "(%)"
      layout_x_axis . laxis_title                   .= timeZoneName tz
      -- layout_x_axis . laxis_style . axis_label_gap .= 10
      -- layout_margin .= 100
      setColors colours

      forM_ vss $ \(name,vs) ->
          plot . liftEC $ do
                colour <- takeColor
                plot_lines_title .= (unpack name)
                plot_lines_values .= [ [ (utcToLocal d,v) | Just (d,v) <- vs] ]
                plot_lines_style . line_color .= colour
                plot_lines_style . line_width .= 3
    where
        utcToLocal :: UTCTime -> LocalTime
        utcToLocal lt = utcToLocalTime tz lt