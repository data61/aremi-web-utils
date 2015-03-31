{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -with-rtsopts=-T #-}

module Main where


import           Network.Wai.Middleware.Cors               (simpleCors)
import           Web.Scotty                                as S

import           Data.List                                 (sortBy)
import           Data.Maybe                                (maybeToList)
import           Data.Monoid                               ((<>))
import           Data.Ord                                  (comparing)

import           Control.Applicative
import           Control.Arrow                             (second)
import           Control.Monad                             (forM_)

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

import           Control.Lens                              as L
import           Data.Aeson                                (Value)
import qualified Data.Aeson                                as A
import           Data.Aeson.Lens                           as AL
import           Data.Text.Lens

import           Data.ByteString                           ()
import qualified Data.ByteString                           as S
import           Data.ByteString.Lazy                      (ByteString)
import qualified Data.ByteString.Lazy                      as BSL

import           Data.Text                                 (Text, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text                                 as T
import           Data.Text.Encoding                        (encodeUtf8)

import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H


import           Network.HTTP.Conduit                      (HttpException (StatusCodeException),
                                                            Manager,
                                                            Request (..),
                                                            httpLbs, parseUrl,
                                                            responseBody,
                                                            responseHeaders,
                                                            withManager)
import           Network.HTTP.Types.Status                 (Status (..))

import           Control.Monad.Catch                       (Handler (..),
                                                            SomeException,
                                                            catch, catches)
import           Control.Monad.IO.Class

-- Chart stuff
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy

import           Data.Csv                                  (defaultEncodeOptions,
                                                            encodeByNameWith,
                                                            toField)
import qualified Data.Csv                                  as Csv
-- import           Data.Vector                               (Vector)
import qualified Data.Vector                               as V




import           System.Log.Formatter                      (simpleLogFormatter)
import           System.Log.Handler                        (setFormatter)
import           System.Log.Handler.Simple
import qualified System.Log.Logger                         as HSL
import           System.Log.Logger.TH                      (deriveLoggers)


import           Control.Concurrent
import           Control.Concurrent.Async                  (async,
                                                            mapConcurrently,
                                                            wait)
import           Control.Retry                             (fibonacciBackoff,
                                                            limitRetries,
                                                            retrying)
import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)
import           Data.Time.Units                           hiding (Day)
import           GHC.Conc.Sync                             (getNumProcessors)

import           System.Remote.Monitoring

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])

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

main :: IO ()
main = do
    -- This is the most reliable way of ensuring that the program runs using multiple threads.
    -- Since we have some background processing happening, we need this to ensure the web server
    -- remains responsive while producing new graphs.
    getNumProcessors >>= setNumCapabilities

    forkServer "localhost" 8000

    h' <- fileHandler "all.log" HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    infoM "apvi-webservice launch"

    ref <- newIORef def

    success <- updateRef 20 ref

    if not success then errorM "Could not perform initial update"
    else do
        infoM "Successfully fetched today's data"
        _tid <- updateRef 10 ref `every` (5 :: Minute)

        scottyOpts def $ do
            middleware simpleCors
            get ( "/contribution/:state/svg") $ do
                current <- liftIO $ readIORef ref
                stat <- param "state" :: ActionM Text
                case H.lookup stat (current ^. contributionGraphs) of
                      Nothing -> next
                      Just (SvgBS bs) -> do
                          setHeader "Content-Type" "image/svg+xml"
                          raw bs
            get ( "/performance/:state/svg") $ do
                current <- liftIO $ readIORef ref
                stat <- param "state" :: ActionM Text
                case H.lookup stat (current ^. performanceGraphs) of
                      Nothing -> next
                      Just (SvgBS bs) -> do
                          setHeader "Content-Type" "image/svg+xml"
                          raw bs
            get ("/contribution/csv") $ do
                current <- liftIO $ readIORef ref
                mhost <- header "Host"
                liftIO $ print mhost
                case mhost >>= \hst -> _contributionCSV current (TL.toStrict hst) of
                    Nothing -> next
                    Just (CsvBS bs) -> do
                        setHeader "Content-Type" "text/csv"
                        raw bs
            get ("/performance/csv") $ do
                current <- liftIO $ readIORef ref
                mhost <- header "Host"
                case mhost >>= \hst -> _performanceCSV current (TL.toStrict hst) of
                    Nothing -> next
                    Just (CsvBS bs) -> do
                        setHeader "Content-Type" "text/csv"
                        raw bs

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

