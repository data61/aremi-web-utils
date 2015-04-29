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


import           Data.List                                 (sortBy)
import           Data.Monoid                               ((<>))
import           Data.Ord                                  (comparing)

import           Control.Applicative
import           Control.Arrow                             (second)

import Data.Default (Default(..))

import           Data.ByteString                           ()
import qualified Data.ByteString                           as S
import           Data.ByteString.Lazy                      (ByteString)
import qualified Data.ByteString.Lazy                      as BSL

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T

import           Data.Text.Encoding                        (encodeUtf8)


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
-- import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy hiding (Default)

-- import           Graphics.Rendering.Chart.Easy

import           Data.Time.Clock                           (UTCTime)
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

import           Control.Monad.Catch                       (SomeException,
                                                            catch)
import           Control.Monad.IO.Class

import           Network.HTTP.Conduit                      (withManager)

import           Control.Concurrent.Async                  (async,
                                                            mapConcurrently,
                                                            wait)
import           Control.Retry                             (fibonacciBackoff,
                                                            limitRetries,
                                                            retrying)
import           Data.IORef                                (newIORef)
import           Data.Time.Units                           hiding (Day)

import           Servant
-- import           Servant.Docs
--
import Util.Charts
import Util.Web
import Util.Types
import Util.Periodic
import Util.Fetch


$(deriveLoggers "HSL" [HSL.DEBUG, HSL.ERROR, HSL.WARNING])



data AppState = AppState {
      _timeFetched           :: !(Maybe ZonedTime)
    , _latestETag            :: !(Maybe ETag)
    , _contributionCSV       :: Maybe (Text -> CsvBS)
    , _contributionGraphs    :: !(HashMap Text SvgBS)
    , _performanceCSV        :: Maybe (Text -> CsvBS)
    , _performanceGraphs     :: !(HashMap Text SvgBS)
    , _performanceGraphJSON  :: Value
    , _contributionGraphJSON :: Value
}

$(makeLenses ''AppState)


instance Default AppState where
    def = AppState {
        _timeFetched        = Nothing,
        _latestETag         = Nothing,
        _contributionCSV    = Nothing,
        _contributionGraphs = H.empty,
        _performanceCSV     = Nothing,
        _performanceGraphs  = H.empty,
        _performanceGraphJSON = A.Array empty,
        _contributionGraphJSON = A.Array empty
    }

states :: [(Text, Int)]
states = [
    ("nsw",1),
    ("vic",2),
    ("qld",3),
    ("sa",4),
    ("wa",5),
    ("tas",6),
    ("nt",7)
    ]

type APVILiveSolar =
    "performance" :>
        ("csv" :> Raw
        :<|> Capture "svgstate" Text :> "svg" :> Raw
        :<|> "json" :> Get Value)
    :<|>
    "contribution" :>
        ("csv" :> Raw
        :<|> Capture "svgstate" Text :> "svg" :> Raw
        :<|> "json" :> Get Value)


-- instance ToCapture (Capture "svgstate" Text) where
--     toCapture _ = DocCapture "svgstate" $
--         "Australian State name, currently supported are: all, "
--         ++ intercalate ", " (map (T.unpack . fst) states)

makeLiveSolarServer :: IO (Either String (Server APVILiveSolar))
makeLiveSolarServer = do
    eref <- initialiseLiveSolar
    case eref of
        Left str -> return $ Left str
        Right ref -> return $ Right (
            (serveCSV ref performanceCSV
                :<|> serveSVG ref performanceGraphs
                :<|> serveJSON ref performanceGraphJSON)
            :<|> (serveCSV ref contributionCSV
                :<|> serveSVG ref contributionGraphs
                :<|> serveJSON ref contributionGraphJSON)
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


-- Takes a number of retries and the current app state ref and attempts to contact APVI for the latest
-- data for today.
updateRef :: Int -> IORef AppState -> IO Bool
updateRef retries ref = flip catch (\e -> (warningM  . show $ (e :: SomeException)) >> return False) $ do
    now <- getZonedTime
    let day = localDay . zonedTimeToLocalTime $ now
        tz = zonedTimeZone now
        url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day

    current <- readIORef ref
    ejsn <- withManager $ \m ->
            liftIO $ retrying (fibonacciBackoff 1000 <> limitRetries retries)
                     (\_ e -> return (isErr e))
                     $ fetchFromCache m url (_latestETag current) [
                            ("Accept"          , "application/json, text/javascript, */*"),
                            ("X-Requested-With", "XMLHttpRequest")
                        ]
    case ejsn of
        Err err -> errorM err >> return False
        NoChange -> debugM "update not necessary" >> return True
        NewData metag fetched bs -> do
            case A.eitherDecode' bs of
                Left err -> errorM err >> return False
                Right jsn -> do
                    allPerfSvgs' <- async $
                        renderCharts (fetched, jsn) tz "performance"
                                     _Double
                                     $ "ts" : map fst states
                    allContSvgs' <- async $
                        renderCharts (fetched, jsn) tz "contribution"
                                     (_String . unpacked . _Show)
                                     $ ("ts":)
                                        . filter (\s -> s /="wa" && s /= "nt")
                                        . map fst
                                        $ states

                    (allPerfSvgs, perfJSON, perfCSV) <- wait allPerfSvgs'
                    (allContSvgs, contJSON, contCSV) <- wait allContSvgs'

                    let svgSize = foldl (\n (_,svg) -> n + BSL.length (unSvg svg))
                                        0
                                        (allContSvgs ++ allPerfSvgs)

                    debugM $ "Total SVG size: " ++ show svgSize

                    let !newState = current
                            & contributionGraphs    .~ H.fromList allContSvgs
                            & performanceGraphs     .~ H.fromList allPerfSvgs
                            & performanceCSV        .~ Just perfCSV
                            & contributionCSV       .~ Just contCSV
                            & performanceGraphJSON  .~ perfJSON
                            & contributionGraphJSON .~ contJSON
                            & timeFetched           .~ Just now
                            & latestETag            .~ (metag <|> current ^. latestETag)

                    liftIO . writeIORef ref $ newState
                    -- performMajorGC
                    return True
    where
        renderCharts :: (UTCTime,Value) -> TimeZone -> Text
                     -> Prism' Value Double
                     -> [Text]
                     -> IO ([(Text,SvgBS)], Value, Text -> CsvBS)
        renderCharts (fetched,jsn) tz title lns cols = do
            let
                vals :: [Value]
                vals = jsn ^.. key title . values

                allStates :: [(Text,[Maybe (UTCTime,Double)])]
                allStates = map (\(name,_) -> (name, getTS lns name vals)) states

                allCorrected :: [(Text,[(LocalTime,Double)])]
                allCorrected = [(name,[(utcToLocalTime tz utct,d)
                                      | Just (utct,d) <- series])
                               | (name,series) <- allStates]

                titleStr :: String
                titleStr = "All states " ++ T.unpack title

                allTitle :: Text
                allTitle = T.pack $ formatTime defaultTimeLocale (titleStr ++ " (%%) %F %X %Z") fetched

                -- allChart :: Renderable ()
                allChart = wsChart allCorrected $ do
                                layout_title .= (T.unpack allTitle)
                                layout_y_axis . laxis_title .= "(%)"
                                layout_x_axis . laxis_title .= timeZoneName tz


            debugM $ "Rendering " ++ titleStr ++ " SVGs"
            (allsvg',_) <- liftIO $ renderableToSVGString allChart 500 300
            let !allsvg = SvgBS $ unchunkBS allsvg'

            ssvgs <- liftIO $ flip mapConcurrently states $ \(sname,_) -> do
                    let fullTitle = T.toUpper sname <> " " <> title <> " (%)"

                        plotVals = [(utcToLocalTime tz utct,d)
                                   | Just (utct,d) <- getTS lns sname vals]

                        chart = wsChart [(sname,plotVals)] $ do
                                    layout_title .= (T.unpack fullTitle)
                                    layout_y_axis . laxis_title .= "(%)"
                                    layout_x_axis . laxis_title .= timeZoneName tz

                    (ssvg',_) <- renderableToSVGString chart 500 300
                    let !ssvg = SvgBS $ unchunkBS ssvg'
                    return (sname, ssvg)
            debugM $ "Done rendering " ++ titleStr ++ " SVGs"



            -- Produce json values for google chart
            let jsonData2 = A.toJSON $
                    ((Just "ts") : [val ^? key "ts" | val <- vals]) :
                    [Just (A.toJSON col) : [ val ^? key col | val <- vals] | col <- drop 1 cols]

            return $ (("all",allsvg):ssvgs, jsonData2, renderCSVs title allStates)

        renderCSVs :: Text -> [(Text,[Maybe (UTCTime,Double)])] -> (Text -> CsvBS)
        renderCSVs title allStates =
            let csvHeader :: Csv.Header
                csvHeader = V.fromList [encodeUtf8 title,"State", "State name","Time","Image"] :: V.Vector S.ByteString

                currentValues :: [(Text,Maybe (UTCTime, Double))]
                currentValues = map (second maximum) allStates

                namedRecords hst =
                    map (\(state, mtv)
                        -> H.fromList [
                                ("State", toField $ lookup state states)
                                ,("State name", toField state)
                                ,("Time", toField $ maybe "-" (formatTime defaultTimeLocale "%FT%X") (fst <$> mtv))
                                ,(encodeUtf8 title, toField $ maybe 0.0 id (snd <$> mtv))
                                ,("Image", toField $ T.concat ["<img src='http://",hst,"/apvi/"
                                                              ,title,"/",state,"/svg'/>"])
                            ]
                        )
                        currentValues
                csvf hst = CsvBS $ encodeByNameWith defaultEncodeOptions csvHeader (namedRecords hst)
            in csvf

unchunkBS :: ByteString -> ByteString
unchunkBS = BSL.fromStrict . BSL.toStrict


getTS :: Prism' Value a -> Text -> [Value] -> [Maybe (UTCTime, a)]
getTS f state objs =
    let timeParser = parseTime defaultTimeLocale "%FT%H:%M:%SZ"
        timeLens   = key "ts" . _String . unpacked . to timeParser . _Just
        stateLens  = key state . f
    in sortBy (comparing (fmap fst))
       . Prelude.map (\v -> (,) <$> v ^? timeLens <*> v ^? stateLens)
       $ objs

