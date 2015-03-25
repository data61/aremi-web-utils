{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -with-rtsopts=-T #-}

module Main where


import           Web.Scotty                                as S

import           Data.Either                               (isLeft)
import           Data.List                                 (sortBy)
import           Data.Ord                                  (comparing)

import           Control.Applicative
import           Control.Monad                             (forM, forM_)
import           Data.Monoid                               ((<>))

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

import           Data.ByteString.Lazy                      (ByteString)
-- import qualified Data.ByteString.Lazy                      as BSL
import           Data.Text                                 (Text, unpack)
import qualified Data.Text                                 as T

import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H


import           Network.HTTP.Conduit                      (simpleHttp)

import           Control.Monad.Catch                       (SomeException,
                                                            catch, tryJust)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

-- Chart stuff
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

import           Data.IORef

import           System.Log.Formatter                      (simpleLogFormatter)
import           System.Log.Handler                        (setFormatter)
import qualified System.Log.Handler
import           System.Log.Handler.Simple
import qualified System.Log.Logger                         as HSL
import           System.Log.Logger.TH                      (deriveLoggers)


import           Control.Concurrent
import           Control.Retry
import           Data.IORef
import           Data.Time.Units                           hiding (Day)

import           System.Remote.Monitoring

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])

states :: [Text]
states = ["nsw", "vic", "qld", "sa", "tas","wa"]

main :: IO ()
main = do
    forkServer "localhost" 8000

    h' <- fileHandler "all.log" HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)

    now <- getZonedTime

    ref <- newIORef (now, Nothing, H.empty)

    success <- updateRef 20 ref

    case success of
        False -> errorM "Something went wrong?"
        True ->  do
            infoM "Successfully fetched today's data"
            _tid <- updateRef 10 ref `every` (5 :: Minute)

            scottyOpts def $ do
                get ( "/contribution/:state/svg") $ do
                    (_, _, hm) <- liftIO $ readIORef ref
                    stat <- param "state" :: ActionM Text
                    case H.lookup stat hm of
                          Nothing -> next
                          Just (SvgBS bs) -> do
                              setHeader "Content-Type" "image/svg+xml"
                              raw bs
                get ("contribution/:state/csv") $ do
                    next


newtype SvgBS = SvgBS ByteString
newtype CsvBS = CsvBS ByteString

updateRef :: IORef (ZonedTime, Maybe CsvBS, HashMap Text SvgBS) -> IO Bool
updateRef ref = flip catch (\e -> (warningM  . show $ (e :: SomeException)) >> return False) $ do
    now <- getZonedTime
    let day = localDay . zonedTimeToLocalTime $ now
        tz = zonedTimeZone now

    jsn <- retrying (fibonacciBackoff 100 <> limitRetries 10)
--- | Run an event every n time units. Does not guarantee that it will be run
                             (\_ e -> return (isLeft e))
                             $ fetchDate day
    let
        vs :: [Value]
        vs = jsn ^.. _Right . key "contribution" . values

        allStates :: [(Text,[Maybe (UTCTime,Double)])]
        allStates = map (\name -> (name, getTS _Show name vs)) states

        allChart :: Renderable ()
        allChart = createContributionChart tz "All states contribution" allStates

    (!allsvg,_) <- liftIO $ renderableToSVGString allChart 800 400
--- | Run an event every n time units. Does not guarantee that it will be run
--- each occurance of n time units if the action takes longer than n time to run.
--- It will run each action at the next block of n time (it can miss deadlines).
every :: TimeUnit a => IO b -> a -> IO (ThreadId,MVar ())
every act t = do
    mv <- newEmptyMVar

    ssvgs <- liftIO $ forM states $ \sname -> do
    now <- getCurrentTime
            let title = T.toUpper sname <> " contribution (%)"
    let ms = toMicroseconds t
                chart = createContributionChart tz title [(sname,getTS _Show sname vs)]
        ps = ms * 1000000
            (!ssvg,_) <- renderableToSVGString chart 800 400
        d = toEnum . fromIntegral $ ps :: NominalDiffTime
            return (sname,(SvgBS ssvg))

    liftIO . writeIORef ref  . (,,) now Nothing $! H.fromList $ ("all",(SvgBS allsvg)):ssvgs
        activations = iterate (addUTCTime d) now
    return True

--- It will run each action at the next block of n time (it can miss deadlines).
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



-- fetchDate :: Day -> EitherT String IO Value
fetchDate :: Day -> IO (Either String Value)
fetchDate day = do
    let url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day
    debugM $ "Fetching " ++  url

    ebs <- tryJust (\e -> Just . show $ (e::SomeException)) $ simpleHttp url
          -- `catch`
          -- (\e -> return . Left . show $ (e :: SomeException))
    -- bs <- liftIO $ BSL.readFile "snapshot-2015-03-13.json"

    return $ ebs >>= A.eitherDecode'



-- getStateContributions :: Value -> Either String [(Text,[Double])]
getKeyedSeries :: (AsValue t, Applicative f)
                => Text -> Text
                -> (Value -> f Value) -> t -> f t
getKeyedSeries dataset ky = key dataset . values . key ky


getTS :: (Read a, Show a)
      => Prism' String a -> Text -> [Value] -> [Maybe (UTCTime, a)]
getTS f state objs =
    let timeParser = parseTime defaultTimeLocale "%FT%H:%M:%SZ"
        timeLens   = key "ts" . _String . unpacked . to timeParser . _Just
        stateLens  = key state . _String . unpacked . f
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
      layout_title .= (unpack title)
      layout_background .= solidFillStyle (opaque white)
      layout_foreground .= (opaque black)
      layout_left_axis_visibility . axis_show_ticks .= False
      layout_legend . _Just . legend_orientation .= LOCols 1
      layout_legend . _Just . legend_margin .= 10
      layout_y_axis . laxis_title .= "(%)"
      layout_x_axis . laxis_title .= timeZoneName tz
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
        utcToLocal lt = utcToLocalTime tz lt -- (localTimeToUTC utc lt)

