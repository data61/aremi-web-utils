{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Main where


import           Web.Scotty                                as S

import           Data.List                                 (sortBy)
import           Data.Ord                                  (comparing)

import           Control.Applicative
import           Control.Monad                             (forM, forM_)
import           Data.Monoid                               ((<>))

import           Data.Time.Calendar                        (Day)
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
                                                            catch)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

-- Chart stuff
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

states :: [Text]
states = ["nsw", "vic", "qld", "sa", "tas","wa"]

main :: IO ()
main = do
    now <- getZonedTime

    ref <- newIORef (now, CsvBS "", H.empty)

    success <- runEitherT $ updateRef ref

newtype SvgBS = SvgBS ByteString
newtype CsvBS = CsvBS ByteString

updateRef :: IORef (ZonedTime, CsvBS, HashMap Text SvgBS) -> EitherT String IO Bool
updateRef ref = flip catch (\e -> (left . show $ (e :: SomeException)) >> return False) $ do
    now <- liftIO $ getZonedTime
    let day = localDay . zonedTimeToLocalTime $ now
        tz = zonedTimeZone now

    jsn <- fetchDate day
    let vs = jsn ^.. key "contribution" . values

        allStates :: [(Text,[Maybe (UTCTime,Double)])]
        allStates = map (\name -> (name, getTS _Show name vs)) states
        allChart = createContributionChart tz "All states contribution" allStates

    (!allsvg,_) <- liftIO $ renderableToSVGString allChart 800 400

    ssvgs <- liftIO $ forM states $ \sname -> do
            let title = T.toUpper sname <> " contribution (%)"
                chart = createContributionChart tz title [(sname,getTS _Show sname vs)]
            (!ssvg,_) <- renderableToSVGString chart 800 400
            return (sname,(SvgBS ssvg))

    liftIO . writeIORef ref  . (,,) now (CsvBS "") $! H.fromList $ ("all",(SvgBS allsvg)):ssvgs
    return True




fetchDate :: Day -> EitherT String IO Value
fetchDate day = do
    let url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day
    liftIO $ print url

    -- bs <- liftIO (simpleHttp url)
        -- `catch` (\e -> left . show $ (e :: SomeException))
    bs <- liftIO $ BSL.readFile "snapshot-2015-03-13.json"

    hoistEither $ A.eitherDecode' bs



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

