{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Main where


import           Web.Scotty                                as S

import           Data.List                                 (sortBy)
import           Data.Ord                                  (comparing)

import           Control.Applicative
import           Control.Monad                             (forM, forM_)
import           Data.Monoid                               ((<>))

import           Data.Time.Calendar                        (Day, fromGregorian)
import           Data.Time.Format                          (formatTime,
                                                            parseTime)
import           Data.Time.LocalTime                       (LocalTime, TimeZone,
                                                            timeZoneName,
                                                            getCurrentTimeZone,
                                                            utcToLocalTime,
                                                            localTimeToUTC,
                                                            utc)
import           System.Locale                             (defaultTimeLocale)

import           Control.Lens                              as L
import           Data.Aeson                                (Value)
import qualified Data.Aeson                                as A
import           Data.Aeson.Lens                           as AL
import           Data.Text.Lens

-- import           Data.ByteString.Lazy                      (ByteString)
import qualified Data.ByteString.Lazy                      as BSL
import           Data.Text                                 (Text, unpack)
import qualified Data.Text                                 as T

-- import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H


import           Network.HTTP.Conduit                      (simpleHttp)

import           Control.Monad.Catch                       (catch)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

-- Chart stuff
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

states :: [Text]
states = ["nsw", "vic", "qld", "sa", "tas"]

main :: IO ()
main = do
    tz <- getCurrentTimeZone


    js <- runEitherT . fetchDate $ fromGregorian 2015 3 17
    msvgs <- case js of
        Left str -> putStrLn str >> return Nothing
        Right v -> do
            let vs = v ^.. key "contribution" . values
                allStates :: [(Text,[Maybe (LocalTime,Double)])]
                allStates = map (\name -> (name, getTS _Show name vs)) states
            (allsvg,_) <- renderableToSVGString (createContributionChart tz "All states contribution" allStates) 800 400
            ssvgs <- forM states $ \sname -> do
                    let title = T.toUpper sname <> " contribution (%)"
                    (ssvg,_) <- renderableToSVGString (createContributionChart tz title [(sname,getTS _Show sname vs)]) 800 400
                    return (sname,ssvg)

            return . Just . H.fromList $ ("all",allsvg):ssvgs

    scottyOpts def $ do
        get ( "/contribution/:state/svg") $ do
            stat <- param "state" :: ActionM Text
            case msvgs >>= H.lookup stat of
                  Nothing -> next
                  Just bs -> do
                      setHeader "Content-Type" "image/svg+xml"
                      raw bs





fetchDate :: Day -> EitherT String IO Value
fetchDate day = do
    let url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day
    liftIO $ print url

    -- bs <- liftIO (simpleHttp url)
        -- `catch` (\e -> left . show $ (e :: SomeException))
    bs <- liftIO $ BSL.readFile "snapshot2.json"

    hoistEither $ A.eitherDecode' bs



-- getStateContributions :: Value -> Either String [(Text,[Double])]
getKeyedSeries :: (AsValue t, Applicative f)
                => Text -> Text
                -> (Value -> f Value) -> t -> f t
getKeyedSeries dataset ky = key dataset . values . key ky


getTS :: (Read a, Show a)
      => Prism' String a -> Text -> [Value] -> [Maybe (LocalTime, a)]
getTS f state objs =
    let timeParser = parseTime defaultTimeLocale "%FT%H:%M:%SZ"
        timeLens   = key "ts" . _String . unpacked . to timeParser . _Just
        stateLens  = key state . _String . unpacked . f
    in sortBy (comparing (fmap fst)) $ Prelude.map (\v -> (,) <$> v ^? timeLens <*> v ^? stateLens) objs

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

createContributionChart :: TimeZone -> Text -> [(Text,[Maybe (LocalTime,Double)])] -> Renderable ()
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
        utcToLocal :: LocalTime -> LocalTime
        utcToLocal lt = utcToLocalTime tz (localTimeToUTC utc lt)

