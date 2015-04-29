module Util.Charts where

import           Data.Colour.SRGB              (sRGB24read)
import           Graphics.Rendering.Chart.Easy

import           Data.Time.Clock               (UTCTime)
import           Data.Time.LocalTime           (LocalTime (..), TimeZone,
                                                utcToLocalTime)


import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Control.Monad


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


-- TODO: Remove the time specific stuff
createContributionChart
    :: TimeZone
    -> [(Text,[Maybe (UTCTime,Double)])]
    -> EC (Layout LocalTime Double) ()
    -> Renderable ()
createContributionChart tz vss settings = toRenderable $ do
    layout_background                             .= solidFillStyle (opaque white)
    layout_foreground                             .= (opaque black)
    -- layout_left_axis_visibility . axis_show_ticks .= False
    layout_legend . _Just . legend_orientation    .= LOCols 1
    layout_legend . _Just . legend_margin         .= 10
    -- layout_x_axis . laxis_style . axis_label_gap .= 10
    -- layout_margin .= 100
    setColors colours

    settings

    forM_ vss $ \(name,vs) ->
        plot . liftEC $ do
            colour <- takeColor
            plot_lines_title .= (T.unpack name)
            plot_lines_values .= [ [ (utcToLocal d,v) | Just (d,v) <- vs] ]
            plot_lines_style . line_color .= colour
            plot_lines_style . line_width .= 3
    where
        utcToLocal :: UTCTime -> LocalTime
        utcToLocal lt = utcToLocalTime tz lt
