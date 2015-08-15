{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Util.Charts where

import           Codec.Picture.Png                         (encodePng)
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Easy

import           Codec.Picture.Types
import           Diagrams.Backend.Rasterific
import           Diagrams.Core.Compile                     (renderDia)
import           Graphics.Rendering.Chart.Backend.Diagrams (DEnv, runBackendR)
#if MIN_VERSION_diagrams_lib(1,3,0)
import           Diagrams.TwoD.Size                        (mkSizeSpec2D)
#else
import           Diagrams.TwoD.Size                        (mkSizeSpec)
#endif

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T

import           Control.Monad

import           Util.Types

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



#if MIN_VERSION_diagrams_lib(1,3,0)
renderImage :: DEnv Double -> Double -> Double -> Renderable () -> IO (Image PixelRGBA8)
renderImage env w h r = do
    -- env <- # SCC "renderImage.defaultEnv" # defaultEnv bitmapAlignmentFns h w
    let (!dia,_) = runBackendR env r
        !img = renderDia Rasterific (RasterificOptions (mkSizeSpec2D (Just w) (Just h))) dia
    return img

#else

renderImage :: DEnv -> Double -> Double -> Renderable () -> IO (Image PixelRGBA8)
renderImage env w h r = do
    -- env <- # SCC "renderImage.defaultEnv" # defaultEnv bitmapAlignmentFns h w
    let (!dia,_) = runBackendR env r
        !img = renderDia Rasterific (RasterificOptions (mkSizeSpec (Just w) (Just h))) dia
    return img
#endif


renderToPng :: Image PixelRGBA8 -> PngBS
renderToPng = Tagged . unchunkBS . encodePng

wsChart
    :: (PlotValue x, PlotValue y)
    => [(Text,[(x,y)])]
    -> EC (Layout x y) ()
    -> Renderable ()
wsChart vss settings = toRenderable $ do
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
            plot_lines_values .= [ vs ]
            plot_lines_style . line_color .= colour
            plot_lines_style . line_width .= 2

