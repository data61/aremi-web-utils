{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Charts where

import           Codec.Picture.Png                         (encodePng)
import           Data.Colour.SRGB                          (sRGB24read)
import           Graphics.Rendering.Chart.Easy

import           Codec.Picture.Types
import           Diagrams.Backend.Rasterific
import           Diagrams.Core.Compile                     (renderDia)
import           Graphics.Rendering.Chart.Backend.Diagrams (DEnv, DFont,
                                                            FontSelector,
                                                            runBackendR)

#if MIN_VERSION_diagrams_lib(1,3,0)
import           Diagrams.TwoD.Size                        (mkSizeSpec2D)
#else
import           Diagrams.TwoD.Size                        (mkSizeSpec)
#endif

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T

import qualified Data.Configurator                         as C
import           Data.Configurator.Types                   (Config)

import qualified Graphics.SVGFonts.CharReference           as F
import qualified Graphics.SVGFonts.ReadFont                as F

import           Control.Concurrent.Async (mapConcurrently)

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



#if MIN_VERSION_diagrams_lib(1,3,0)
type ChartEnv = DEnv Double
#else
type ChartEnv = DEnv
#endif

isFontFamily :: String -> DFont n -> Bool
isFontFamily n (fd, _) = n == F.fontDataFamily fd

alterFontFamily :: String -> DFont n -> DFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

loadFonts :: Config -> IO (FontSelector Double)
loadFonts conf = do
    fontPath <- C.lookupDefault "fonts" conf "font-path"

    [
       sansR
     , sansRB
     , sansRBI
     , sansRI
     ] <- mapConcurrently (F.loadFont . (fontPath ++))
                    [ "/SourceSansPro_R.svg"
                    , "/SourceSansPro_RB.svg"
                    , "/SourceSansPro_RBI.svg"
                    , "/SourceSansPro_RI.svg"
                    ]
    putStrLn "loaded fonts"

    let selectFont :: FontStyle -> F.PreparedFont Double
        selectFont fs = alterFontFamily "sans-serif" $ case (_font_name fs, _font_slant fs, _font_weight fs) of
          (_, FontSlantNormal , FontWeightNormal) -> sansR
          (_, FontSlantNormal , FontWeightBold  ) -> sansRB
          (_, FontSlantItalic , FontWeightNormal) -> sansRI
          (_, FontSlantOblique, FontWeightNormal) -> sansRI
          (_, FontSlantItalic , FontWeightBold  ) -> sansRBI
          (_, FontSlantOblique, FontWeightBold  ) -> sansRBI


    return selectFont


renderToPng :: Image PixelRGBA8 -> PngBS
renderToPng = Tagged . unchunkBS . encodePng

wsChart
    :: (PlotValue x, PlotValue y)
    => [(Text,[(x,y)])]
    -> EC (Layout x y) ()
    -> Renderable ()
wsChart vss settings =

    case vss of
        [] -> error "empty chart data"

        _ | any (null . snd) vss -> error $ "empty chart series for: " ++ show (map fst . filter (null . snd) $ vss)
          | otherwise -> toRenderable $ do
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

