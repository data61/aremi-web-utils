{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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

import           Diagrams.TwoD.Size                        (mkSizeSpec2D)

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T

import qualified Graphics.SVGFonts.ReadFont                as F

import           Control.Concurrent.Async (mapConcurrently)

import           Control.Monad

import           Util.Types

import           Configuration.Utils hiding ((.=))
import qualified Configuration.Utils as C
-- import Control.Lens

newtype FontConfig = FontConfig {_fontDir :: String} deriving (Show, Eq)
$(makeLenses ''FontConfig)

instance FromJSON (FontConfig -> FontConfig) where
  parseJSON = withObject "FontConfig" $ \o -> id <$< fontDir ..: "fontdir" % o

instance ToJSON FontConfig where
  toJSON a = object
      [ "fontdir" C..= _fontDir a]

pFontConfig :: MParser FontConfig
pFontConfig = id
    <$< fontDir .:: strOption
        % short 'F'
        <> long "fontsdir"
        <> help "Directory containing SVG fonts for"

defaultFontConfig :: FontConfig
defaultFontConfig = FontConfig "./fonts"



-- From http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/
colours :: [AlphaColour Double]
colours = map (opaque . sRGB24read) [
    "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0",
    "#B2912F", "#B276B2", "#DECF3F", "#4D4D4D",
    "#F15854"]



renderImage :: DEnv Double -> Double -> Double -> Renderable () -> IO (Image PixelRGBA8)
renderImage env w h r = do
    -- env <- # SCC "renderImage.defaultEnv" # defaultEnv bitmapAlignmentFns h w
    let (!dia,_) = runBackendR env r
        !img = renderDia Rasterific (RasterificOptions (mkSizeSpec2D (Just w) (Just h))) dia
    return img

type ChartEnv = DEnv Double

isFontFamily :: String -> DFont n -> Bool
isFontFamily n (fd, _) = n == F.fontDataFamily fd

alterFontFamily :: String -> DFont n -> DFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

loadFonts :: FontConfig -> IO (FontSelector Double)
loadFonts fconf = do
    [
       sansR
     , sansRB
     , sansRBI
     , sansRI
     ] <- mapConcurrently (F.loadFont . (_fontDir fconf ++))
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

        _ | any (null . snd) vss
                -> error $ "empty chart series for: " ++ show (map fst . filter (null . snd) $ vss)
          | any (\(_,_:xs) -> null xs) vss
                -> error $ "cannot make chart of a single data point for: " ++ show (map fst . filter (\(_,_:xs) -> null xs) $ vss)
          | otherwise -> toRenderable $ do
            layout_background                             .= solidFillStyle (opaque white)
            layout_foreground                             .= opaque black
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
                    plot_lines_title .= T.unpack name
                    plot_lines_values .= [ vs ]
                    plot_lines_style . line_color .= colour
                    plot_lines_style . line_width .= 2
