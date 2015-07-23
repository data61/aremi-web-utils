{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Util.Types where

import qualified Data.ByteString          as S
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as BSL

import           Network.HTTP.Media       ((//))
import           Servant.API.ContentTypes


newtype SvgBS = SvgBS {unSvg :: ByteString} deriving (Show)
newtype CsvBS = CsvBS {unCsv :: ByteString} deriving (Show)
newtype PngBS = PngBS {unPng :: ByteString} deriving (Show)

data CSV
data SVG
data PNG

instance Accept CSV where
	contentType _ = "text" // "csv"

instance MimeRender CSV CsvBS where
	mimeRender _ = unCsv

instance Accept SVG where
	contentType _ = "image" // "svg+xml"

instance MimeRender SVG SvgBS where
	mimeRender _ = unSvg

instance Accept PNG where
	contentType _ = "image" // "png"

instance MimeRender PNG PngBS where
	mimeRender _ = unPng


type ETag = S.ByteString

unchunkBS :: ByteString -> ByteString
unchunkBS = BSL.fromStrict . BSL.toStrict
