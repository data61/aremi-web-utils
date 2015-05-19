{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Types where

import qualified Data.ByteString      as S
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Servant.API.ContentTypes
import Network.HTTP.Media ((//))

data CSV
data SVG

instance Accept CSV where
	contentType _ = "text" // "csv"

instance MimeRender CSV CsvBS where
	mimeRender _ = unCsv

instance Accept SVG where
	contentType _ = "image" // "svg+xml"

instance MimeRender SVG SvgBS where
	mimeRender _ = unSvg


newtype SvgBS = SvgBS {unSvg :: ByteString} deriving (Show)
newtype CsvBS = CsvBS {unCsv :: ByteString} deriving (Show)

type ETag = S.ByteString

unchunkBS :: ByteString -> ByteString
unchunkBS = BSL.fromStrict . BSL.toStrict