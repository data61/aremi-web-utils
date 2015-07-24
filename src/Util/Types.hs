{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Util.Types
	( SvgBS
    , CsvBS
    , PngBS
    , CSV
    , SVG
    , PNG
    , ETag
    , unchunkBS
    , Tagged(..)
    , untag
	) where

import qualified Data.ByteString          as S
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as BSL

import           Network.HTTP.Media       ((//))
import           Servant.API.ContentTypes



type SvgBS = Tagged SVG ByteString
type CsvBS = Tagged CSV ByteString
type PngBS = Tagged PNG ByteString

data CSV
data SVG
data PNG


instance Accept CSV where
	contentType _ = "text" // "csv"

instance Accept SVG where
	contentType _ = "image" // "svg+xml"

instance Accept PNG where
	contentType _ = "image" // "png"


type ETag = S.ByteString

unchunkBS :: ByteString -> ByteString
unchunkBS = BSL.fromStrict . BSL.toStrict
