{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    , ISOUtcTime(..)
    ) where

import qualified Data.ByteString          as S
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as BSL

import           Data.Text                as T

import           Network.HTTP.Media       ((//))
import           Servant.API.ContentTypes
import           Servant.Common.Text

import           Data.Time.Clock          (UTCTime)
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
#else
import           Data.Time.Format         (parseTime)
import           System.Locale            (defaultTimeLocale)
#endif

import           Data.Tagged


-- | `untag` - Useful when you have a lazy ByteString which you know the
-- content type of, such as from some kind of precomputed value,
-- i.e. images etc.
instance Accept k => MimeRender k (Tagged k ByteString) where
  mimeRender _ = untag


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

newtype ISOUtcTime = ISOUtcTime {unISOUtc :: UTCTime} deriving Show

instance FromText ISOUtcTime where
    fromText =
        let
            formatStr :: String
            formatStr = "%FT%T%z"
            timeParser :: String -> Maybe UTCTime
#if MIN_VERSION_time(1,5,0)
            timeParser = parseTimeM True defaultTimeLocale formatStr
#else
            timeParser = parseTime defaultTimeLocale formatStr
#endif
        in fmap ISOUtcTime . timeParser . T.unpack
