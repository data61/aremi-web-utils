{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , TimeOffsets(..)
    , TimeOffset(..)
    , TimePeriod(..)
    , modifyTime
    , APIDoc(..)
    , docsHtml
    ) where

import qualified Data.ByteString          as S
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as BSL

import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL

import           Network.HTTP.Media       ((//))
import           Servant.API.ContentTypes
import           Servant.Common.Text
import           Servant.Docs

import           Text.Blaze.Html                           (Html)
import qualified Text.Blaze.Html5                          as H
import           Text.Blaze.Html5.Attributes               (charset, name, content, rel, href)

import qualified Text.Markdown                             as MD


import Data.Proxy

import Data.Default

import           Data.Time.Clock          (UTCTime)
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
#else
import           Data.Time.Format         (parseTime)
import           System.Locale            (defaultTimeLocale)
#endif

import           Data.Tagged

import           Control.Applicative
import           Data.Attoparsec.Text     hiding (D)


import           Control.Lens
import           Data.Time.Lens
import           Numeric.Lens             (integral)

import           Data.Maybe               (fromJust)

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


data TimePeriod = Y | M | D | H | Mn deriving (Show,Eq)

data TimeOffset = TimeOffset Int TimePeriod  deriving Show

newtype TimeOffsets = TimeOffsets [TimeOffset] deriving Show

timePeriodMap :: [(TimePeriod,Char)]
timePeriodMap =
    [(Y,'Y')
    ,(M,'M')
    ,(D,'D')
    ,(H,'h')
    ,(Mn,'m')
    ]

instance ToText TimeOffsets where
    toText (TimeOffsets ts) = T.pack
        $ concatMap (\(TimeOffset n o)
                    -> show n ++ fromJust (lookup o timePeriodMap):[]
                    ) ts

instance FromText TimeOffsets where
    fromText t = parseTimeOffsets t

parseTimeOffsets :: T.Text -> Maybe TimeOffsets
parseTimeOffsets t = either (const Nothing) Just . flip parseOnly t $
    TimeOffsets <$> (
        many1 parsePair <* endOfInput
        <|> pure []
        )

    where
        parsePair = TimeOffset <$> decimal <*> timePeriod

        timePeriod = foldr (\(o,c) r -> (char c *> pure o) <|> r)
                           (fail "expected one of \"ymdh\"")
                           timePeriodMap

modifyTime :: FlexibleDateTime t => TimeOffsets -> t -> t
modifyTime (TimeOffsets ts) t = t & flexDT %~ adjust where
    adjust = foldr (\(TimeOffset n o) r -> (fromOff o -~ n) . r) id ts
    fromOff o = case o of
        Y -> years . integral
        M -> months
        D -> days
        H -> hours
        Mn -> minutes

-- API Documentation utilities


newtype APIDoc = APIDoc Html deriving (H.ToMarkup)

instance ToSample APIDoc APIDoc where
    toSample _ = Just (APIDoc "(This page)")


docsHtml :: HasDocs api => T.Text -> Proxy api -> APIDoc
docsHtml title api = APIDoc $ do
    H.docTypeHtml $ do
        H.head $ do
            H.meta H.! charset "utf-8"
            H.meta H.! name "viewport" H.! content "width=device-width, initial-scale=1.0"
            H.link H.! rel "stylesheet" H.! href "//writ.cmcenroe.me/1.0.2/writ.min.css"
        H.head $
            H.main $ do
                H.h1 (H.text title) --
                MD.markdown def $ TL.pack $ markdown $ docs api
