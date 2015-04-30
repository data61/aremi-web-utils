module Util.Types where

import qualified Data.ByteString      as S
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL


newtype SvgBS = SvgBS {unSvg :: ByteString} deriving (Show)
newtype CsvBS = CsvBS {unCsv :: ByteString} deriving (Show)

type ETag = S.ByteString

unchunkBS :: ByteString -> ByteString
unchunkBS = BSL.fromStrict . BSL.toStrict