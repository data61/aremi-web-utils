module Util.Types where

import qualified Data.ByteString      as S
import           Data.ByteString.Lazy (ByteString)


newtype SvgBS = SvgBS {unSvg :: ByteString}
newtype CsvBS = CsvBS {unCsv :: ByteString}

type ETag = S.ByteString
