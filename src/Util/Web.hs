{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Util.Web where


import           Data.IORef                 (IORef, readIORef)

import           Control.Lens
import Control.Applicative

import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8')

import           Network.HTTP.Types.Status  (status200)
import           Network.Wai                (Application, requestHeaderHost)
import           Network.Wai.Util			(bytestring)

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as H


import           Data.Aeson                 (Value)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Util.Types

import Servant.Server

import Data.Functor ((<$>))



serveCSV :: IORef a -> Getter a (Maybe (Text -> CsvBS)) -> Maybe Text -> EitherT ServantErr IO CsvBS
serveCSV ref lns mhost = do
        current <- liftIO $ readIORef ref
        case (current ^. lns) <*> mhost of
            Nothing -> error "TODO: fixme"
            Just csv -> return csv


serveSVG :: IORef a -> Getter a (HashMap Text SvgBS) -> Text -> EitherT ServantErr IO SvgBS
serveSVG ref lns stat = do
    current <- liftIO $ readIORef ref

    case H.lookup stat (current ^. lns) of
              Nothing -> error "TODO: fixme"
              Just svg -> return svg

serveJSON :: IORef a -> Getter a Value -> EitherT ServantErr IO Value
serveJSON ref lns = do
    current <- liftIO $ readIORef ref
    return (current ^. lns)
