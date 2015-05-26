{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Util.Web where


import           Data.IORef                 (IORef, readIORef)

import           Control.Applicative
import           Control.Lens

import           Data.Text                  (Text)

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as H


import           Data.Aeson                 (Value)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Util.Types

import           Servant.Server

import qualified Data.ByteString.Lazy.Char8 as LC8



serveCSV :: IORef a -> Getter a (Maybe (Text -> CsvBS)) -> Maybe Text -> EitherT ServantErr IO CsvBS
serveCSV ref lns mhost = do
        current <- liftIO $ readIORef ref
        case (current ^. lns) <*> mhost of
            Nothing -> left err404 {errBody = LC8.pack ("not found")}
            Just csv -> return csv


serveSVG :: IORef a -> Getter a (HashMap Text SvgBS) -> Text -> EitherT ServantErr IO SvgBS
serveSVG ref lns stat = do
    current <- liftIO $ readIORef ref

    case H.lookup stat (current ^. lns) of
              Nothing -> left err404 {errBody = LC8.pack ("Cound not find SVG for " ++ show stat)}
              Just svg -> return svg

serveJSON :: IORef a -> Getter a Value -> EitherT ServantErr IO Value
serveJSON ref lns = do
    current <- liftIO $ readIORef ref
    return (current ^. lns)
