{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -with-rtsopts=-T #-}

module Main where


import           Network.Wai.Middleware.Cors   (simpleCors)
import           Web.Scotty                    as S


import           Control.Lens                  as L

import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL

import qualified Data.HashMap.Strict           as H


import           Control.Monad.IO.Class

-- Chart stuff
import           Graphics.Rendering.Chart.Easy

import           System.Log.Formatter          (simpleLogFormatter)
import           System.Log.Handler            (setFormatter)
import           System.Log.Handler.Simple
import qualified System.Log.Logger             as HSL
import           System.Log.Logger.TH          (deriveLoggers)


import           Control.Concurrent

import           Data.IORef                    (readIORef)
import           GHC.Conc.Sync                 (getNumProcessors)

import           System.Remote.Monitoring

import           APVI.LiveSolar

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])



main :: IO ()
main = do
    -- This is the most reliable way of ensuring that the program runs using multiple threads.
    -- Since we have some background processing happening, we need this to ensure the web server
    -- remains responsive while producing new graphs.
    getNumProcessors >>= setNumCapabilities

    forkServer "localhost" 8000

    h' <- fileHandler "all.log" HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    HSL.updateGlobalLogger "APVI.LiveSolar" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    infoM "apvi-webservice launch"

    eref <- initialiseLiveSolar

    case eref of
        Left err -> errorM err
        Right ref -> do

            scottyOpts def $ do
                middleware simpleCors
                get ( "/contribution/:state/svg") $ do
                    current <- liftIO $ readIORef ref
                    stat <- param "state" :: ActionM Text
                    case H.lookup stat (current ^. contributionGraphs) of
                          Nothing -> next
                          Just (SvgBS bs) -> do
                              setHeader "Content-Type" "image/svg+xml"
                              raw bs
                get ( "/performance/:state/svg") $ do
                    current <- liftIO $ readIORef ref
                    stat <- param "state" :: ActionM Text
                    case H.lookup stat (current ^. performanceGraphs) of
                          Nothing -> next
                          Just (SvgBS bs) -> do
                              setHeader "Content-Type" "image/svg+xml"
                              raw bs
                get ("/contribution/csv") $ do
                    current <- liftIO $ readIORef ref
                    mhost <- header "Host"
                    case mhost >>= \hst -> _contributionCSV current (TL.toStrict hst) of
                        Nothing -> next
                        Just (CsvBS bs) -> do
                            setHeader "Content-Type" "text/csv"
                            raw bs
                get ("/performance/csv") $ do
                    current <- liftIO $ readIORef ref
                    mhost <- header "Host"
                    case mhost >>= \hst -> _performanceCSV current (TL.toStrict hst) of
                        Nothing -> next
                        Just (CsvBS bs) -> do
                            setHeader "Content-Type" "text/csv"
                            raw bs




