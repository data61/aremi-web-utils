{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -with-rtsopts=-T #-}

module Main where

import           System.Log.Formatter                 (simpleLogFormatter)
import           System.Log.Handler                   (setFormatter)
import           System.Log.Handler.Simple
import qualified System.Log.Logger                    as HSL
import           System.Log.Logger.TH                 (deriveLoggers)

import           GHC.Conc.Sync                        (getNumProcessors,
                                                       setNumCapabilities)

-- import qualified System.Remote.Monitoring             as M

import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger (Destination (..),
                                                       IPAddrSource (..),
                                                       OutputFormat (..), RequestLoggerSettings (..),
                                                       mkRequestLogger)
import           Network.Wai.Util                      (replaceHeader)
import           Servant
import           System.IO                            (BufferMode (..),
                                                       IOMode (..),
                                                       hSetBuffering, openFile)

import           Control.Monad.Trans.Either

import           Data.Default

import Data.Configurator
import Data.Configurator.Types

import           APVI.LiveSolar

import AEMO.LivePower


$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])

type App =
    "apvi" :> APVILiveSolar
    :<|> "aemo" :> AEMOLivePower
    :<|> "static" :> Raw

appProxy :: Proxy App
appProxy = Proxy


appServer ::Config -> EitherT String IO (Server App)
appServer conf = do
    lp <- EitherT $ makeAEMOLivePowerServer (subconfig "aemo" conf) :: EitherT String IO (Server AEMOLivePower)
    ls <- EitherT $ makeLiveSolarServer (subconfig "apvi" conf) :: EitherT String IO (Server APVILiveSolar)
    return $ ls
        :<|> lp
        :<|> serveDirectory "static"
    where
        _addCorsHeader :: Middleware
        _addCorsHeader app req respond = app req (respond . replaceHeader ("Access-Control-Allow-Origin","*"))


makeMiddleware :: IO Middleware
makeMiddleware = do
    h <- openFile "access.log" AppendMode
    hSetBuffering h NoBuffering
    accessLogger <- mkRequestLogger (def {destination = Handle h
                                         ,outputFormat = Apache FromFallback
                                         ,autoFlush = True})
    return (accessLogger . simpleCors)
    -- return (logStdoutDev . simpleCors)
    -- return (simpleCors)


main :: IO ()
main = do
    -- This is the most reliable way of ensuring that the program runs using multiple threads.
    -- Since we have some background processing happening, we need this to ensure the web server
    -- remains responsive while producing new graphs.
    getNumProcessors >>= setNumCapabilities

    (config,_tid) <- autoReload (autoConfig {onError = print}) ["service.conf"]

    -- M.forkServer "localhost" 8000

    h' <- fileHandler "all.log" HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    HSL.updateGlobalLogger "APVI.LiveSolar" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    infoM "apvi-webservice launch"

    appServ <- runEitherT (appServer config)

    case appServ of
        Left err -> errorM err
        Right serv -> do
            mids <- makeMiddleware
            port <- lookupDefault 3000 config "port"
            run port $ mids $ serve appProxy serv


