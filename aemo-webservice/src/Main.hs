{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -with-rtsopts=-T #-}

module Main where

import           System.Log.Formatter                      (simpleLogFormatter)
import           System.Log.Handler                        (setFormatter)
import           System.Log.Handler.Simple
import qualified System.Log.Logger                         as HSL
import           System.Log.Logger.TH                      (deriveLoggers)

import           GHC.Conc.Sync                             (getNumProcessors,
                                                            setNumCapabilities)

-- import qualified System.Remote.Monitoring             as M

import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Cors               (simpleCors)
import           Network.Wai.Middleware.Gzip               (gzip)
import           Network.Wai.Middleware.RequestLogger      (Destination (..),
                                                            IPAddrSource (..),
                                                            OutputFormat (..), RequestLoggerSettings (..),
                                                            mkRequestLogger)
import           Network.Wai.Util                          (replaceHeader)
import           Servant
import           System.IO                                 (BufferMode (..),
                                                            IOMode (..),
                                                            hSetBuffering,
                                                            openFile)


import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Trans.Either

import           Data.Default

import           Data.Configurator                         as C
import           Data.Configurator.Types

import           AEMO.LivePower

import           Graphics.Rendering.Chart.Backend.Diagrams (createEnv )
import           Graphics.Rendering.Chart.Easy             hiding (Default)
import           Util.Charts                               (loadFonts)


$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])

type App =
    "aemo" :> AEMOLivePower :<|> "static" :> Raw

appProxy :: Proxy App
appProxy = Proxy


appServer ::Config -> EitherT String IO (Server App)
appServer conf = do
    fontSelector <- liftIO $ loadFonts conf
    let !env = createEnv bitmapAlignmentFns 500 300 fontSelector
    lp <- EitherT $ makeAEMOLivePowerServer appProxy conf env
    return $ lp :<|> serveDirectory "static"
    where
        _addCorsHeader :: Middleware
        _addCorsHeader app req respond = app req (respond . replaceHeader ("Access-Control-Allow-Origin","*"))


makeMiddleware :: Config -> IO Middleware
makeMiddleware config = do
    accessLog <- C.lookupDefault "access.log" config "access-log"
    h <- openFile accessLog AppendMode
    hSetBuffering h NoBuffering
    accessLogger <- mkRequestLogger (def {destination = Handle h
                                         ,outputFormat = Apache FromFallback
                                         ,autoFlush = True})
    return (accessLogger . gzip def . simpleCors)
    -- return (logStdoutDev . simpleCors)
    -- return (simpleCors)


main :: IO ()
main = do
    -- This is the most reliable way of ensuring that the program runs using multiple threads.
    -- Since we have some background processing happening, we need this to ensure the web server
    -- remains responsive while producing new graphs.
    getNumProcessors >>= setNumCapabilities

    (config,_tid) <- autoReload (autoConfig {onError = print})
                        [ C.Required "/etc/aremi/aemo-webservice.conf"
                        ]

    -- M.forkServer "localhost" 8000
    allLog <- C.lookupDefault "all.log" config "all-log" :: IO FilePath
    h' <- fileHandler allLog HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    infoM "aemo-webservice launch"

    appServ <- runEitherT (appServer config)

    case appServ of
        Left err -> errorM err
        Right serv -> do
            mids <- makeMiddleware config
            port <- lookupDefault 3000 config "port"
            run port $ mids $ serve appProxy serv


