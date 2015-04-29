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

import           APVI.LiveSolar


$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])

type App = "apvi" :> APVILiveSolar
      :<|> "static" :> Raw

appProxy :: Proxy App
appProxy = Proxy


appServer :: EitherT String IO (Server App)
appServer = do
    ls <- EitherT $ makeLiveSolarServer :: EitherT String IO (Server APVILiveSolar)
    return $ ls :<|> serveDirectory "static"
    where
        addCorsHeader :: Middleware
        addCorsHeader app req respond = app req (respond . replaceHeader ("Access-Control-Allow-Origin","*"))


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

    -- M.forkServer "localhost" 8000

    h' <- fileHandler "all.log" HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    HSL.updateGlobalLogger "APVI.LiveSolar" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    infoM "apvi-webservice launch"

    appServ <- runEitherT appServer

    case appServ of
        Left err -> errorM err
        Right serv -> do
            mids <- makeMiddleware
            run 3000 $ mids $ serve appProxy serv


