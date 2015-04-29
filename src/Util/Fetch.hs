{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Fetch (
	FetchResponse(..)
	, isErr
	,fetchFromCache
	) where



import           Network.HTTP.Conduit      (HttpException (StatusCodeException),
                                            Manager, Request (..), httpLbs,
                                            parseUrl, responseBody,
                                            responseHeaders)
import           Network.HTTP.Types.Header (RequestHeaders)
import           Network.HTTP.Types.Status (Status (..))



import           Data.Time.Clock           (UTCTime, getCurrentTime)

import           Data.Functor              ((<$>))
import           Data.Maybe                (maybeToList)

import qualified System.Log.Logger         as HSL
import           System.Log.Logger.TH      (deriveLoggers)

import           Control.Monad.Catch       (Handler (..), SomeException,
                                            catches)

import           Data.ByteString.Lazy      (ByteString)


import           Util.Types

$(deriveLoggers "HSL" [HSL.DEBUG])

data FetchResponse a
    = Err String
    | NoChange
    | NewData (Maybe ETag) UTCTime a
    deriving (Functor, Show)

isErr :: FetchResponse a -> Bool
isErr (Err _) = True
isErr _       = False

fetchFromCache :: Manager -> String -> Maybe ETag -> RequestHeaders -> IO (FetchResponse ByteString)
fetchFromCache manager url metag hdrs = do
    initReq <- parseUrl url
    let req = initReq {
            requestHeaders = hdrs ++ maybeToList ((,) "If-None-Match" <$> metag)
        }
    debugM $ "Fetching " ++  url

    debugM $ show req

    catches (do
        ts <- getCurrentTime
        rsp <- httpLbs req manager
        debugM $ show (rsp {responseBody = ()})
        return $ NewData (lookup "ETag" (responseHeaders rsp)) ts (responseBody rsp)

        )
        [
            Handler $ \e -> case e of
                -- If we get a 306 Not Modified there's nothing more to do
                StatusCodeException (Status {statusCode = 304}) _ _ ->
                    return $ NoChange
                _ ->
                    return $ Err (show e),
            Handler $ \e -> return $ Err (show (e :: SomeException))
        ]

