{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Web.Scotty

import           Data.Default               (def)

import           Control.Applicative        (Applicative)
import           Control.Monad              (when)
import           Data.Monoid                (mconcat)

import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Time.Format           (formatTime)
import           System.Locale              (defaultTimeLocale)

import           Control.Lens               as L
import           Data.Aeson                 as A
import           Data.Aeson.Lens            as AL
import           Data.Text.Lens

import           Data.Text                  as T

import           Network.HTTP.Conduit       (simpleHttp)

import           Control.Exception          (SomeException)
import           Control.Monad.Catch        (catch)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

main :: IO ()
main = do


    js <- runEitherT $ fetchDate $ fromGregorian 2015 3 17
    -- print js
    case js of
        Left str -> putStrLn str
        Right v -> print $ v ^.. getKeyedSeries "contribution" "nsw"
                                 . _String . unpacked . to (read :: String -> Double)



    when False $ scottyOpts (def {verbose = 1}) $ do
      get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
      get "/state/:state" $ do
        state <- param "state"
        html $ mconcat ["<h1>State: ", state, "</h1>"]



fetchDate :: Day -> EitherT String IO Value
fetchDate day = do
    let url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day
    liftIO $ print url

    bs <- liftIO (simpleHttp url)
        `catch` (\e -> left . show $ (e :: SomeException))


    hoistEither $ eitherDecode' bs



-- getStateContributions :: Value -> Either String [(Text,[Double])]
getKeyedSeries :: (AsValue t, Applicative f)
                => Text -> Text
                -> (Value -> f Value) -> t -> f t
getKeyedSeries dataset ky = key dataset . values . key ky
