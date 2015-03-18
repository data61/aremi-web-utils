{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Web.Scotty

import           Data.Default               (def)

import           Control.Applicative
import           Control.Monad              (when)
import           Data.Monoid                (mconcat)

import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Time.Format           (formatTime, parseTime)
import           Data.Time.LocalTime        (LocalTime)
import           System.Locale              (defaultTimeLocale)

import           Control.Lens               as L
import           Data.Aeson                 as A
import           Data.Aeson.Lens            as AL
import           Data.Text.Lens

import           Text.Read                  (readMaybe)

-- import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Text                  (Text)
-- import qualified Data.Text                  as T

-- import           Network.HTTP.Conduit       (simpleHttp)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

main :: IO ()
main = do


    js <- runEitherT $ fetchDate $ fromGregorian 2015 3 17
    -- print js
    case js of
        Left str -> putStrLn str
        Right v -> do
            let vs = v ^.. key "contribution" . values
            print $ (v ^.. getKeyedSeries "contribution" "nsw"
                            . _String . unpacked
                            -- . _Show
                            . to readMaybe
                            :: [Maybe Double])
            mapM_ print $ getTS "nsw" vs
                                 -- . to (read :: String -> Double)



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

    -- bs <- liftIO (simpleHttp url)
        -- `catch` (\e -> left . show $ (e :: SomeException))
    bs <- liftIO $ BSL.readFile "snapshot.json"


    hoistEither $ eitherDecode' bs



-- getStateContributions :: Value -> Either String [(Text,[Double])]
getKeyedSeries :: (AsValue t, Applicative f)
                => Text -> Text
                -> (Value -> f Value) -> t -> f t
getKeyedSeries dataset ky = key dataset . values . key ky


getTS :: Text -> [Value] -> [Maybe (LocalTime, Double)]
getTS state objs =
    let timeParser = parseTime defaultTimeLocale "%FT%H:%M:%SZ"
        timeLens   = key "ts" . _String . unpacked . to timeParser . _Just
        stateLens  = key state . _String . unpacked . _Show
    in Prelude.map (\v -> (,) <$> v ^? timeLens <*> v ^? stateLens) objs
