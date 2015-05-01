{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell #-}

module AEMO.LivePower where

import           Control.Applicative
import Control.Lens hiding ((.=))

import           Data.Text            (Text)
-- import qualified Data.Text as T
--
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B


import           Data.Maybe           (catMaybes)


import           Data.IORef           (IORef, readIORef, writeIORef, newIORef)

import           Data.Csv

import           Control.Exception    (throw)

import           Database.Persist
-- import Database.Persist.Class

import           Servant

import           AEMO.Database
import           AEMO.Types
-- import           System.Log.FastLogger
import           Control.Monad.Logger (LogLevel (..))

import Data.Default

import           Util.Types
import           Util.Web
import           Util.Periodic

type AEMOLivePower =
    "csv" :> Raw
    -- :<|>
    -- Capture "DUID" Text :> "svg" :> Raw




data ALPState = ALPS {
    _powerStationLocs :: Maybe (Text -> CsvBS)
}

$(makeLenses ''ALPState)

instance Default ALPState where
    def = ALPS {
        _powerStationLocs = Nothing
        }


makeAEMOLivePowerServer :: IO (Either String (Server AEMOLivePower))
makeAEMOLivePowerServer = do
    ref <- newIORef def

    success <- updateALPState ref
    if success
        then do
            _tid <- updateALPState ref `every` (5 :: Minute)
            return . Right $ (serveCSV ref powerStationLocs)
        else return . Left $ "The impossible happened!"

updateALPState :: IORef ALPState -> IO Bool
updateALPState ref = do
    current <- readIORef ref
    (locs,pows,dats) <- getLocs
    let csvf = makeCsv locs pows dats
    writeIORef ref $ current
        & powerStationLocs .~ Just csvf
    return True

getLocs :: IO ([Entity DuidLocation],[Entity PowerStation],[Entity PowerStationDatum])
getLocs = do
    r <- runApp dbConn 1 LevelDebug $ do
        runDB $ do
            locs <- selectList [] []
            pows <- selectList [] []
            mrecent <- selectFirst [] [Desc PowerStationDatumSampleTime]
            case mrecent of
                Nothing -> error "Database has no PowerStationData"
                Just epsd -> do
                    dats <- selectList [PowerStationDatumSampleTime
                                        ==. powerStationDatumSampleTime (entityVal epsd)] []
                    return (locs, pows, dats)
    case r of
        Left ex -> throw ex
        Right ls -> return ls

makeCsv :: [Entity DuidLocation]
        -> [Entity PowerStation]
        -> [Entity PowerStationDatum]
        -> (Text -> CsvBS)
makeCsv locs pows dats = let
    toLocRec :: DuidLocation -> NamedRecord
    toLocRec dloc = namedRecord
        [ "DUID"    .= duidLocationDuid dloc
        , "Lat"     .= duidLocationLat dloc
        , "Lon"     .= duidLocationLon dloc
        , "comment" .= duidLocationComment dloc
        ]

    powsByDuid :: HashMap Text NamedRecord
    powsByDuid =
        H.fromList
        . map (\ps -> (powerStationDuid ps, toNamedRecord ps))
        . map entityVal
        $ pows

    latestByDuid :: HashMap Text NamedRecord
    latestByDuid =
        H.fromList
        . map (\psd -> (powerStationDatumDuid psd
                       , replaceKey "MW" "Most Recent Output (MW)" $ toNamedRecord psd))
        . map entityVal
        $ dats

    displayCols =
        [ "Lat"
        , "Lon"
        , "Most Recent Output (MW)"
        , "DUID"
        , "Participant"
        , "Station Name"
        , "Dispatch Type"
        , "Category"
        , "Classification"
        , "Fuel Source - Primary"
        , "Fuel Source - Descriptor"
        , "Technology Type - Primary"
        , "Technology Type - Descriptor"
        , "Aggregation"

        , "Sample Time UTC"
        , "Max Cap (MW)"
        , "Reg Cap (MW)"
        , "Max ROC/Min"
        , "Unit Size (MW)"
        , "Physical Unit No."
        , "comment"
        ]

    replaceKey :: ByteString -> ByteString -> HashMap ByteString a -> HashMap ByteString a
    replaceKey from to mp = case H.lookup from mp of
        Nothing -> mp
        Just v  -> H.insert to v (H.delete from mp)

    empty :: Text
    empty = "-"

    emptyDatum :: NamedRecord
    emptyDatum = namedRecord
        [ "Most Recent Output (MW)" .= empty
        , "Sample Time UTC" .= empty
        ]

    !csv = unchunkBS $
        encodeByName displayCols
        . catMaybes
        . map (\loc -> do
            let duid = (duidLocationDuid loc)
            ps <- H.lookup duid powsByDuid
            return $ H.unions [toLocRec loc
                              , ps
                              , maybe emptyDatum id (H.lookup duid latestByDuid)
                              ]
            )
        . map entityVal
        $ locs


    in \_host -> CsvBS $! csv
