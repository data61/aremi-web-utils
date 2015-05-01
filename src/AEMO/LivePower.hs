{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module AEMO.LivePower where

-- import           Control.Applicative
import           Control.Lens

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
--
import           Data.Hashable
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H

-- import           Data.ByteString      (ByteString)
-- import qualified Data.ByteString      as B


import           Data.Maybe                                (catMaybes)

import           Data.Monoid

import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)

import           Data.Time.Clock                           (UTCTime)

import           Data.Time.LocalTime

import           Control.Monad.IO.Class

import           Data.Csv                                  (NamedRecord,
                                                            encodeByName,
                                                            namedRecord,
                                                            toNamedRecord)
import qualified Data.Csv                                  as C

import           Control.Exception                         (throw)

import           Database.Persist
-- import Database.Persist.Class

import           Servant

import           AEMO.Database
import           AEMO.Types
-- import           System.Log.FastLogger
import           Control.Monad.Logger                      (LogLevel (..))

import           Data.Default

import           Util.Periodic
import           Util.Types
import           Util.Web

import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy
import           Util.Charts

import           Network.HTTP.Types.Status                 (status200)
import           Network.Wai                               (Application)
import           Network.Wai.Util                          (bytestring)




type AEMOLivePower =
    "csv" :> Raw
    :<|>
    Capture "DUID" Text :> "svg" :> Raw




data ALPState = ALPS
    { _powerStationLocs :: Maybe (Text -> CsvBS)
    -- , _psSvgs           :: HashMap Text CsvBS
}

$(makeLenses ''ALPState)

instance Default ALPState where
    def = ALPS
        { _powerStationLocs = Nothing
        -- , _psSvgs = H.empty
        }


makeAEMOLivePowerServer :: IO (Either String (Server AEMOLivePower))
makeAEMOLivePowerServer = do
    ref <- newIORef def

    success <- updateALPState ref
    if success
        then do
            _tid <- updateALPState ref `every` (5 :: Minute)
            return . Right $ (serveCSV ref powerStationLocs
                             :<|> serveSVGLive)
        else return . Left $ "The impossible happened!"

updateALPState :: IORef ALPState -> IO Bool
updateALPState ref = do
    current <- readIORef ref
    (locs,pows,dats) <- getLocs
    let csvf = makeCsv locs pows dats
    writeIORef ref $ current
        & powerStationLocs .~ Just csvf
    return True


serveSVGLive :: Text -> Application
serveSVGLive duid _req resp = do
     evs <- runApp dbConn 1 LevelDebug (getPSDForToday duid)
     case evs of
        Left err -> error (show err)
        Right vs -> do
             chrt <- makePSDChart duid vs
             (svg',_) <- liftIO $ renderableToSVGString chrt 500 300
             resp =<< bytestring status200 [("Content-Type", "image/svg+xml")] svg'

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
        [ "DUID"    C..= duidLocationDuid dloc
        , "Lat"     C..= duidLocationLat dloc
        , "Lon"     C..= duidLocationLon dloc
        , "comment" C..= duidLocationComment dloc
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

    replaceKey :: (Hashable k, Eq k) => k -> k -> HashMap k a -> HashMap k a
    replaceKey frm to mp = case H.lookup frm mp of
        Nothing -> mp
        Just v  -> H.insert to v (H.delete frm mp)

    emptyT :: Text
    emptyT = "-"

    emptyDatum :: NamedRecord
    emptyDatum = namedRecord
        [ "Most Recent Output (MW)" C..= emptyT
        , "Sample Time UTC" C..= emptyT
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


-- getPSDForToday :: Text -> AppM (Maybe SvgBS)
getPSDForToday :: Text -> AppM [Entity PowerStationDatum]
getPSDForToday duid = do
    now <- liftIO getZonedTime
    let today :: ZonedTime
        today = now {zonedTimeToLocalTime =
                     (zonedTimeToLocalTime now) {localTimeOfDay = midnight}
                    } -- need lenses :(
        todayUTC :: UTCTime
        todayUTC = zonedTimeToUTC today

    runDB $ do
        selectList [PowerStationDatumDuid ==. duid
                   ,PowerStationDatumSampleTime >=. todayUTC]
                   []

makePSDChart :: Text -> [Entity PowerStationDatum] -> IO (Renderable ())
makePSDChart duid es = do
    tz <- liftIO getCurrentTimeZone
    let psds = map entityVal es
        tvs = map (\psd -> (powerStationDatumSampleTime psd, powerStationDatumMegaWatt psd)) psds
        lvs = map (\(t,v) -> (utcToLocalTime tz t, v)) tvs
    return $ wsChart [(duid,lvs)] $ do
                layout_title .= (T.unpack ("Production for " <> duid))
                layout_y_axis . laxis_title .= "MW"
                layout_x_axis . laxis_title .= timeZoneName tz
