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
import qualified Data.ByteString                           as B


import           Data.Maybe                                (catMaybes)

import           Data.Functor
import           Data.Monoid

import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)

import           Data.Time.Clock                           (UTCTime,
                                                            getCurrentTime)
import           Data.Time.Lens
import           Data.Time.LocalTime

import           Control.Monad.IO.Class

import           Data.Csv                                  (NamedRecord,
                                                            encodeByName,
                                                            namedRecord,
                                                            toNamedRecord)
import qualified Data.Csv                                  as C

import           Control.Exception                         (throw)

import           Database.Persist
import           Database.Persist.Postgresql
-- import Database.Persist.Class

import           Servant

-- import           System.Log.FastLogger
import           Control.Monad.Logger                      (LogLevel (..),
                                                            runNoLoggingT)

import           Data.Default

import           Util.Periodic
import           Util.Types
import           Util.Web

import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy             hiding (days)
import           Util.Charts

import           Network.HTTP.Types.Status                 (status200)
import           Network.Wai                               (Application)
import           Network.Wai.Util                          (bytestring)

import           AEMO.Database
import           AEMO.Types



type AEMOLivePower =
    "csv" :> Raw
    :<|>
    Capture "DUID" Text :> "svg" :> Raw




data ALPState = ALPS
    { _powerStationLocs :: Maybe (Text -> CsvBS)
    , _alpConnPool      :: Maybe ConnectionPool
    , _alpMinLogLevel   :: LogLevel
    -- , _psSvgs           :: HashMap Text CsvBS
}

$(makeLenses ''ALPState)

instance Default ALPState where
    def = ALPS
        { _powerStationLocs = Nothing
        , _alpConnPool = Nothing
        , _alpMinLogLevel = LevelDebug
        -- , _psSvgs = H.empty
        }


makeAEMOLivePowerServer :: IO (Either String (Server AEMOLivePower))
makeAEMOLivePowerServer = do
    connStr <- dbConn
    pool <- runNoLoggingT $ createPostgresqlPool connStr 1
    ref <- newIORef def {_alpConnPool = Just pool}

    success <- updateALPState ref
    if success
        then do
            _tid <- updateALPState ref `every` (5 :: Minute)
            return . Right $ (serveCSV ref powerStationLocs
                             :<|> serveSVGLive ref)
        else return . Left $ "The impossible happened!"

updateALPState :: IORef ALPState -> IO Bool
updateALPState ref = do
    current <- readIORef ref
    (locs,pows,dats) <- getLocs ref
    let csvf = makeCsv locs pows dats
    writeIORef ref $ current
        & powerStationLocs .~ Just csvf
    return True


serveSVGLive :: IORef ALPState -> Text -> Application
serveSVGLive ref duid _req resp = do
    st <- readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
    evs <- runAppPool pool lev (getPSDForToday duid)
    case evs of
        Left err -> error (show err)
        Right vs -> do
             chrt <- makePSDChart duid vs
             (svg',_) <- liftIO $ renderableToSVGString chrt 500 300
             resp =<< bytestring status200 [("Content-Type", "image/svg+xml")] svg'

getLocs :: IORef ALPState -> IO ([Entity DuidLocation],[Entity PowerStation],[Entity PowerStationDatum])
getLocs ref = do
    st <- readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
    r <- runAppPool pool lev $ do
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
        [ "Station Name"
        , "Most Recent Output (MW)"
        , "Most Recent Output Time (AEST)"
        , "Max Cap (MW)"
        , "Reg Cap (MW)"
        , "Max ROC/Min"
        , "Unit Size (MW)"
        , "Physical Unit No."
        , "Participant"
        , "Dispatch Type"
        , "Category"
        , "Classification"
        , "Fuel Source - Primary"
        , "Fuel Source - Descriptor"
        , "Technology Type - Primary"
        , "Technology Type - Descriptor"
        , "Aggregation"
        , "DUID"
        , "Lat"
        , "Lon"
        , "Image"
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
        , "Image" C..= emptyT
        ]


    addImage :: Text -> Text -> NamedRecord -> NamedRecord
    addImage hst duid rec = H.insert "Image" (C.toField $ T.concat ["<img src='http://",hst,"/aemo/",duid,"/svg'/>"]) rec

    csv hst = unchunkBS $
        encodeByName displayCols
        . catMaybes
        . map (\loc -> do
            let duid = (duidLocationDuid loc)
            ps <- H.lookup duid powsByDuid
            let datum = case H.lookup duid latestByDuid of
                    Nothing -> emptyDatum
                    Just nr -> addImage hst duid nr
            return $ H.unions [toLocRec loc
                              , ps
                              , datum
                              ]
            )
        . map entityVal
        $ locs


    in CsvBS . csv


-- getPSDForToday :: Text -> AppM (Maybe SvgBS)
getPSDForToday :: Text -> AppM [Entity PowerStationDatum]
getPSDForToday duid = do
    now <- liftIO getCurrentTime
    let yesterday = now & days -~ 1

    runDB $ do
        selectList [PowerStationDatumDuid ==. duid
                   ,PowerStationDatumSampleTime >=. yesterday]
                   []

makePSDChart :: Text -> [Entity PowerStationDatum] -> IO (Renderable ())
makePSDChart duid es = do
    let psds = map entityVal es
        tvs = map (\psd -> (powerStationDatumSampleTime psd, powerStationDatumMegaWatt psd)) psds
        lvs = map (\(t,v) -> (utcToLocalTime aest t, v)) tvs
    return $ wsChart [(duid,lvs)] $ do
                layout_title .= (T.unpack ("Last 24h of production for " <> duid))
                layout_y_axis . laxis_title .= "MW"
                layout_x_axis . laxis_title .= timeZoneName aest
