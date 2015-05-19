{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module AEMO.LivePower where

import           Control.Applicative
import           Control.Lens

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T


import           Network.HTTP.Base                         (urlEncode)

import           Data.Hashable
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H

import           Data.Maybe                                (catMaybes)

import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)

import           Data.Time.Clock                           (getCurrentTime)
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

import           Control.Monad.Trans.Either
import           Servant

-- import           System.Log.FastLogger
import           Control.Monad.Logger                      (LogLevel (..),
                                                            runNoLoggingT)

import           Data.Default

import           Util.Periodic
import           Util.Types

import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy             hiding (days)
import           Util.Charts

import           AEMO.Database
import           AEMO.Types

import qualified Data.Configurator                         as C
import           Data.Configurator.Types                   (Config)




type AEMOLivePower =
    "svg" :> Capture "DUID" Text :> Get '[SVG] SvgBS
    :<|>
    "csv" :> Capture "tech" Text :> Header "Host" Text :> Get '[CSV] CsvBS




data ALPState = ALPS
    { _csvMap         :: HashMap Text (Text -> CsvBS)
    , _alpConnPool    :: Maybe ConnectionPool
    , _alpMinLogLevel :: LogLevel
    -- , _psSvgs           :: HashMap Text CsvBS
}

$(makeLenses ''ALPState)

instance Default ALPState where
    def = ALPS
        { _csvMap = H.empty
        , _alpConnPool = Nothing
        , _alpMinLogLevel = LevelDebug
        -- , _psSvgs = H.empty
        }


makeAEMOLivePowerServer :: Config -> IO (Either String (Server AEMOLivePower))
makeAEMOLivePowerServer conf = do
    connStr <- C.require conf "db-conn-string"
    conns <- C.lookupDefault 10 conf "db-connections"
    pool <- runNoLoggingT $ createPostgresqlPool connStr conns
    ref <- newIORef def {_alpConnPool = Just pool}

    success <- updateALPState ref
    if not success
        then return . Left $ "The impossible happened!"
        else do
            mins <- C.lookupDefault 5 conf "update-frequency"
            _tid <- updateALPState ref `every` (fromInteger mins :: Minute)
            return . Right $ (serveSVGLive ref :<|> serveCSVByTech ref)


updateALPState :: IORef ALPState -> IO Bool
updateALPState ref = do
    current <- readIORef ref
    let trav :: Text -> [Filter PowerStation] -> IO (Text -> CsvBS)
        trav _typ filt = do
            (locs,pows,dats) <- getLocs ref filt
            return $ makeCsv locs pows dats

    csvs <- H.traverseWithKey trav sectors :: IO (HashMap Text (Text -> CsvBS))

    writeIORef ref $ current
        & csvMap .~ csvs
    return True

sectors :: HashMap Text [Filter PowerStation]
sectors = H.fromList $
    [("all"     ,[])
    ,("wind"    ,[PowerStationFuelSourcePrimary <-. (Just <$> ["Wind"])])
    ,("solar"   ,[PowerStationFuelSourcePrimary <-. (Just <$> ["Solar"])])
    ,("hydro"   ,[PowerStationFuelSourcePrimary <-. (Just <$> ["Hydro"])])
    ,("fossil"  ,[PowerStationFuelSourcePrimary <-. (Just <$> ["Fossil", "Fuel Oil"])])
    ,("bio"     ,[PowerStationFuelSourcePrimary <-. (Just <$>
                                                        ["Biomass"
                                                        ,"Bagasse"
                                                        ,"Landfill / Biogas"
                                                        ,"Landfill, Biogas"
                                                        ,"Renewable/ Biomass / Waste"
                                                        ,"Sewerage"
                                                        ,"Renewable"]
                                                    )
                ]
    )
    ]


serveSVGLive :: IORef ALPState -> Text -> EitherT ServantErr IO SvgBS
serveSVGLive ref = \duid -> liftIO $ do
    st <- readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
    eres <- runAppPool pool lev . runDB $ do
        psName <- fmap (powerStationStationName . entityVal)
                  <$> selectFirst [PowerStationDuid ==. duid] []
        evs <- getPSDForToday duid
        return (evs, psName)
    case eres of
        Left err -> error (show err)
        Right (vs,psName) -> do
             chrt <- makePSDChart duid psName vs
             (svg',_) <- liftIO $ renderableToSVGString chrt 500 300
             return (SvgBS svg')


serveCSVByTech :: IORef ALPState -> Text -> Maybe Text -> EitherT ServantErr IO CsvBS
serveCSVByTech ref = \tech mhost -> do
    st <- liftIO $ readIORef ref
    case (st ^. csvMap . at tech) <*> mhost of
        Nothing -> error $ "tech " ++ show tech ++ " not found"
        Just csv -> return csv



getLocs :: IORef ALPState -> [Filter PowerStation] -> IO ([Entity DuidLocation],[Entity PowerStation],[Entity PowerStationDatum])
getLocs ref filts = do
    st <- readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
    r <- runAppPool pool lev $ do
        runDB $ do
            locs <- selectList [] []
            pows <- selectList filts []
            mrecent <- selectFirst [] [Desc PowerStationDatumSampleTime]
            case mrecent of
                Nothing -> error "Database has no PowerStationData"
                Just epsd -> do
                    -- TODO: this is fragile and doesn't return the most recent for each DUID
                    --       only the values which have samples equal to the latest.
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
                       , replaceKey "MW" "Most Recent Output (MW)"
                        . replaceKey "Sample Time (AEST)" "Most Recent Output Time (AEST)"
                         $ toNamedRecord psd))
        . map entityVal
        $ dats

    replaceKey :: (Hashable k, Eq k) => k -> k -> HashMap k a -> HashMap k a
    replaceKey frm too mp = case H.lookup frm mp of -- too used because of Control.Lens.Getter.to
        Nothing -> mp
        Just v  -> H.insert too v (H.delete frm mp)

    emptyT :: Text
    emptyT = "-"

    emptyDatum :: NamedRecord
    emptyDatum = namedRecord
        [ "Most Recent Output (MW)" C..= emptyT
        , "Most Recent Output Time (AEST)" C..= emptyT
        , "Image" C..= emptyT
        ]


    addImageTag :: Text -> Text -> NamedRecord -> NamedRecord
    addImageTag hst duid rec =
        let encduid = T.pack . urlEncode . T.unpack $ duid
        in H.insert "Image"
        (C.toField $ T.concat ["<img src='http://",hst,"/aemo/svg/",encduid,"'/>"])
        rec

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

    csv hst = unchunkBS $
        encodeByName displayCols
        . catMaybes
        . map (\loc -> do
            let duid = (duidLocationDuid loc)
            ps <- H.lookup duid powsByDuid
            let datum = case H.lookup duid latestByDuid of
                    Nothing -> emptyDatum
                    Just nr -> addImageTag hst duid nr
            return $ H.unions [toLocRec loc
                              , ps
                              , datum
                              ]
            )
        . map entityVal
        $ locs


    in CsvBS . csv


-- getPSDForToday :: Text -> AppM (Maybe SvgBS)
getPSDForToday :: Text -> DBMonad [Entity PowerStationDatum]
getPSDForToday duid = do
    now <- liftIO getCurrentTime
    let yesterday = now & days -~ 1

    selectList [PowerStationDatumDuid ==. duid
               ,PowerStationDatumSampleTime >=. yesterday]
               []

makePSDChart :: Text -> Maybe Text -> [Entity PowerStationDatum] -> IO (Renderable ())
makePSDChart duid mpsName es = do
    let psds = map entityVal es
        tvs = map (\psd -> (powerStationDatumSampleTime psd, powerStationDatumMegaWatt psd)) psds
        lvs = map (\(t,v) -> (utcToLocalTime aest t, v)) tvs
    return $ wsChart [(duid,lvs)] $ do
                let title = T.unpack . T.concat $ case mpsName of
                        Nothing -> ["Last 24h of production for ", duid]
                        Just psName -> ["Last 24h of production for ", psName, " (", duid, ")"]
                layout_title .= title
                layout_y_axis . laxis_title .= "MW"
                layout_x_axis . laxis_title .= timeZoneName aest
