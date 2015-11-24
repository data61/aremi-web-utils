{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

module AEMO.LivePower where


import           Control.Applicative                       ((<|>))
import           Control.Arrow                             ((***))
import           Control.Lens                              hiding ((<.))

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T

import           Data.Hashable
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as H

import           Data.Maybe                                (catMaybes,
                                                            fromMaybe)

import           Data.List                                 (sortBy)
import           Data.Ord                                  (comparing)

import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)

import           Data.Time.Clock                           (UTCTime,
                                                            getCurrentTime)
import           Data.Time.Lens
import           Data.Time.LocalTime
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format                          (defaultTimeLocale,
                                                            formatTime)
#else
import           Data.Time.Format                          (formatTime)
import           System.Locale                             (defaultTimeLocale)
#endif


import           Control.Monad.IO.Class

import           Data.Csv                                  (NamedRecord,
                                                            encodeByName,
                                                            namedRecord,
                                                            toNamedRecord)
import qualified Data.Csv                                  as C

import           Data.Vector                               (Vector)

import           Control.Exception                         (throw)

import           Database.Persist
import           Database.Persist.Postgresql
-- import Database.Persist.Class
import           Database.Esqueleto                        ()
import qualified Database.Esqueleto                        as E


import           Control.Monad.Trans.Either
import           Servant
-- import           System.Log.FastLogger
import           Control.Monad.Logger                      (LogLevel (..),
                                                            runNoLoggingT)
import           Data.Default

import           Util.Periodic
import           Util.Types

import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToSVGString)
import           Graphics.Rendering.Chart.Easy             hiding (Vector, days,
                                                            (<.))
import           Util.Charts


import           AEMO.Database
import           AEMO.Types

import qualified Data.Configurator                         as C
import           Data.Configurator.Types                   (Config)

import qualified Data.ByteString.Lazy.Char8                as LC8

import           Text.Printf
import           Text.Read                                 (readMaybe)

import           Data.String


import           Control.Monad.Reader                      (runReaderT)
import           Data.Pool                                 (withResource)



type AEMOLivePower =
    "v2" :> (
        "svg" :> Capture "DUID" Text :> Get '[SVG] SvgBS
        :<|>
        "csv" :> Capture "tech" Text :> Header "Host" Text :> Get '[CSV] CsvBS
    )
    :<|>
    "v3" :> (
        "svg" :> Capture "DUID" Text :> Get '[SVG] SvgBS
        :<|>
        "png" :> Capture "DUID" Text :> Get '[PNG] PngBS
        :<|>
        "csv" :> Capture "tech" Text :> Header "Host" Text :> Get '[CSV] CsvBS
    )
    :<|>
    "v4" :> (
        "svg" :> Capture "DUID" Text :> Get '[SVG] SvgBS
        :<|>
        "png" :> Capture "DUID" Text :> Get '[PNG] PngBS
        :<|>
        "duidcsv"
            :> Capture "DUID" Text
            :> QueryParam "startTime" ISOUtcTime
            :> QueryParam "endTime" ISOUtcTime
            :> QueryParam "offset" TimeOffsets
            :> Get '[CSV] CsvBS
        :<|>
        "csv" :> Capture "tech" Text :> Header "Host" Text :> Get '[CSV] CsvBS
    )



data ALPState api = ALPS
    { _csvMap         :: HashMap Text (Text -> CsvBS)
    , _alpConnPool    :: Maybe ConnectionPool
    , _alpMinLogLevel :: LogLevel
    , _alpChartEnv    :: Maybe ChartEnv
    , _alpAPIProxy    :: Proxy api
    -- , _psSvgs           :: HashMap Text CsvBS
}

$(makeLenses ''ALPState)

instance Default (ALPState api) where
    def = ALPS
        { _csvMap = H.empty
        , _alpConnPool = Nothing
        , _alpMinLogLevel = LevelDebug
        , _alpChartEnv = Nothing
        , _alpAPIProxy = Proxy
        -- , _psSvgs = H.empty
        }



-- Handler initialisation function - called in Main.hs to create the `Server api`.
-- Arguments are:
--  * a `Proxy api` where `api` is the API for the entire app
--  * the Config for the AEMO portion of the app
makeAEMOLivePowerServer
    :: (HasAEMOLinks api)
    => Proxy api -> Config -> ChartEnv -> IO (Either String (Server AEMOLivePower))
makeAEMOLivePowerServer api conf env = do
    connStr <- C.require conf "db-conn-string"
    conns <- C.lookupDefault 10 conf "db-connections"
    pool <- runNoLoggingT $ createPostgresqlPool connStr conns

    -- env <- defaultEnv bitmapAlignmentFns 500 300

    ref <- newIORef def {_alpConnPool = Just pool, _alpChartEnv = Just env, _alpAPIProxy = api}

    success <- updateALPState api ref
    if not success
        then return . Left $ "The impossible happened!"
        else do
            mins <- C.lookupDefault 5 conf "update-frequency"
            _tid <- updateALPState api ref `every` (fromInteger mins :: Minute)
            return . Right $
                {-v2-} (serveSVGLive ref :<|> serveCSVByTech ref)
                {-v3-} :<|> (serveSVGLive ref :<|> servePNGLive ref :<|> serveCSVByTech ref)
                {-v4-} :<|> (serveSVGLive ref :<|> servePNGLive ref :<|> serveCSVByDUID ref :<|> serveCSVByTech ref)

-- Runs every `update-frequency` minutes (from Config passed to makeAEMOLivePowerServer)
-- to update the CSVs returned by serveCSVByTech
updateALPState
    :: (HasAEMOLinks api)
    => Proxy api -> IORef (ALPState api) -> IO Bool
updateALPState api ref = do
    let trav :: Text -> [Text] -> IO (Text -> CsvBS)
        trav _typ filt = do
            (locs,pows,dats, timeMap) <- getLocs ref filt
            return $ makeCsv api locs pows dats timeMap displayCols
                             [("MW",                 "Most Recent Output (MW)")
                             ,("Sample Time (AEST)", "Most Recent Output Time (AEST)")
                             ]

    csvs <- H.traverseWithKey trav (H.fromList sectorMap) :: IO (HashMap Text (Text -> CsvBS))

    current <- readIORef ref
    writeIORef ref $ current
        & csvMap .~ csvs
    return True


displayCols :: Vector C.Name
displayCols =
    [ "Station Name"
    , "Most Recent Output (MW)"
    , "Most Recent Output Time (AEST)"
    , "Most Recent % of Max Cap"
    , "Most Recent % of Reg Cap"
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
    , duidCsvKey24h
    , duidCsvAllKey
    -- , svgKey
    , pngKey
    ]


-- The filters needed to select the correct stations for each technology type
-- from the database
techSectors :: HashMap Text [Filter PowerStation]
techSectors = H.fromList $ map (\(sec,srcs) -> (sec, toInQuery srcs)) sectorMap
    where
        toInQuery :: [Text] -> [Filter PowerStation]
        toInQuery [] = []
        toInQuery xs = [PowerStationFuelSourcePrimary <-. (Just <$> xs)]

sectorMap :: [(Text,[Text])]
sectorMap =
    [("all"       ,[])
    ,("wind"      ,["Wind"])
    ,("solar"     ,["Solar"])
    ,("hydro"     ,["Hydro"])
    ,("fossil"    ,["Fossil", "Fuel Oil"])
    ,("bio"       , bioSectors)
    ,("renewables", ["Solar","Hydro","Wind"] ++ bioSectors)
    ]

bioSectors :: [Text]
bioSectors =
    ["Biomass"
    ,"Bagasse"
    ,"Landfill / Biogas"
    ,"Landfill, Biogas"
    ,"Renewable/ Biomass / Waste"
    ,"Sewerage"
    ,"Renewable"
    ]

-- =============
-- HTTP Handlers
-- =============

-- When given a reference to the app state, produces a handler which accepts
-- a Text value which specifies which power station to to produce a graph
-- of the last 24h of generation.
serveSVGLive :: IORef (ALPState api) -> Text -> EitherT ServantErr IO SvgBS
serveSVGLive ref = \duid -> do
    st <- liftIO $ readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
    eres <- liftIO $ runAppPool pool lev . runDB $ do
        psName <- fmap (powerStationStationName . entityVal)
                  <$> selectFirst [PowerStationDuid ==. duid] []
        evs <- getPSDForToday duid
        return (evs, psName)
    case eres of
        Left err -> left err404 {errBody = LC8.pack (show err)}
        Right (vs,psName) -> liftIO $ do
             let chrt = makePSDChart duid psName vs
             (svg',_) <- liftIO $ renderableToSVGString chrt 500 300
             return (Tagged svg')

-- Same as serveSVGLive but produces a PNG instead.
servePNGLive :: IORef (ALPState api) -> Text -> EitherT ServantErr IO PngBS
servePNGLive ref = \duid -> do
    st <- liftIO $ readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
        Just env = st ^. alpChartEnv
    eres <- liftIO $ runAppPool pool lev . runDB $ do
        psName <- fmap (powerStationStationName . entityVal)
                  <$> selectFirst [PowerStationDuid ==. duid] []
        evs <- getPSDForToday duid
        return (evs, psName)
    case eres of
        Left err -> left err404 {errBody = LC8.pack (show err)}
        Right (vs,psName) -> liftIO $ do
             let chrt = makePSDChart duid psName vs
             img <- renderImage env 500 300 chrt
             return (renderToPng img)

-- Returns the CSV for the specified technology group. If the Host header is not
-- present (which is an HTTP/1.1 no-no), or the group isn't known then the
-- function will fail.
serveCSVByTech :: IORef (ALPState api) -> Text -> Maybe Text -> EitherT ServantErr IO CsvBS
serveCSVByTech ref = \tech mhost -> do
    st <- liftIO $ readIORef ref
    case (st ^. csvMap . at tech) <*> mhost of
        Nothing -> left err404 {errBody = LC8.pack ("tech " ++ show tech ++ " not found")}
        Just csv -> return csv


serveCSVByDUID :: (IsElem SVGPath api, IsElem PNGPath api)
               => IORef (ALPState api)
               -> Text
               -> Maybe ISOUtcTime -> Maybe ISOUtcTime -- start and end times for data
               -> Maybe TimeOffsets -- Time offset from now, eg "1Y6M" for 1 year and 6 months, supports YMDhm
               -> EitherT ServantErr IO CsvBS
serveCSVByDUID ref = \duid mstart mend moff -> do
    r <- liftIO $ do
        st <- readIORef ref
        moff' <- case moff of
            Nothing -> pure Nothing
            Just offs ->  Just . modifyTime offs <$> getCurrentTime

        let Just pool = st ^. alpConnPool
            lev = st ^. alpMinLogLevel

        runAppPool pool lev $ runDB $
                selectList ([PowerStationDatumDuid ==. duid]
                            ++ catMaybes
                                [ (PowerStationDatumSampleTime >=.) <$> (moff' <|> (unISOUtc <$> mstart))
                                ,((PowerStationDatumSampleTime <=.) . unISOUtc) <$> mend
                                ]
                            )
                           [Asc PowerStationDatumSampleTime]
    case r of
        Left ex
            -> left err500 {errBody = LC8.pack (show ex)}
        Right allRecs
            -> pure . Tagged . encodeByName duidCols
                . map (substituteNames duidSubs . toNamedRecord . entityVal)
                $ allRecs

duidSubs :: [(C.Name,C.Name)]
duidSubs =
    [("Sample Time (AEST)", "Time (AEST)")
    ]


duidCols :: Vector C.Name
duidCols =
    -- [ "Station Name"
    [ "Time (AEST)"
    , "MW"
    -- , "% of Reg Cap"
    -- , "% of Max Cap"
    -- , "Max Cap (MW)"
    -- , "Reg Cap (MW)"
    -- , "Max ROC/Min"
    -- , "Unit Size (MW)"
    -- , "Physical Unit No."
    -- , "Participant"
    -- , "Dispatch Type"
    -- , "Category"
    -- , "Classification"
    -- , "Fuel Source - Primary"
    -- , "Fuel Source - Descriptor"
    -- , "Technology Type - Primary"
    -- , "Technology Type - Descriptor"
    -- , "Aggregation"
    -- , "DUID"
    -- , "Lat"
    -- , "Lon"
    -- , svgKey
    -- , pngKey
    ]


-- Query the database to obtain lists of DuidLocations, PowerStations and PowerStationData
-- based on the given list of filters to select a technology group.
getLocs :: IORef (ALPState api)
        -> [Text]
        -> IO ( [Entity DuidLocation]
              , [Entity PowerStation]
              , [Entity PowerStationDatum]
              , HashMap Text UTCTime
              )
getLocs ref filts = do
    st <- readIORef ref
    let Just pool = st ^. alpConnPool
        lev = st ^. alpMinLogLevel
    r <- runAppPool pool lev $ do
        runDB $ do
            mrecent <- E.select $ E.from $ \datum -> do
                pure (E.max_ (datum E.^. PowerStationDatumSampleTime))

            locs <- E.select $ E.from $ pure
            pows <- E.select $ E.from $ \ps -> do
                if null filts
                    then return ()
                    else E.where_ (ps E.^. PowerStationFuelSourcePrimary `E.in_` E.valList (map Just filts))
                return ps

            latests :: [(Single Text, Single UTCTime)]
                <- withResource pool $ \sqlbknd -> do
                    liftIO $ flip runReaderT sqlbknd $
                        rawSql "SELECT duid, sample_time FROM latest_power_station_datum;"
                               []
            let timeMap = H.fromList . map (unSingle *** unSingle) $ latests

            case mrecent of
                [] -> error "Database has no PowerStationData"
                [E.Value Nothing] -> error "Database has no PowerStationData"
                [E.Value (Just epsd)] -> do
                    dats <- E.select $ E.from $ \dat -> do
                        E.where_ (dat E.^. PowerStationDatumSampleTime E.==. E.val epsd)
                        pure dat :: E.SqlQuery (E.SqlExpr (Entity PowerStationDatum))
                    return (locs, pows, dats, timeMap)
    case r of
        Left ex -> throw ex
        Right ls -> return ls



replaceKey :: (Hashable k, Eq k, Show k) => k -> k -> HashMap k a -> HashMap k a
replaceKey frm too mp = case H.lookup frm mp of -- too used because of Control.Lens.Getter.to
    Nothing -> error $ "replaceKey: Key not found: " ++ show frm -- mp
    Just v  -> H.insert too v (H.delete frm mp)


substituteNames :: [(C.Name,C.Name)] -> NamedRecord -> NamedRecord
substituteNames subs = foldr (\(frm,too) f -> replaceKey frm too . f) id subs

type HasAEMOLinks api =
    ( IsElem SVGPath api
    , IsElem PNGPath api
    , IsElem DUIDCSVPathWithOffset api
    , IsElem DUIDCSVPath api
    )

type SVGPath = ("aemo" :> "v2" :> "svg" :> Capture "DUID" Text :> Get '[SVG] SvgBS)
type PNGPath = ("aemo" :> "v4" :> "png" :> Capture "DUID" Text :> Get '[PNG] PngBS)
type DUIDCSVPath = ("aemo" :> "v4":> "duidcsv":> Capture "DUID" Text :> Get '[CSV] CsvBS )
type DUIDCSVPathWithOffset = ("aemo" :> "v4"
    :> "duidcsv"
            :> Capture "DUID" Text
            :> QueryParam "offset" TimeOffsets
            :> Get '[CSV] CsvBS
            )

emptyT :: Text
emptyT = "-"

svgKey :: IsString a => a
svgKey = "Latest 24h generation (SVG)"

pngKey :: IsString a => a
pngKey = "Latest 24h generation"

duidCsvKey24h :: IsString a => a
duidCsvKey24h = "CSV for last 24h"

duidCsvAllKey :: IsString a => a
duidCsvAllKey = "CSV for all data"

makeCsv :: (HasAEMOLinks api)
        => Proxy api
        -> [Entity DuidLocation]
        -> [Entity PowerStation]
        -> [Entity PowerStationDatum]
        -> HashMap Text UTCTime
        -> Vector C.Name
        -> [(C.Name,C.Name)]
        -> (Text -> CsvBS)
makeCsv api locs pows dats timeMap colNames subs = let
    toLocRec :: DuidLocation -> NamedRecord
    toLocRec dloc = namedRecord
        [ "DUID"    C..= duidLocationDuid dloc
        , "Lat"     C..= duidLocationLat dloc
        , "Lon"     C..= duidLocationLon dloc
        , "comment" C..= duidLocationComment dloc
        ]

    powsByDuid :: HashMap Text PowerStation
    powsByDuid =
        H.fromList
        . map (\ps -> (powerStationDuid ps, ps))
        . map entityVal
        $ pows

    latestByDuid :: HashMap Text [PowerStationDatum]
    latestByDuid =
        H.fromListWith (++)
        . map (\psd -> (powerStationDatumDuid psd
                       , [psd]))
        . map entityVal
        $ dats

    _emptyDatum :: NamedRecord
    _emptyDatum = namedRecord
        [ "MW" C..= emptyT
        , "Sample Time (AEST)" C..= emptyT
        , svgKey C..= emptyT
        , pngKey C..= emptyT
        , duidCsvKey24h C..= emptyT
        , duidCsvAllKey C..= emptyT
        ]


    addImageTag :: Text -> Text -> NamedRecord -> NamedRecord
    addImageTag hst duid rec =
        let svgProxy           = Proxy :: Proxy SVGPath
            pngProxy           = Proxy :: Proxy PNGPath
            svgUri             = safeLink api svgProxy duid
            pngUri             = safeLink api pngProxy duid
        in   H.insert svgKey
            (C.toField $ T.concat ["<img src='http://",hst,"/",T.pack $ show svgUri,"'/>"])
            $ H.insert pngKey
            (C.toField $ T.concat ["<img src='http://",hst,"/",T.pack $ show pngUri,"'/>"])
            rec

    addCsvLinks :: Text -> Text -> NamedRecord -> NamedRecord
    addCsvLinks hst duid rec =
        let duidCsvOffsetProxy = Proxy :: Proxy DUIDCSVPathWithOffset
            duidCsvProxy       = Proxy :: Proxy DUIDCSVPath
            duidCsv24hUri      = safeLink api duidCsvOffsetProxy duid (TimeOffsets [TimeOffset 24 H])
            duidCsvUri         = safeLink api duidCsvProxy duid
        in H.insert duidCsvKey24h
            (C.toField $ T.concat ["<a href='http://",hst,"/",T.pack $ show duidCsv24hUri,"'>Download</a>"])
            $ H.insert duidCsvAllKey
            (C.toField $ T.concat ["<a href='http://",hst,"/",T.pack $ show duidCsvUri,"'>Download</a>"])
            rec


    csv hst = unchunkBS $
        encodeByName colNames
        . concat
        . catMaybes
        . map (\loc -> do
            let duid = (duidLocationDuid loc)
            ps <- H.lookup duid powsByDuid

            let recs = case H.lookup duid latestByDuid of
                    Nothing -> [(_emptyDatum, Nothing, Nothing)]
                    Just psds -> map (\psd -> (addImageTag hst duid . toNamedRecord $ psd
                                            , calculateProdPct powerStationRegCapMW ps psd
                                            , calculateProdPct powerStationMaxCapMW ps psd )
                                    ) psds
            return $ map (\(datum, regCapPct,maxCapPct) ->
                    let latestTime = H.lookup duid timeMap
                        timeString = fmap (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z". utcToZonedTime aest)
                                          latestTime
                    in substituteNames subs
                       $ H.unions [toLocRec loc
                                  , toNamedRecord ps
                                  , H.insert "Sample Time (AEST)"
                                                (C.toField $ fromMaybe "-" timeString)
                                                (addCsvLinks hst duid datum)
                                  , namedRecord ["Most Recent % of Reg Cap"
                                    C..= (fromMaybe "-" $ printf "%.2f" <$> regCapPct :: String)]
                                  , namedRecord ["Most Recent % of Max Cap"
                                    C..= (fromMaybe "-" $ printf "%.2f" <$> maxCapPct :: String)]
                                  ]
                    ) recs
            )
        . map entityVal
        $ locs

    in Tagged . csv


calculateProdPct :: (PowerStation -> Maybe Text) -> PowerStation -> PowerStationDatum -> Maybe Double
calculateProdPct capfun ps psd = do
    cap <- capfun ps
    tot <- readMaybe . T.unpack $ cap
    let mw = powerStationDatumMegaWatt psd
    return (100 * mw/tot)


getPSDForToday :: Text -> DBMonad [Entity PowerStationDatum]
getPSDForToday duid = do
    now <- liftIO getCurrentTime
    let yesterday = now & flexDT . days -~ 1

    selectList [PowerStationDatumDuid ==. duid
               ,PowerStationDatumSampleTime >=. yesterday]
               []


makePSDChart :: Text -> Maybe Text -> [Entity PowerStationDatum] -> Renderable ()
makePSDChart duid mpsName es =
    let psds = map entityVal es
        tvs = map (\psd -> (powerStationDatumSampleTime psd, powerStationDatumMegaWatt psd)) psds
        lvs = map (\(t,v) -> (utcToLocalTime aest t, v)) tvs
    in wsChart [(duid,lvs)] $ do
                let title = T.unpack . T.concat $ case mpsName of
                        Nothing -> ["Last 24h of production for ", duid]
                        Just psName -> ["Last 24h of production for ", psName, " (", duid, ")"]
                layout_title .= title
                layout_y_axis . laxis_title .= "MW"
                layout_x_axis . laxis_title .= timeZoneName aest
