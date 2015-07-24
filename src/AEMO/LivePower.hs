{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Codec.Picture.Png                         (encodePng)
import           Util.Charts

import           AEMO.Database
import           AEMO.Types

import qualified Data.Configurator                         as C
import           Data.Configurator.Types                   (Config)

import qualified Data.ByteString.Lazy.Char8                as LC8

import           Text.Read                                 (readMaybe)
import           Text.Printf

import           Data.String



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

-- Handler initialisation function - called in Main.hs to create the `Server api`.
-- Arguments are:
--  * a `Proxy api` where `api` is the API for the entire app
--  * the Config for the AEMO portion of the app
makeAEMOLivePowerServer
    :: (IsElem SVGPath api, IsElem PNGPath api)
    => Proxy api -> Config -> IO (Either String (Server AEMOLivePower))
makeAEMOLivePowerServer api conf = do
    connStr <- C.require conf "db-conn-string"
    conns <- C.lookupDefault 10 conf "db-connections"
    pool <- runNoLoggingT $ createPostgresqlPool connStr conns
    ref <- newIORef def {_alpConnPool = Just pool}

    success <- updateALPState api ref
    if not success
        then return . Left $ "The impossible happened!"
        else do
            mins <- C.lookupDefault 5 conf "update-frequency"
            _tid <- updateALPState api ref `every` (fromInteger mins :: Minute)
            return . Right $
                {-v2-} (serveSVGLive ref :<|> serveCSVByTech ref)
                {-v3-} :<|> (serveSVGLive ref :<|> servePNGLive ref :<|> serveCSVByTech ref)

-- Runs every `update-frequency` minutes (from Config passed to makeAEMOLivePowerServer)
-- to update the CSVs returned by serveCSVByTech
updateALPState
    :: (IsElem SVGPath api, IsElem PNGPath api)
    => Proxy api -> IORef ALPState -> IO Bool
updateALPState api ref = do
    current <- readIORef ref
    let trav :: Text -> [Filter PowerStation] -> IO (Text -> CsvBS)
        trav _typ filt = do
            (locs,pows,dats) <- getLocs ref filt
            return $ makeCsv api locs pows dats

    csvs <- H.traverseWithKey trav sectors :: IO (HashMap Text (Text -> CsvBS))

    writeIORef ref $ current
        & csvMap .~ csvs
    return True


-- The filters needed to select the correct stations for each technology type
-- from the database
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

-- =============
-- HTTP Handlers
-- =============

-- When given a reference to the app state, produces a handler which accepts
-- a Text value which specifies which power station to to produce a graph
-- of the last 24h of generation.
serveSVGLive :: IORef ALPState -> Text -> EitherT ServantErr IO SvgBS
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
servePNGLive :: IORef ALPState -> Text -> EitherT ServantErr IO PngBS
servePNGLive ref = \duid -> do
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
             img <- renderImage 500 300 chrt
             return (Tagged (encodePng img))

-- Returns the CSV for the specified technology group. If the Host header is not
-- present (which is an HTTP/1.1 no-no), or the group isn't known then the
-- function will fail.
serveCSVByTech :: IORef ALPState -> Text -> Maybe Text -> EitherT ServantErr IO CsvBS
serveCSVByTech ref = \tech mhost -> do
    st <- liftIO $ readIORef ref
    case (st ^. csvMap . at tech) <*> mhost of
        Nothing -> left err404 {errBody = LC8.pack ("tech " ++ show tech ++ " not found")}
        Just csv -> return csv


-- Query the database to obtain lists of DuidLocations, PowerStations and PowerStationData
-- based on the given list of filters to select a technology group.
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

type SVGPath = ("aemo" :> "v3" :> "svg" :> Capture "DUID" Text :> Get '[SVG] SvgBS)
type PNGPath = ("aemo" :> "v3" :> "png" :> Capture "DUID" Text :> Get '[PNG] PngBS)

makeCsv :: (IsElem SVGPath api, IsElem PNGPath api)
        => Proxy api
        -> [Entity DuidLocation]
        -> [Entity PowerStation]
        -> [Entity PowerStationDatum]
        -> (Text -> CsvBS)
makeCsv api locs pows dats = let
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

    latestByDuid :: HashMap Text PowerStationDatum
    latestByDuid =
        H.fromList
        . map (\psd -> (powerStationDatumDuid psd
                       , psd))
        . map entityVal
        $ dats

    replaceKey :: (Hashable k, Eq k) => k -> k -> HashMap k a -> HashMap k a
    replaceKey frm too mp = case H.lookup frm mp of -- too used because of Control.Lens.Getter.to
        Nothing -> mp
        Just v  -> H.insert too v (H.delete frm mp)

    emptyT :: Text
    emptyT = "-"

    svgKey :: IsString a => a
    svgKey = "Latest 24h generation (SVG)"

    pngKey :: IsString a => a
    pngKey = "Latest 24h generation"

    emptyDatum :: NamedRecord
    emptyDatum = namedRecord
        [ "Most Recent Output (MW)" C..= emptyT
        , "Most Recent Output Time (AEST)" C..= emptyT
        , svgKey C..= emptyT
        , pngKey C..= emptyT
        ]


    addImageTag :: Text -> Text -> NamedRecord -> NamedRecord
    addImageTag hst duid rec =
        let encduid = T.pack . urlEncode . T.unpack $ duid
            svgProxy = Proxy :: Proxy SVGPath
            pngProxy = Proxy :: Proxy PNGPath
            svguri = safeLink api svgProxy encduid
            pnguri = safeLink api pngProxy encduid
        in H.insert svgKey
            (C.toField $ T.concat ["<img src='http://",hst,"/",T.pack $ show svguri,"'/>"])
            $ H.insert pngKey
            (C.toField $ T.concat ["<img src='http://",hst,"/",T.pack $ show pnguri,"'/>"])
            rec

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
        -- , svgKey
        , pngKey
        ]

    csv hst = unchunkBS $
        encodeByName displayCols
        . catMaybes
        . map (\loc -> do
            let duid = (duidLocationDuid loc)
            ps <- H.lookup duid powsByDuid

            let (datum, regCapPct, maxCapPct) = case H.lookup duid latestByDuid of
                    Nothing -> (emptyDatum, Nothing, Nothing)
                    Just nr -> (addImageTag hst duid . toNamedRecord $ nr
                                , calculateProdPct powerStationRegCapMW ps nr
                                , calculateProdPct powerStationMaxCapMW ps nr )
            return $ H.unions [toLocRec loc
                              , toNamedRecord ps
                              , replaceKey "MW" "Most Recent Output (MW)"
                                . replaceKey "Sample Time (AEST)" "Most Recent Output Time (AEST)"
                                . toNamedRecord
                                $ datum
                              , namedRecord ["Most Recent % of Reg Cap"
                                C..= (printf "%.2f" <$> regCapPct :: Maybe String)]
                              , namedRecord ["Most Recent % of Max Cap"
                                C..= (printf "%.2f" <$> maxCapPct :: Maybe String)]
                              ]
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
