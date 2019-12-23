{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ExistentialQuantification        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE DataKinds        #-}
module Infra where

import Data.Maybe

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Monoid ((<>))
import Data.Maybe
import           Data.Tuple (fst)
import Prelude (Int, (<=), dropWhile, error, mempty, read, (.), (/=), (==), String, Maybe, Bool(..), ($), Double, Show, Eq, Ord, filter, fmap, pure, fail, show, tail, takeWhile, undefined)
import           Control.DeepSeq
import Data.Char (toLower)
import           Data.Typeable
import           GHC.Generics                   ( Generic )

import           React.Flux.Rn.Views
import           React.Flux.Rn.Properties (Props)
import Svg.SvgXml
import LayerTypes
import Data.Typeable
import Numeric.Natural
import qualified Data.Text as T
import GHC.TypeLits
import Maps.Circle
import Maps.Polyline
import Maps.Polygon
import Numeric (readHex)

apiBase :: String
apiBase = "https://rata.digitraffic.fi/infra-api/0.5/"

data Infra =
    TrafficControlAreas
  | AccountingRailwaySections
  | SpeedRestrictionAreas
  | TransportationPlanningAreas
  
  | PositioningMarkers
  | Milestones
  | Railways
  
  | TimetableLocations
  | PartsOfStation
  | LineSwitches
  | Stops
  | Stations
  | StationIntervals

  | AudioFrequencyTrackCircuits
  | AxleCountingSections
  | TrackCircuits
  | AxleCounters
  | RailInsulations
  | Tracks
  
  | Tunnels
  | Bridges
  | Platforms
  | LevelCrossings

  | ElectricalWorkInsulators
  | GroundingDevices
  | SeparationFields
  | SeparationSections
  | ElectrificationEnds
  | GroupingInsulators
  | ElectrificationGroups
  
  | PantographMonitoringCameras
  | RFIDReaders
  | WheelForceIndicators
  | HotboxDetectors

  | StopBoards
  | Derailers
  | StationBoundaryMarks

  | Balises
  | Signals
  | Buffers
  | Switches
  deriving (Show, Typeable, Generic, NFData, Eq, Ord)

data GOid (a :: Infra) = GOid { oid :: Oid }
  deriving (Show, Typeable, Generic, NFData, Eq, Ord)

parseGOid :: Natural -> Value -> Parser (GOid a)
parseGOid b = withText "GOid" (\t -> if T.isPrefixOf (T.pack $ "1.2.246.586.1." <> show b <> ".") t then pure (GOid $ Oid $ T.unpack t) else fail "unexpected oid")

parseGOidTrex :: Natural -> Value -> Parser (GOid a)
parseGOidTrex b = withText "GOid" (\t -> if T.isPrefixOf (T.pack $ "1.2.246.578.1." <> show b <> ".") t then pure (GOid $ Oid $ T.unpack t) else fail "unexpected oid")

instance FromJSON (GOid TrafficControlAreas)          where parseJSON = parseGOid 48
instance FromJSON (GOid AccountingRailwaySections)    where parseJSON = parseGOid 32
instance FromJSON (GOid SpeedRestrictionAreas)        where parseJSON = parseGOid 0 -- no identity
instance FromJSON (GOid TransportationPlanningAreas)  where parseJSON = parseGOid 29

instance FromJSON (GOid PositioningMarkers)           where parseJSON = parseGOid 43
instance FromJSON (GOid Milestones)                   where parseJSON = parseGOid 41
instance FromJSON (GOid Railways)                     where parseJSON = parseGOid 45

instance FromJSON (GOid TimetableLocations)           where parseJSON = parseGOid 0 -- no identity
instance FromJSON (GOid PartsOfStation)               where parseJSON = parseGOid 51
instance FromJSON (GOid LineSwitches)                 where parseJSON = parseGOid 39
instance FromJSON (GOid Stops)                        where parseJSON = parseGOid 39
instance FromJSON (GOid Stations)                     where parseJSON = parseGOid 39
instance FromJSON (GOid StationIntervals)             where parseJSON = parseGOid 40

instance FromJSON (GOid AudioFrequencyTrackCircuits)  where parseJSON = parseGOid 36
instance FromJSON (GOid AxleCountingSections)         where parseJSON = parseGOid 36
instance FromJSON (GOid TrackCircuits)                where parseJSON = parseGOid 36
instance FromJSON (GOid AxleCounters)                 where parseJSON = parseGOid 10
instance FromJSON (GOid RailInsulations)              where parseJSON = parseGOid 17 
instance FromJSON (GOid Tracks)                       where parseJSON = parseGOid 44

instance FromJSON (GOid Tunnels)                      where parseJSON = parseGOidTrex 15
instance FromJSON (GOid Bridges)                      where parseJSON = parseGOidTrex 17
instance FromJSON (GOid Platforms)                    where parseJSON = parseGOid 37
instance FromJSON (GOid LevelCrossings)               where parseJSON = parseGOid 23

instance FromJSON (GOid ElectricalWorkInsulators)     where parseJSON = parseGOid 71
instance FromJSON (GOid GroundingDevices)             where parseJSON = parseGOid 70
instance FromJSON (GOid SeparationFields)             where parseJSON = parseGOid 68
instance FromJSON (GOid SeparationSections)           where parseJSON = parseGOid 67
instance FromJSON (GOid ElectrificationEnds)          where parseJSON = parseGOid 21
instance FromJSON (GOid GroupingInsulators)           where parseJSON = parseGOid 20
instance FromJSON (GOid ElectrificationGroups)        where parseJSON = parseGOid 33

instance FromJSON (GOid PantographMonitoringCameras)  where parseJSON = parseGOid 25
instance FromJSON (GOid RFIDReaders)                  where parseJSON = parseGOid 19
instance FromJSON (GOid WheelForceIndicators)         where parseJSON = parseGOid 16
instance FromJSON (GOid HotboxDetectors)              where parseJSON = parseGOid 12

instance FromJSON (GOid StopBoards)                   where parseJSON = parseGOid 22
instance FromJSON (GOid Derailers)                    where parseJSON = parseGOid 18
instance FromJSON (GOid StationBoundaryMarks)         where parseJSON = parseGOid 13

instance FromJSON (GOid Balises)                      where parseJSON = parseGOid 11
instance FromJSON (GOid Signals)                      where parseJSON = parseGOid 14
instance FromJSON (GOid Buffers)                      where parseJSON = parseGOid 15
instance FromJSON (GOid Switches)                     where parseJSON = parseGOid 24

layerPath :: Infra -> LayerSource
layerPath = \case
  TrafficControlAreas         -> LayerSource "toimialueet"               Nothing                          ["nimi","tunniste","valit.geometria","vari"]
  AccountingRailwaySections   -> LayerSource "tilirataosat"              Nothing                          ["geometria","nimi","numero","tunniste"]
  SpeedRestrictionAreas       -> LayerSource "nopeusrajoitusalueet"      Nothing                          ["geometria"]
  TransportationPlanningAreas -> LayerSource "liikennesuunnittelualueet" Nothing                          ["geometria", "nimi","tunniste"]

  PositioningMarkers          -> LayerSource "paikantamismerkit"         Nothing                          ["geometria","numero","sijainnit","tunniste"]
  Milestones                  -> LayerSource "kilometrimerkit"           Nothing                          ["geometria","pituus","ratakm","ratanumero","tunniste"]
  Railways                    -> LayerSource "radat"                     Nothing                          ["geometria","ratanumero","tunniste"]

  TimetableLocations          -> LayerSource "aikataulupaikat"           Nothing                          ["geometria","tunniste","uickoodi"]
  PartsOfStation              -> LayerSource "liikennepaikanosat"        Nothing                          ["geometria","lyhenne","nimi","tunniste"]
  LineSwitches                -> LayerSource "rautatieliikennepaikat"    (Just "linjavaihde")             ["geometria","lyhenne","nimi","tunniste"]
  Stops                       -> LayerSource "rautatieliikennepaikat"    (Just "seisake")                 ["geometria","lyhenne","nimi","tunniste"]
  Stations                    -> LayerSource "rautatieliikennepaikat"    (Just "liikennepaikka")          ["geometria","lyhenne","nimi","tunniste"]
  StationIntervals            -> LayerSource "liikennepaikkavalit"       Nothing                          ["geometria","tunniste"]

  AudioFrequencyTrackCircuits -> LayerSource "raideosuudet"              (Just "raidevirtapiiri")         ["geometria","tunniste","turvalaiteNimi","turvalaiteRaide"]
  AxleCountingSections        -> LayerSource "raideosuudet"              (Just "akselinlaskentaosuus")    ["geometria","tunniste","turvalaiteNimi","turvalaiteRaide"]
  TrackCircuits               -> LayerSource "raideosuudet"              (Just "eristysosuus")            ["geometria","tunniste","turvalaiteNimi","turvalaiteRaide"]
  AxleCounters                -> LayerSource "elementit"                 (Just "akselinlaskija")          ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  RailInsulations             -> LayerSource "elementit"                 (Just "raideeristys")            ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  Tracks                      -> LayerSource "raiteet"                   Nothing                          ["geometria","kuvaus","tunnus","tunniste"]

  Tunnels                     -> LayerSource "tunnelit"                  Nothing                          ["geometria","nimi","tunniste"]
  Bridges                     -> LayerSource "sillat"                    Nothing                          ["geometria","nimi","siltakoodi","tunniste"]
  Platforms                   -> LayerSource "laiturit"                  Nothing                          ["geometria","kaupallinenNumero","tunniste","tunnus"]
  LevelCrossings              -> LayerSource "tasoristeykset"            Nothing                          ["geometria","nimi","tunniste","tunnus"]

  ElectricalWorkInsulators    -> LayerSource "elementit"                 (Just "tyonaikaineneristin")     ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  GroundingDevices            -> LayerSource "elementit"                 (Just "maadoittimet" )           ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  SeparationFields            -> LayerSource "elementit"                 (Just "erotuskentat")            ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  SeparationSections          -> LayerSource "elementit"                 (Just "erotusjaksot")            ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  ElectrificationEnds         -> LayerSource "elementit"                 (Just "sahkoistyspaattyy")       ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  GroupingInsulators          -> LayerSource "elementit"                 (Just "ryhmityseristin")         ["geometria","nimi","rotaatio","ryhmityseristin.nopeastiAjettava","tunniste","tyyppi"]
  ElectrificationGroups       -> LayerSource "kytkentaryhmat"            Nothing                          ["geometria","numero","tunniste","vari"]

  PantographMonitoringCameras -> LayerSource "elementit"                 (Just "virroitinvalvontakamera") ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  RFIDReaders                 -> LayerSource "elementit"                 (Just "rfidlukija")              ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  WheelForceIndicators        -> LayerSource "elementit"                 (Just "pyoravoimailmaisin")      ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  HotboxDetectors             -> LayerSource "elementit"                 (Just "kuumakayntiilmaisin")     ["geometria","nimi","rotaatio","tunniste","tyyppi"]

  StopBoards                  -> LayerSource "elementit"                 (Just "seislevy")                ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  Derailers                   -> LayerSource "elementit"                 (Just "raiteensulku")            ["geometria","nimi","raiteensulku.kasinAsetettava","rotaatio","tunniste","tyyppi"]
  StationBoundaryMarks        -> LayerSource "elementit"                 (Just "liikennepaikanraja")      ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  Balises                     -> LayerSource "elementit"                 (Just "baliisi")                 ["baliisi.toistopiste","geometria","nimi","rotaatio","baliisi.suunta","tunniste","tyyppi"]
  Signals                     -> LayerSource "elementit"                 (Just "opastin")                 ["geometria","nimi","opastin.puoli","opastin.tyyppi","rotaatio","opastin.suunta","tunniste,tyyppi"]
  Buffers                     -> LayerSource "elementit"                 (Just "puskin")                  ["geometria","nimi","rotaatio","tunniste","tyyppi"]
  Switches                    -> LayerSource "elementit"                 (Just "vaihde")                  ["geometria","nimi","rotaatio","tunniste","tyyppi","vaihde.kasikaantoisyys","vaihde.risteyssuhde","vaihde.tyyppi"]


layerVisibility :: Infra -> Visibility
layerVisibility = \case
  TrafficControlAreas         -> Visibility 2  3  20
  AccountingRailwaySections   -> Visibility 2  3  20
  SpeedRestrictionAreas       -> Visibility 3  6  20
  TransportationPlanningAreas -> Visibility 0  0  10
  
  PositioningMarkers          -> Visibility 5  7  20
  Milestones                  -> Visibility 5  7  20
  Railways                    -> Visibility 0  3  20
  
  TimetableLocations          -> Visibility 0  2  20
  PartsOfStation              -> Visibility 0  6  20
  LineSwitches                -> Visibility 0  2  20
  Stops                       -> Visibility 0  2  20
  Stations                    -> Visibility 0  2  20
  StationIntervals            -> Visibility 2  4  20
  
  AudioFrequencyTrackCircuits -> Visibility 2  7  20
  AxleCountingSections        -> Visibility 2  7  20
  TrackCircuits               -> Visibility 2  7  20
  AxleCounters                -> Visibility 4  7  20
  RailInsulations             -> Visibility 4  7  20
  Tracks                      -> Visibility 0  9  20
  
  Tunnels                     -> Visibility 2  6  20
  Bridges                     -> Visibility 2  6  20
  Platforms                   -> Visibility 4  7  20
  LevelCrossings              -> Visibility 2  6  20
  
  ElectricalWorkInsulators    -> Visibility 4  7  20
  GroundingDevices            -> Visibility 4  7  20
  SeparationFields            -> Visibility 4  7  20
  SeparationSections          -> Visibility 4  7  20
  ElectrificationEnds         -> Visibility 4  7  20
  GroupingInsulators          -> Visibility 4  7  20
  ElectrificationGroups       -> Visibility 4  7  20

  PantographMonitoringCameras -> Visibility 4  7  20
  RFIDReaders                 -> Visibility 4  7  20
  WheelForceIndicators        -> Visibility 4  7  20
  HotboxDetectors             -> Visibility 4  7  20

  StopBoards                  -> Visibility 4  7  20
  Derailers                   -> Visibility 4  7  20
  StationBoundaryMarks        -> Visibility 4  7  20
  Balises                     -> Visibility 4  7  20
  Signals                     -> Visibility 4  7  20
  Buffers                     -> Visibility 4  7  20
  Switches                    -> Visibility 4  7  20



data Puoli = Vasen | Oikea
  deriving (Show, Typeable, Generic, NFData, Eq)
instance FromJSON Puoli where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = fmap toLower }

data Suunta = Nouseva | Laskeva
  deriving (Show, Typeable, Generic, NFData, Eq)
instance FromJSON Suunta where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = fmap toLower }

data Kasikaantoisyys = Ei | Kokonaan | Vasen_ | Oikea_
  deriving (Show, Typeable, Generic, NFData, Eq)
instance FromJSON Kasikaantoisyys where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = takeWhile (/= '_') . fmap toLower }

data BaliisiE = BaliisiE {
  suunta :: Suunta,
  toistopiste :: Bool
} deriving (FromJSON, Show, Typeable, Generic, NFData, Eq)

data RaiteensulkuE = RaiteensulkuE {
  kasinAsetettava :: Bool
} deriving (FromJSON, Show, Typeable, Generic, NFData, Eq)

data OpastinE = OpastinE {
  puoli :: Puoli,
  suunta :: Suunta,
  tyyppi :: String
} deriving (FromJSON, Show, Typeable, Generic, NFData, Eq)

data RyhmityseristinE = RyhmityseristinE {
  nopeastiAjettava :: Bool
} deriving (FromJSON, Show, Typeable, Generic, NFData, Eq)

data VaihdeE = VaihdeE {
  kasikaantoisyys :: Kasikaantoisyys,
  risteyssuhde :: Maybe String,
  tyyppi :: String
} deriving (FromJSON, Show, Typeable, Generic, NFData, Eq)

data InfraProperties =
    TrafficControlAreasF {
    nimi_ :: String,
    tunniste_TrafficControlAreasF :: GOid TrafficControlAreas,
    vari :: String
  }
  | AccountingRailwaySectionsF {
    nimi_ :: String,
    numero :: Natural,
    tunniste_AccountingRailwaySectionsF :: GOid AccountingRailwaySections
  }
  | TransportationPlanningAreasF {
    nimi_ :: String,
    tunniste_TransportationPlanningAreasF :: GOid TransportationPlanningAreas
  }
  
  | PositioningMarkersF {
    numero :: Natural,
    tunniste_PositioningMarkersF :: GOid PositioningMarkers
  }
  | MilestonesF {
    pituus :: Natural,
    ratakm :: Natural,
    ratanumero :: String,
    tunniste_MilestonesF :: GOid Milestones
  }
  | RailwaysF {
    ratanumero :: String,
    tunniste_RailwaysF :: GOid Railways
  }
  
  | TimetableLocationsF {
    tunniste_TimetableLocationsF :: GOid TimetableLocations,
    uickoodi :: String
  }
  | PartsOfStationF {
    lyhenne :: String,
    nimi_ :: String,
    tunniste_PartsOfStationF :: GOid PartsOfStation
  }
  | LineSwitchesF {
    lyhenne :: String,
    nimi_ :: String,
    tunniste_LineSwitchesF :: GOid LineSwitches
  }
  | StopsF {
    lyhenne :: String,
    nimi_ :: String,
    tunniste_StopsF :: GOid Stops
  }
  | StationsF {
    lyhenne :: String,
    nimi_ :: String,
    tunniste_StationsF :: GOid Stations
  }
  | StationIntervalsF {
    tunniste_StationIntervalsF :: GOid StationIntervals
  }

  | AudioFrequencyTrackCircuitsF {
    tunniste_AudioFrequencyTrackCircuitsF :: GOid AudioFrequencyTrackCircuits,
    turvalaiteNimi :: String,
    turvalaiteRaide :: String
  }
  | AxleCountingSectionsF {
    tunniste_AxleCountingSectionsF :: GOid AxleCountingSections,
    turvalaiteNimi :: String,
    turvalaiteRaide :: String
  }
  | TrackCircuitsF {
    tunniste_TrackCircuitsF :: GOid TrackCircuits,
    turvalaiteNimi :: String,
    turvalaiteRaide :: String
  }
  | AxleCountersF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_AxleCountersF :: GOid AxleCounters,
    tyyppi :: String
  }
  | RailInsulationsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_RailInsulationsF :: GOid RailInsulations,
    tyyppi :: String
  }
  | TracksF {
    kuvaus :: Maybe String,
    tunnus_ :: Maybe String,
    tunniste_TracksF :: GOid Tracks
  }
  
  | TunnelsF {
    nimi :: Maybe String,
    tunniste_TunnelsF :: GOid Tunnels
  }
  | BridgesF {
    nimi :: Maybe String,
    siltakoodi :: String,
    tunniste_BridgesF :: GOid Bridges
  }
  | PlatformsF {
    kaupallinenNumero :: Natural,
    tunniste_PlatformsF :: GOid Platforms,
    tunnus :: String
  }
  | LevelCrossingsF {
    nimi :: Maybe String,
    tunniste_LevelCrossingsF :: GOid LevelCrossings,
    tunnus :: String
  }

  | ElectricalWorkInsulatorsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_ElectricalWorkInsulatorsF :: GOid ElectricalWorkInsulators,
    tyyppi :: String
  }
  | GroundingDevicesF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_GroundingDevicesF :: GOid GroundingDevices,
    tyyppi :: String
  }
  | SeparationFieldsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_SeparationFieldsF :: GOid SeparationFields,
    tyyppi :: String
  }
  | SeparationSectionsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_SeparationSectionsF :: GOid SeparationSections,
    tyyppi :: String
  }
  | ElectrificationEndsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_ElectrificationEndsF :: GOid ElectrificationEnds,
    tyyppi :: String
  }
  | GroupingInsulatorsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    ryhmityseristin :: RyhmityseristinE,
    tunniste_GroupingInsulatorsF :: GOid GroupingInsulators,
    tyyppi :: String
  }
  | ElectrificationGroupsF {
    numero_ :: String,
    tunniste_ElectrificationGroupsF :: GOid ElectrificationGroups,
    vari :: String
  }
  
  | PantographMonitoringCamerasF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_PantographMonitoringCamerasF :: GOid PantographMonitoringCameras,
    tyyppi :: String
  }
  | RFIDReadersF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_RFIDReadersF :: GOid RFIDReaders,
    tyyppi :: String
  }
  | WheelForceIndicatorsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_WheelForceIndicatorsF :: GOid WheelForceIndicators,
    tyyppi :: String
  }
  | HotboxDetectorsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_HotboxDetectorsF :: GOid HotboxDetectors,
    tyyppi :: String
  }

  | StopBoardsF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_StopBoardsF :: GOid StopBoards,
    tyyppi :: String
  }
  | DerailersF {
    nimi :: Maybe String,
    raiteensulku :: RaiteensulkuE,
    rotaatio :: Double,
    tunniste_DerailersF :: GOid Derailers,
    tyyppi :: String
  }
  | StationBoundaryMarksF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_StationBoundaryMarksF :: GOid StationBoundaryMarks,
    tyyppi :: String
  }
  | BalisesF {
    baliisi  :: BaliisiE,
    nimi     :: Maybe String,
    rotaatio :: Double,
    tunniste_BalisesF :: GOid Balises,
    tyyppi   :: String
  }
  | SignalsF {
    nimi     :: Maybe String,
    opastin  :: OpastinE,
    rotaatio :: Double,
    tunniste_SignalsF :: GOid Signals,
    tyyppi   :: String
  }
  | BuffersF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_BuffersF :: GOid Buffers,
    tyyppi :: String
  }
  | SwitchesF {
    nimi :: Maybe String,
    rotaatio :: Double,
    tunniste_SwitchesF :: GOid Switches,
    tyyppi :: String,
    vaihde :: VaihdeE
  }
  | SpeedRestrictionAreasF {
  }
 deriving (Show, Typeable, Generic, NFData, Eq)

toOid :: InfraProperties -> Maybe Oid
toOid x@TrafficControlAreasF{} = Just $ oid $ tunniste_TrafficControlAreasF x
toOid x@AccountingRailwaySectionsF{} = Just $ oid $ tunniste_AccountingRailwaySectionsF x
toOid x@SpeedRestrictionAreasF{} = Nothing
toOid x@TransportationPlanningAreasF{} = Just $ oid $ tunniste_TransportationPlanningAreasF x

toOid x@PositioningMarkersF{} = Just $ oid $ tunniste_PositioningMarkersF x
toOid x@MilestonesF{} = Just $ oid $ tunniste_MilestonesF x
toOid x@RailwaysF{} = Just $ oid $ tunniste_RailwaysF x

toOid x@TimetableLocationsF{} = Just $ oid $ tunniste_TimetableLocationsF x
toOid x@PartsOfStationF{} = Just $ oid $ tunniste_PartsOfStationF x
toOid x@LineSwitchesF{} = Just $ oid $ tunniste_LineSwitchesF x
toOid x@StopsF{} = Just $ oid $ tunniste_StopsF x
toOid x@StationsF{} = Just $ oid $ tunniste_StationsF x
toOid x@StationIntervalsF{} = Just $ oid $ tunniste_StationIntervalsF x

toOid x@AudioFrequencyTrackCircuitsF{} = Just $ oid $ tunniste_AudioFrequencyTrackCircuitsF x
toOid x@AxleCountingSectionsF{} = Just $ oid $ tunniste_AxleCountingSectionsF x
toOid x@TrackCircuitsF{} = Just $ oid $ tunniste_TrackCircuitsF x
toOid x@AxleCountersF{} = Just $ oid $ tunniste_AxleCountersF x
toOid x@RailInsulationsF{} = Just $ oid $ tunniste_RailInsulationsF x
toOid x@TracksF{} = Just $ oid $ tunniste_TracksF x

toOid x@TunnelsF{} = Just $ oid $ tunniste_TunnelsF x
toOid x@BridgesF{} = Just $ oid $ tunniste_BridgesF x
toOid x@PlatformsF{} = Just $ oid $ tunniste_PlatformsF x
toOid x@LevelCrossingsF{} = Just $ oid $ tunniste_LevelCrossingsF x

toOid x@ElectricalWorkInsulatorsF{} = Just $ oid $ tunniste_ElectricalWorkInsulatorsF x
toOid x@GroundingDevicesF{} = Just $ oid $ tunniste_GroundingDevicesF x
toOid x@SeparationFieldsF{} = Just $ oid $ tunniste_SeparationFieldsF x
toOid x@SeparationSectionsF{} = Just $ oid $ tunniste_SeparationSectionsF x
toOid x@ElectrificationEndsF{} = Just $ oid $ tunniste_ElectrificationEndsF x
toOid x@GroupingInsulatorsF{} = Just $ oid $ tunniste_GroupingInsulatorsF x
toOid x@ElectrificationGroupsF{} = Just $ oid $ tunniste_ElectrificationGroupsF x

toOid x@PantographMonitoringCamerasF{} = Just $ oid $ tunniste_PantographMonitoringCamerasF x
toOid x@RFIDReadersF{} = Just $ oid $ tunniste_RFIDReadersF x
toOid x@WheelForceIndicatorsF{} = Just $ oid $ tunniste_WheelForceIndicatorsF x
toOid x@HotboxDetectorsF{} = Just $ oid $ tunniste_HotboxDetectorsF x

toOid x@StopBoardsF{} = Just $ oid $ tunniste_StopBoardsF x
toOid x@DerailersF{} = Just $ oid $ tunniste_DerailersF x
toOid x@StationBoundaryMarksF{} = Just $ oid $ tunniste_StationBoundaryMarksF x

toOid x@BalisesF{} = Just $ oid $ tunniste_BalisesF x
toOid x@SignalsF{} = Just $ oid $ tunniste_SignalsF x
toOid x@BuffersF{} = Just $ oid $ tunniste_BuffersF x
toOid x@SwitchesF{} = Just $ oid $ tunniste_SwitchesF x

instance FromJSON InfraProperties where
  parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue, fieldLabelModifier = takeWhile (/= '_') }

rotStyle :: Double -> Props SvgXml handler
rotStyle rotaatio = style [transform [Rotate $ Deg rotaatio]]



lineStyle :: Infra -> InfraProperties -> [Props Polyline handler]
lineStyle TrafficControlAreas TrafficControlAreasF{ vari }             = [strokeWidth 5, strokeColor (parseColor vari)]
lineStyle SpeedRestrictionAreas SpeedRestrictionAreasF{  }             = [strokeWidth 1, strokeColor (Rgb 89 161 183)]
lineStyle Railways RailwaysF{  }                                       = [strokeWidth 1, strokeColor (Rgb 255 42 42)]
lineStyle StationIntervals StationIntervalsF{  }                       = [strokeWidth 3, strokeColor (Rgba 42 42 255 0.3)]
lineStyle AudioFrequencyTrackCircuits AudioFrequencyTrackCircuitsF{  } = [strokeWidth 1, strokeColor (Rgba 255 0 0 0.5)]
lineStyle AxleCountingSections AxleCountingSectionsF{  }               = [strokeWidth 1, strokeColor (Rgba 0 255 0 0.5)] 
lineStyle TrackCircuits TrackCircuitsF{  }                             = [strokeWidth 1, strokeColor (Rgba 0 0 255 0.5)]
lineStyle Tracks TracksF{  }                                           = [strokeWidth 1, strokeColor (Rgb 0 0 0)]
lineStyle Tunnels TunnelsF{  }                                         = [strokeWidth 1, strokeColor (Rgb 0 128 0)]
lineStyle Bridges BridgesF{  }                                         = [strokeWidth 1, strokeColor (Rgb 128 0 0)]
lineStyle Platforms PlatformsF{  }                                     = [strokeWidth 1, strokeColor (Rgb 0 0 128)]
lineStyle ElectrificationGroups ElectrificationGroupsF{ vari }         = [strokeWidth 3, strokeColor (parseColor vari)]
lineStyle x _ = error $ "Not implemented lineStyle: " <> show x

parseColor :: String -> Color
parseColor [r1,r2,g1,g2,b1,b2] = let [(r,_)] = readHex [r1,r2]
                                     [(g,_)] = readHex [g1,g2]
                                     [(b,_)] = readHex [b1,b2]
                                 in Rgba (read $ show r) (read $ show g) (read $ show b) 0.5

polygonStyle :: Infra -> InfraProperties -> [Props Polygon handler]
polygonStyle AccountingRailwaySections AccountingRailwaySectionsF{  }     = [strokeWidth 1, strokeColor (Rgb 0 0 128), fillColor (Rgba 0 0 128 0.1)]
polygonStyle TransportationPlanningAreas TransportationPlanningAreasF{  } = [strokeWidth 1, strokeColor (Rgb 0 128 0), fillColor (Rgba 0 128 0 0.1)]
polygonStyle Stations StationsF{  }                                       = [strokeWidth 1, strokeColor (Rgb 0 0 255), fillColor (Rgba 0 0 128 0.5)]
polygonStyle x _ = error $ "Not implemented polygonStyle: " <> show x

layerIcon :: LatLng -> Infra -> InfraProperties -> ReactElementM handler ()
layerIcon coordinate PositioningMarkers PositioningMarkersF{  } = undefined -- TODO
layerIcon coordinate Milestones MilestonesF{  }                 = svgXml [ xml "icons/kilometrimerkki" ]
layerIcon coordinate TimetableLocations TimetableLocationsF{  } = circle [ center coordinate
                                                                         , radius 5
                                                                         , strokeColor (Rgba 128 128 128 0.3)
                                                                         , fillColor (Rgba 128 128 128 0.3)
                                                                         ]
layerIcon coordinate PartsOfStation PartsOfStationF{  }         = circle [ center coordinate
                                                                         , radius 3
                                                                         , strokeColor (Rgba 42 42 255 0.6)
                                                                         , fillColor (Rgba 42 42 255 0.6)
                                                                         ]
layerIcon coordinate LineSwitches LineSwitchesF{  }             = circle [ center coordinate
                                                                         , radius 5
                                                                         , strokeColor (Rgba 255 42 42 0.3)
                                                                         , fillColor (Rgba 255 42 42 0.3)
                                                                         ]
layerIcon coordinate Stops StopsF{  }                           = circle [ center coordinate
                                                                         , radius 5
                                                                         , strokeColor (Rgba 42 255 42 0.3)
                                                                         , fillColor (Rgba 42 255 42 0.3)
                                                                         ]
layerIcon coordinate Stations StationsF{  }                     = circle [ center coordinate
                                                                         , radius 5
                                                                         , strokeColor (Rgba 42 42 255 0.3)
                                                                         , fillColor (Rgba 42 42 255 0.3)
                                                                         ]
layerIcon coordinate AxleCounters AxleCountersF{ rotaatio }                                                                            = svgXml [ xml "icons/akselinlaskija" , rotStyle rotaatio ]
layerIcon coordinate RailInsulations RailInsulationsF{ rotaatio }                                                                      = svgXml [ xml "icons/raideeristys" , rotStyle rotaatio ]
layerIcon coordinate LevelCrossings LevelCrossingsF{  }                                                                                = svgXml [ xml "icons/tasoristeys" {-TODO: , rotStyle rotaatio-} ]
layerIcon coordinate ElectricalWorkInsulators ElectricalWorkInsulatorsF{ rotaatio }                                                    = svgXml [ xml "icons/tyonaikaineneristin" , rotStyle rotaatio ]
layerIcon coordinate GroundingDevices GroundingDevicesF{ rotaatio }                                                                    = svgXml [ xml "icons/maadoitin" , rotStyle rotaatio ]
layerIcon coordinate SeparationFields SeparationFieldsF{ rotaatio }                                                                    = svgXml [ xml "icons/erotuskentta" , rotStyle rotaatio ]
layerIcon coordinate SeparationSections SeparationSectionsF{ rotaatio }                                                                = svgXml [ xml "icons/erotusjakso" , rotStyle rotaatio ]
layerIcon coordinate ElectrificationEnds ElectrificationEndsF{ rotaatio }                                                              = svgXml [ xml "icons/sahkoistyspaattyy" , rotStyle rotaatio ]
layerIcon coordinate GroupingInsulators GroupingInsulatorsF{ rotaatio, ryhmityseristin=RyhmityseristinE{ nopeastiAjettava = True } }   = svgXml [ xml "icons/ryhmityseristinNopea", rotStyle rotaatio ]
layerIcon coordinate GroupingInsulators GroupingInsulatorsF{ rotaatio, ryhmityseristin=RyhmityseristinE{ nopeastiAjettava = False } }  = svgXml [ xml "icons/ryhmityseristin", rotStyle rotaatio ]
layerIcon coordinate PantographMonitoringCameras PantographMonitoringCamerasF{ rotaatio }                                              = svgXml [ xml "icons/virroitinvalvontakamera" , rotStyle rotaatio ]
layerIcon coordinate RFIDReaders RFIDReadersF{ rotaatio }                                                                              = svgXml [ xml "icons/rfidlukija" , rotStyle rotaatio ]
layerIcon coordinate WheelForceIndicators WheelForceIndicatorsF{ rotaatio }                                                            = svgXml [ xml "icons/pyoravoimailmaisin" , rotStyle rotaatio ]
layerIcon coordinate HotboxDetectors HotboxDetectorsF{ rotaatio }                                                                      = svgXml [ xml "icons/kuumakayntiilmaisin" , rotStyle rotaatio ]
layerIcon coordinate StopBoards StopBoardsF{ rotaatio } = svgXml [ xml "icons/seislevy" , rotStyle rotaatio ]
layerIcon coordinate Derailers DerailersF{ raiteensulku=RaiteensulkuE{ kasinAsetettava=True }, rotaatio } = svgXml [ xml "icons/raiteensulkuKasin" , rotStyle rotaatio ]
layerIcon coordinate Derailers DerailersF{ raiteensulku=RaiteensulkuE{ kasinAsetettava=False }, rotaatio } = svgXml [ xml "icons/raiteensulku" , rotStyle rotaatio ]
layerIcon coordinate StationBoundaryMarks StationBoundaryMarksF{ rotaatio } = svgXml [ xml "icons/liikennepaikanraja" , rotStyle rotaatio ]
layerIcon coordinate Balises BalisesF{ baliisi=BaliisiE{ toistopiste=False }, rotaatio } = svgXml [ xml "icons/baliisi"              , rotStyle rotaatio ]
layerIcon coordinate Balises BalisesF{ baliisi=BaliisiE{ toistopiste=True } , rotaatio } = svgXml [ xml "icons/baliisitoistopiste"   , rotStyle rotaatio ]
layerIcon coordinate Signals SignalsF{ opastin=OpastinE{ puoli, tyyppi }    , rotaatio } = svgXml [ xml ("icons/opastin_" <> tyyppi) , rotStyle rotaatio, originY (case puoli of Vasen -> 5; Oikea -> -5) ]
layerIcon coordinate Buffers BuffersF{ rotaatio } = svgXml [ xml "icons/puskin" , rotStyle rotaatio ]
layerIcon coordinate Switches SwitchesF{ rotaatio, vaihde=VaihdeE{ tyyppi, kasikaantoisyys, risteyssuhde } } = svgXml [ xml ("icons/" <> vaihdeIcon tyyppi kasikaantoisyys risteyssuhde ), rotStyle rotaatio ]
layerIcon coordinate x _ = error $ "Not implemented layerIcon: " <> show x

vaihdeIcon tyyppi kasikaantoisyys risteyssuhde = "vaihde_" <>
                                              tyyppi <>
                                              (if kasikaantoisyys == Ei then "" else "_" <> printKasikaantoisyys kasikaantoisyys) <>
                                              (case risteyssuhde of
                                                  Nothing -> ""
                                                  Just rr -> if parseRisteyssuhde rr <= 10.0 then "" else if parseRisteyssuhde rr <= 18 then "_keskinopea" else "_nopea")

parseRisteyssuhde = read . fmap (\c -> if c == ',' then '.' else c) . tail . dropWhile (/= ':')

printKasikaantoisyys Ei       = ""
printKasikaantoisyys Kokonaan = "kokonaan"
printKasikaantoisyys Vasen_   = "vasen"
printKasikaantoisyys Oikea_   = "oikea"