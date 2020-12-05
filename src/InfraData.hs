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
module InfraData where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Char (toLower)
import           Data.Maybe
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Tuple (fst)
import           Data.Typeable
import           Data.Typeable

import           GHC.Generics                   ( Generic )
import           GHC.TypeLits
import           Infra
import           LayerTypes
import           Maps.Circle
import           Maps.Polygon
import           Maps.Polyline
import           Numeric (readHex)
import           Numeric.Natural
import           Prelude (Int, (<=), dropWhile, error, mempty, read, (.), (/=), (==), String, Maybe, Bool(..), ($), Double, Show, Eq, Ord, filter, fmap, pure, fail, show, tail, takeWhile, undefined)
import           React.Flux.Rn.Properties (Props)
import           React.Flux.Rn.Views
import           Svg.SvgXml

data BaliisiE = BaliisiE {
  suunta      :: Suunta,
  toistopiste :: Bool
} deriving (Show, NFData, Generic, Eq, FromJSON)

data RaiteensulkuE = RaiteensulkuE {
  kasinAsetettava :: Bool
} deriving (Show, NFData, Generic, Eq, FromJSON)

data OpastinE = OpastinE {
  puoli  :: Puoli,
  suunta :: Suunta,
  tyyppi :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data RyhmityseristinE = RyhmityseristinE {
  nopeastiAjettava :: Bool
} deriving (Show, NFData, Generic, Eq, FromJSON)

data VaihdeE = VaihdeE {
  kasikaantoisyys :: Kasikaantoisyys,
  risteyssuhde    :: Maybe String,
  tyyppi          :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)


data TrafficControlAreasData = TrafficControlAreasData {
  nimi     :: String,
  tunniste :: Oid,
  vari     :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data AccountingRailwaySectionsData = AccountingRailwaySectionsData {
  nimi     :: String,
  numero   :: Natural,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data SpeedRestrictionAreasData = SpeedRestrictionAreasData {
} deriving (Show, NFData, Generic, Eq, FromJSON)

data TransportationPlanningAreasData = TransportationPlanningAreasData {
  nimi     :: String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data PositioningMarkersData = PositioningMarkersData {
  numero   :: Natural,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data MilestonesData = MilestonesData {
  pituus     :: Natural,
  ratakm     :: Natural,
  ratanumero :: String,
  tunniste   :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data RailwaysData = RailwaysData {
  ratanumero :: String,
  tunniste   :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data TimetableLocationsData = TimetableLocationsData {
  tunniste :: Oid,
  uickoodi :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data PartsOfStationData = PartsOfStationData {
  lyhenne  :: String,
  nimi     :: String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data LineSwitchesData = LineSwitchesData {
  lyhenne  :: String,
  nimi     :: String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data StopsData = StopsData {
  lyhenne  :: String,
  nimi     :: String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data StationsData = StationsData {
  lyhenne  :: String,
  nimi     :: String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data StationIntervalsData = StationIntervalsData {
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data AudioFrequencyTrackCircuitsData = AudioFrequencyTrackCircuitsData {
  tunniste        :: Oid,
  turvalaiteNimi  :: String,
  turvalaiteRaide :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data AxleCountingSectionsData = AxleCountingSectionsData {
  tunniste        :: Oid,
  turvalaiteNimi  :: String,
  turvalaiteRaide :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data TrackCircuitsData = TrackCircuitsData {
  tunniste        :: Oid,
  turvalaiteNimi  :: String,
  turvalaiteRaide :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data AxleCountersData = AxleCountersData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data RailInsulationsData = RailInsulationsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data TracksData = TracksData {
  kuvaus   :: Maybe String,
  tunnus   :: Maybe String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data TunnelsData = TunnelsData {
  nimi     :: Maybe String,
  tunniste :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data BridgesData = BridgesData {
  nimi       :: Maybe String,
  siltakoodi :: String,
  tunniste   :: Oid
} deriving (Show, NFData, Generic, Eq, FromJSON)

data PlatformsData = PlatformsData {
  kaupallinenNumero :: Natural,
  tunniste          :: Oid,
  tunnus            :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data LevelCrossingsData = LevelCrossingsData {
  nimi     :: Maybe String,
  tunniste :: Oid,
  tunnus   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data ElectricalWorkInsulatorsData = ElectricalWorkInsulatorsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data GroundingDevicesData = GroundingDevicesData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data SeparationFieldsData = SeparationFieldsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data SeparationSectionsData = SeparationSectionsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data ElectrificationEndsData = ElectrificationEndsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data GroupingInsulatorsData = GroupingInsulatorsData {
  nimi            :: Maybe String,
  rotaatio        :: Double,
  ryhmityseristin :: RyhmityseristinE,
  tunniste        :: Oid,
  tyyppi          :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data ElectrificationGroupsData = ElectrificationGroupsData {
  numero   :: String,
  tunniste :: Oid,
  vari     :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data PantographMonitoringCamerasData = PantographMonitoringCamerasData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data RFIDReadersData = RFIDReadersData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data WheelForceIndicatorsData = WheelForceIndicatorsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data HotboxDetectorsData = HotboxDetectorsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data StopBoardsData = StopBoardsData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data DerailersData = DerailersData {
  nimi         :: Maybe String,
  raiteensulku :: RaiteensulkuE,
  rotaatio     :: Double,
  tunniste     :: Oid,
  tyyppi       :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data StationBoundaryMarksData = StationBoundaryMarksData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data SignalsData = SignalsData {
  nimi     :: Maybe String,
  opastin  :: OpastinE,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data BuffersData = BuffersData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data BalisesData = BalisesData {
  baliisi  :: BaliisiE,
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String
} deriving (Show, NFData, Generic, Eq, FromJSON)

data SwitchesData = SwitchesData {
  nimi     :: Maybe String,
  rotaatio :: Double,
  tunniste :: Oid,
  tyyppi   :: String,
  vaihde   :: VaihdeE
} deriving (Show, NFData, Generic, Eq, FromJSON)

data InfraProperties =
   TrafficControlAreasF TrafficControlAreasData
 | AccountingRailwaySectionsF AccountingRailwaySectionsData
 | SpeedRestrictionAreasF SpeedRestrictionAreasData
 | TransportationPlanningAreasF TransportationPlanningAreasData
 | PositioningMarkersF PositioningMarkersData
 | MilestonesF MilestonesData
 | RailwaysF RailwaysData
 | TimetableLocationsF TimetableLocationsData
 | PartsOfStationF PartsOfStationData
 | LineSwitchesF LineSwitchesData
 | StopsF StopsData
 | StationsF StationsData
 | StationIntervalsF StationIntervalsData
 | AudioFrequencyTrackCircuitsF AudioFrequencyTrackCircuitsData
 | AxleCountingSectionsF AxleCountingSectionsData
 | TrackCircuitsF TrackCircuitsData
 | AxleCountersF AxleCountersData
 | RailInsulationsF RailInsulationsData
 | TracksF TracksData
 | TunnelsF TunnelsData
 | BridgesF BridgesData
 | PlatformsF PlatformsData
 | LevelCrossingsF LevelCrossingsData
 | ElectricalWorkInsulatorsF ElectricalWorkInsulatorsData
 | GroundingDevicesF GroundingDevicesData
 | SeparationFieldsF SeparationFieldsData
 | SeparationSectionsF SeparationSectionsData
 | ElectrificationEndsF ElectrificationEndsData
 | GroupingInsulatorsF GroupingInsulatorsData
 | ElectrificationGroupsF ElectrificationGroupsData
 | PantographMonitoringCamerasF PantographMonitoringCamerasData
 | RFIDReadersF RFIDReadersData
 | WheelForceIndicatorsF WheelForceIndicatorsData
 | HotboxDetectorsF HotboxDetectorsData
 | StopBoardsF StopBoardsData
 | DerailersF DerailersData
 | StationBoundaryMarksF StationBoundaryMarksData
 | SignalsF SignalsData
 | BuffersF BuffersData
 | BalisesF BalisesData
 | SwitchesF SwitchesData
  deriving (Show, Eq, NFData, Generic)

toOid :: InfraProperties -> Maybe Oid
toOid (TrafficControlAreasF TrafficControlAreasData{tunniste})                 = Just tunniste
toOid (AccountingRailwaySectionsF AccountingRailwaySectionsData{tunniste})     = Just tunniste
toOid (SpeedRestrictionAreasF _)                                               = Nothing
toOid (TransportationPlanningAreasF TransportationPlanningAreasData{tunniste}) = Just tunniste

toOid (PositioningMarkersF PositioningMarkersData{tunniste})                   = Just tunniste
toOid (MilestonesF MilestonesData{tunniste})                                   = Just tunniste
toOid (RailwaysF RailwaysData{tunniste})                                       = Just tunniste

toOid (TimetableLocationsF TimetableLocationsData{tunniste})                   = Just tunniste
toOid (PartsOfStationF PartsOfStationData{tunniste})                           = Just tunniste
toOid (LineSwitchesF LineSwitchesData{tunniste})                               = Just tunniste
toOid (StopsF StopsData{tunniste})                                             = Just tunniste
toOid (StationsF StationsData{tunniste})                                       = Just tunniste
toOid (StationIntervalsF StationIntervalsData{tunniste})                       = Just tunniste

toOid (AudioFrequencyTrackCircuitsF AudioFrequencyTrackCircuitsData{tunniste}) = Just tunniste
toOid (AxleCountingSectionsF AxleCountingSectionsData{tunniste})               = Just tunniste
toOid (TrackCircuitsF TrackCircuitsData{tunniste})                             = Just tunniste
toOid (AxleCountersF AxleCountersData{tunniste})                               = Just tunniste
toOid (RailInsulationsF RailInsulationsData{tunniste})                         = Just tunniste
toOid (TracksF TracksData{tunniste})                                           = Just tunniste

toOid (TunnelsF TunnelsData{tunniste})                                         = Just tunniste
toOid (BridgesF BridgesData{tunniste})                                         = Just tunniste
toOid (PlatformsF PlatformsData{tunniste})                                     = Just tunniste
toOid (LevelCrossingsF LevelCrossingsData{tunniste})                           = Just tunniste

toOid (ElectricalWorkInsulatorsF ElectricalWorkInsulatorsData{tunniste})       = Just tunniste
toOid (GroundingDevicesF GroundingDevicesData{tunniste})                       = Just tunniste
toOid (SeparationFieldsF SeparationFieldsData{tunniste})                       = Just tunniste
toOid (SeparationSectionsF SeparationSectionsData{tunniste})                   = Just tunniste
toOid (ElectrificationEndsF ElectrificationEndsData{tunniste})                 = Just tunniste
toOid (GroupingInsulatorsF GroupingInsulatorsData{tunniste})                   = Just tunniste
toOid (ElectrificationGroupsF ElectrificationGroupsData{tunniste})             = Just tunniste

toOid (PantographMonitoringCamerasF PantographMonitoringCamerasData{tunniste}) = Just tunniste
toOid (RFIDReadersF RFIDReadersData{tunniste})                                 = Just tunniste
toOid (WheelForceIndicatorsF WheelForceIndicatorsData{tunniste})               = Just tunniste
toOid (HotboxDetectorsF HotboxDetectorsData{tunniste})                         = Just tunniste

toOid (StopBoardsF StopBoardsData{tunniste})                                   = Just tunniste
toOid (DerailersF DerailersData{tunniste})                                     = Just tunniste
toOid (StationBoundaryMarksF StationBoundaryMarksData{tunniste})               = Just tunniste

toOid (BalisesF BalisesData{tunniste})                                         = Just tunniste
toOid (SignalsF SignalsData{tunniste})                                         = Just tunniste
toOid (BuffersF BuffersData{tunniste})                                         = Just tunniste
toOid (SwitchesF SwitchesData{tunniste})                                       = Just tunniste
