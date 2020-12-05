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
