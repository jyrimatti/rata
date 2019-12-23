{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Layer where

import Data.Aeson
import Control.DeepSeq
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Typeable                  ( Typeable )
import GHC.Generics                   ( Generic )
import LayerTypes
import Numeric.Natural
import Prelude (($), String, Show(..), Maybe(..), Eq, Ord, Bool(..), (.), (<>), (==), (>), (<), (>=), fmap)
import Transform
import Infra
import Maps.Polyline (Polyline)
import Maps.Polygon (Polygon)
import           React.Flux.Rn.Views (ReactElementM)
import           React.Flux.Rn.Properties (Props)
import Maps.Types (LatLng) 

data Layer = Layer {
    layerType :: LayerType,
    visibility :: Visibility
} deriving (Typeable, Generic, NFData, Eq, Ord)

instance Show Layer where
  show = show . layerType

data FeatureProperties = InfraProps InfraProperties
  deriving (FromJSON, Show, Typeable, Generic, NFData, Eq)

data LayerType = InfraLayer Infra
  deriving (Show, Typeable, Generic, NFData, Eq, Ord)

layerPath :: Layer -> LayerSource
layerPath Layer{layerType = InfraLayer layer} = Infra.layerPath layer

toOid :: FeatureProperties -> Maybe Oid
toOid (InfraProps props) = Infra.toOid props

layerIcon :: LatLng -> Layer -> FeatureProperties -> ReactElementM handler ()
layerIcon coordinate Layer{layerType = InfraLayer layer} (InfraProps props) = Infra.layerIcon coordinate layer props

lineStyle :: Layer -> FeatureProperties -> [Props Polyline handler]
lineStyle Layer{layerType = InfraLayer layer} (InfraProps props) = Infra.lineStyle layer props

polygonStyle :: Layer -> FeatureProperties -> [Props Polygon handler]
polygonStyle Layer{layerType = InfraLayer layer} (InfraProps props) = Infra.polygonStyle layer props

layerBase :: Layer -> String
layerBase Layer{layerType = InfraLayer _} = Infra.apiBase

allLayers :: [Layer]
allLayers = fmap (\x -> Layer (InfraLayer x) (layerVisibility x))
  [ TrafficControlAreas
  , AccountingRailwaySections
  , SpeedRestrictionAreas
  , TransportationPlanningAreas
  
  , PositioningMarkers
  , Milestones
  , Railways
  
  , TimetableLocations
  , PartsOfStation
  , LineSwitches
  , Stops
  , Stations
  , StationIntervals
  
  , AudioFrequencyTrackCircuits
  , AxleCountingSections
  , TrackCircuits
  , AxleCounters
  , RailInsulations
  , Tracks
  
  , Tunnels
  , Bridges
  , Platforms
  , LevelCrossings
  
  , ElectricalWorkInsulators
  , GroundingDevices
  , SeparationFields
  , SeparationSections
  , ElectrificationEnds
  , GroupingInsulators
  , ElectrificationGroups

  , PantographMonitoringCameras
  , RFIDReaders
  , WheelForceIndicators
  , HotboxDetectors

  , StopBoards
  , Derailers
  , StationBoundaryMarks
  , Balises
  , Signals
  , Buffers
  , Switches
  ]

layerName :: Layer -> LayerState -> String
layerName Layer{layerType = InfraLayer infra} _ = show infra

wmtsSuffix :: String
wmtsSuffix = "/MERCATOR/{z}/{y}/{x}.png"

vectorSuffix :: String
vectorSuffix = ".geojson" 

wmtsUrl :: String -> LayerSource -> Bool -> String
wmtsUrl baseURL LayerSource{path, typeNames} diagram =
  baseURL <> path <> wmtsSuffix <> case intercalate "&" (catMaybes [formatPresentation diagram, formatTypeNames typeNames]) of
    "" -> ""
    x  -> "?" <> x

vectorUrl :: String -> LayerSource -> BBox -> Bool -> String
vectorUrl baseURL LayerSource{path, typeNames, propertyName} bbox diagram =
  baseURL <> path <> vectorSuffix <> case intercalate "&" (catMaybes [Just ("bbox=" <> bbox2string bbox), formatPresentation diagram, formatPropertyName propertyName, Just "srsName=epsg:4326", formatTypeNames typeNames]) of
    "" -> ""
    x  -> "?" <> x

formatPropertyName :: [String] -> Maybe String
formatPropertyName [] = Nothing
formatPropertyName xs = Just $ "propertyName=" <> intercalate "," xs

formatPresentation :: Bool -> Maybe String
formatPresentation True = Just "presentation=diagram"
formatPresentation False = Nothing

formatTypeNames :: Maybe String -> Maybe String
formatTypeNames = fmap ("typeNames=" <>)

wmtsVisible :: Natural -> Layer -> LayerState -> Bool
wmtsVisible _ _ LayerHidden                                                                 = False
wmtsVisible zoomLevel Layer{visibility = Visibility {minZoom}}   _ | zoomLevel < minZoom    = False
wmtsVisible zoomLevel Layer{visibility = Visibility {limitZoom}} _ | zoomLevel >= limitZoom = False
wmtsVisible _ _ _                                                                           = True

vectorVisible :: Natural -> Layer -> LayerState -> Bool
vectorVisible _ _ LayerHidden                                                                = False
vectorVisible zoomLevel Layer{visibility = Visibility {limitZoom}} _ | zoomLevel < limitZoom = False
vectorVisible zoomLevel Layer{visibility = Visibility {maxZoom}}   _ | zoomLevel > maxZoom   = False
vectorVisible _ _ _                                                                          = True
