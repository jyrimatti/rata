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
import InfraStyles
import InfraData
import Maps.Polyline (Polyline)
import Maps.Polygon (Polygon)
import           React.Flux.Rn.Views (ReactElementM)
import           React.Flux.Rn.Properties (Props)
import Maps.Types (LatLng) 

data Layer = Layer {
    layerType :: LayerType,
    visibility :: Visibility
} deriving (Typeable, Eq, NFData, Generic, Ord)

instance Show Layer where
  show = show . layerType

newtype FeatureProperties = InfraProps InfraProperties
  deriving (Show, Eq, NFData, Generic)

newtype LayerType = InfraLayer { infraLayerType :: Infra }
  deriving (Show, Eq, NFData, Generic, Ord)

layerPath :: Layer -> LayerSource
layerPath Layer{layerType = InfraLayer layer} = Infra.layerPath layer

toOid :: FeatureProperties -> Maybe Oid
toOid (InfraProps props) = InfraData.toOid props

layerIcon :: LatLng -> FeatureProperties -> ReactElementM handler ()
layerIcon coordinate (InfraProps props) = InfraStyles.layerIcon coordinate props

lineStyle :: FeatureProperties -> [Props Polyline handler]
lineStyle (InfraProps props) = InfraStyles.lineStyle props

polygonStyle :: FeatureProperties -> [Props Polygon handler]
polygonStyle (InfraProps props) = InfraStyles.polygonStyle props

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
