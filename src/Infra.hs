{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Infra where

import           Data.Maybe

import           Layer
import           LayerTypes

apiBase = "https://rata.digitraffic.fi/infra-api/0.3/"

allLayers =
  [ Layer (LayerType TransportationPlanningAreas) 0  3  10
  , Layer (LayerType SpeedRestrictionAreas)       5  5  17
  , Layer (LayerType AccountingRailwaySections)   5  7  15
  , Layer (LayerType Railways)                    5  7  20
  , Layer (LayerType Milestones)                  7  10 20
  , Layer (LayerType PositioningMarkers)          7  10 20
  , Layer (LayerType Tracks)                      7  10 20
  , Layer (LayerType TimetableLocations)          7  10 20
  , Layer (LayerType PartsOfStation)              7  10 20
  , Layer (LayerType StationIntervals)            7  10 20
  , Layer (LayerType Stations)                    7  12 20
  , Layer (LayerType Stops)                       7  12 20
  , Layer (LayerType LineSwitches)                7  12 20
  , Layer (LayerType TrackCircuits)               7  12 20
  , Layer (LayerType AxleCountingSections)        7  12 20
  , Layer (LayerType AudioFrequencyTrackCircuits) 7  12 20
  , Layer (LayerType LevelCrossings)              10 15 20
  , Layer (LayerType Bridges)                     7  10 20
  , Layer (LayerType Tunnels)                     7  10 20
  , Layer (LayerType Platforms)                   7  10 20
  , Layer (LayerType Balises)                     10 15 20
  , Layer (LayerType Signals)                     10 15 20
  , Layer (LayerType Buffers)                     10 15 20
  , Layer (LayerType Switches)                    10 15 20
  , Layer (LayerType Derailers)                   10 15 20
  , Layer (LayerType AxleCounters)                10 15 20
  , Layer (LayerType ElectrificationEnds)         10 15 20
  , Layer (LayerType RailInsulations)             10 15 20
  , Layer (LayerType GroupingInsulators)          10 15 20
  , Layer (LayerType StopBoards)                  10 15 20
  , Layer (LayerType StationBoundaryMarks)        10 15 20
  , Layer (LayerType PantographMonitoringCameras) 10 15 20
  , Layer (LayerType RFIDReaders)                 10 15 20
  , Layer (LayerType HotboxDetectors)             10 15 20
  , Layer (LayerType WheelForceIndicators)        10 15 20
  , Layer (LayerType ElectrificationGroups)       10 15 20
  , Layer (LayerType SeparationSections)          10 15 20
  , Layer (LayerType SeparationFields)            10 15 20
  ]

layerPath TransportationPlanningAreas =
  ("liikennesuunnittelualueet", Nothing, [])
layerPath SpeedRestrictionAreas     = ("nopeusrajoitusalueet", Nothing, [])
layerPath AccountingRailwaySections = ("tilirataosat", Nothing, [])
layerPath Railways                  = ("radat", Nothing, [])
layerPath Milestones                = ("kilometrimerkit", Nothing, [])
layerPath PositioningMarkers        = ("paikantamismerkit", Nothing, [])
layerPath Tracks                    = ("raiteet", Nothing, [])
layerPath TimetableLocations        = ("aikataulupaikat", Nothing, [])
layerPath PartsOfStation            = ("liikennepaikanosat", Nothing, [])
layerPath StationIntervals          = ("liikennepaikkavalit", Nothing, [])
layerPath Stations = ("rautatieliikennepaikat", Just "liikennepaikka", [])
layerPath Stops = ("rautatieliikennepaikat", Just "seisake", [])
layerPath LineSwitches = ("rautatieliikennepaikat", Just "linjavaihde", [])
layerPath TrackCircuits             = ("raideosuudet", Just "eristysosuus", [])
layerPath AxleCountingSections =
  ("raideosuudet", Just "akselinlaskentaosuus", [])
layerPath AudioFrequencyTrackCircuits =
  ("raideosuudet", Just "raidevirtapiiri", [])
layerPath LevelCrossings       = ("tasoristeykset", Nothing, [])
layerPath Bridges              = ("sillat", Nothing, [])
layerPath Tunnels              = ("tunnelit", Nothing, [])
layerPath Platforms            = ("laiturit", Nothing, [])
layerPath Balises              = ("elementit", Just "baliisi", [])
layerPath Signals              = ("elementit", Just "opastin", [])
layerPath Buffers              = ("elementit", Just "puskin", [])
layerPath Switches             = ("elementit", Just "vaihde", [])
layerPath Derailers            = ("elementit", Just "raiteensulku", [])
layerPath AxleCounters         = ("elementit", Just "akselinlaskija", [])
layerPath ElectrificationEnds  = ("elementit", Just "sahkoistyspaattyy", [])
layerPath RailInsulations      = ("elementit", Just "raideeristys", [])
layerPath GroupingInsulators   = ("elementit", Just "ryhmityseristin", [])
layerPath StopBoards           = ("elementit", Just "seislevy", [])
layerPath StationBoundaryMarks = ("elementit", Just "liikennepaikanraja", [])
layerPath PantographMonitoringCameras =
  ("elementit", Just "virroitinvalvontakamera", [])
layerPath RFIDReaders           = ("elementit", Just "rfidlukija", [])
layerPath HotboxDetectors       = ("elementit", Just "kuumakayntiilmaisin", [])
layerPath WheelForceIndicators  = ("elementit", Just "pyoravoimailmaisin", [])
layerPath ElectrificationGroups = ("kytkentaryhmat", Nothing, [])
layerPath SeparationSections    = ("elementit", Just "erotusjaksot", [])
layerPath SeparationFields      = ("elementit", Just "erotuskentat", [])
