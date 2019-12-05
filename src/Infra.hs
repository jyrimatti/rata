
{-# LANGUAGE NoImplicitPrelude #-}
module Infra where

import Data.Maybe

import Layer
import LayerTypes
import Prelude (String, Maybe)

apiBase :: String
apiBase = "https://rata.digitraffic.fi/infra-api/0.3/"

allLayers :: [Layer]
allLayers =
  [ Layer (LayerType TransportationPlanningAreas) 0  0  10
  , Layer (LayerType SpeedRestrictionAreas)       3  6  20
  , Layer (LayerType AccountingRailwaySections)   2  3  20
  , Layer (LayerType Railways)                    0  3  20
  , Layer (LayerType Milestones)                  5  7  20
  , Layer (LayerType PositioningMarkers)          5  7  20
  , Layer (LayerType Tracks)                      0  9  20
  , Layer (LayerType TimetableLocations)          0  2  20
  , Layer (LayerType PartsOfStation)              0  6  20
  , Layer (LayerType StationIntervals)            2  4  20
  , Layer (LayerType Stations)                    0  2  20
  , Layer (LayerType Stops)                       0  2  20
  , Layer (LayerType LineSwitches)                0  2  20
  , Layer (LayerType TrackCircuits)               2  7  20
  , Layer (LayerType AxleCountingSections)        2  7  20
  , Layer (LayerType AudioFrequencyTrackCircuits) 2  7  20
  , Layer (LayerType LevelCrossings)              2  6  20
  , Layer (LayerType Bridges)                     2  6  20
  , Layer (LayerType Tunnels)                     2  6  20
  , Layer (LayerType Platforms)                   4  7  20
  , Layer (LayerType Balises)                     4  7  20
  , Layer (LayerType Signals)                     4  7  20
  , Layer (LayerType Buffers)                     4  7  20
  , Layer (LayerType Switches)                    4  7  20
  , Layer (LayerType Derailers)                   4  7  20
  , Layer (LayerType AxleCounters)                4  7  20
  , Layer (LayerType ElectrificationEnds)         4  7  20
  , Layer (LayerType RailInsulations)             4  7  20
  , Layer (LayerType GroupingInsulators)          4  7  20
  , Layer (LayerType StopBoards)                  4  7  20
  , Layer (LayerType StationBoundaryMarks)        4  7  20
  , Layer (LayerType PantographMonitoringCameras) 4  7  20
  , Layer (LayerType RFIDReaders)                 4  7  20
  , Layer (LayerType HotboxDetectors)             4  7  20
  , Layer (LayerType WheelForceIndicators)        4  7  20
  , Layer (LayerType ElectrificationGroups)       4  7  20
  , Layer (LayerType SeparationSections)          4  7  20
  , Layer (LayerType SeparationFields)            4  7  20
  ]

layerPath :: Infra -> LayerSource
layerPath TransportationPlanningAreas = ("liikennesuunnittelualueet", Nothing, [])
layerPath SpeedRestrictionAreas       = ("nopeusrajoitusalueet", Nothing, [])
layerPath AccountingRailwaySections   = ("tilirataosat", Nothing, [])
layerPath Railways                    = ("radat", Nothing, [])
layerPath Milestones                  = ("kilometrimerkit", Nothing, [])
layerPath PositioningMarkers          = ("paikantamismerkit", Nothing, [])
layerPath Tracks                      = ("raiteet", Nothing, [])
layerPath TimetableLocations          = ("aikataulupaikat", Nothing, [])
layerPath PartsOfStation              = ("liikennepaikanosat", Nothing, [])
layerPath StationIntervals            = ("liikennepaikkavalit", Nothing, [])
layerPath Stations                    = ("rautatieliikennepaikat", Just "liikennepaikka", [])
layerPath Stops                       = ("rautatieliikennepaikat", Just "seisake", [])
layerPath LineSwitches                = ("rautatieliikennepaikat", Just "linjavaihde", [])
layerPath TrackCircuits               = ("raideosuudet", Just "eristysosuus", [])
layerPath AxleCountingSections        = ("raideosuudet", Just "akselinlaskentaosuus", [])
layerPath AudioFrequencyTrackCircuits = ("raideosuudet", Just "raidevirtapiiri", [])
layerPath LevelCrossings              = ("tasoristeykset", Nothing, [])
layerPath Bridges                     = ("sillat", Nothing, [])
layerPath Tunnels                     = ("tunnelit", Nothing, [])
layerPath Platforms                   = ("laiturit", Nothing, [])
layerPath Balises                     = ("elementit", Just "baliisi", [])
layerPath Signals                     = ("elementit", Just "opastin", [])
layerPath Buffers                     = ("elementit", Just "puskin", [])
layerPath Switches                    = ("elementit", Just "vaihde", [])
layerPath Derailers                   = ("elementit", Just "raiteensulku", [])
layerPath AxleCounters                = ("elementit", Just "akselinlaskija", [])
layerPath ElectrificationEnds         = ("elementit", Just "sahkoistyspaattyy", [])
layerPath RailInsulations             = ("elementit", Just "raideeristys", [])
layerPath GroupingInsulators          = ("elementit", Just "ryhmityseristin", [])
layerPath StopBoards                  = ("elementit", Just "seislevy", [])
layerPath StationBoundaryMarks        = ("elementit", Just "liikennepaikanraja", [])
layerPath PantographMonitoringCameras = ("elementit", Just "virroitinvalvontakamera", [])
layerPath RFIDReaders                 = ("elementit", Just "rfidlukija", [])
layerPath HotboxDetectors             = ("elementit", Just "kuumakayntiilmaisin", [])
layerPath WheelForceIndicators        = ("elementit", Just "pyoravoimailmaisin", [])
layerPath ElectrificationGroups       = ("kytkentaryhmat", Nothing, [])
layerPath SeparationSections          = ("elementit", Just "erotusjaksot", [])
layerPath SeparationFields            = ("elementit", Just "erotuskentat", [])

