{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module LayerTypes where

import Control.DeepSeq
import Data.Typeable
import GHC.Generics                   ( Generic )

data Infra =
    TransportationPlanningAreas | SpeedRestrictionAreas | AccountingRailwaySections |
    Railways | Milestones | PositioningMarkers |
    Tracks |
    TimetableLocations | PartsOfStation | StationIntervals | Stations | Stops | LineSwitches |
    TrackCircuits | AxleCountingSections | AudioFrequencyTrackCircuits |
    LevelCrossings | Bridges | Tunnels | Platforms |
    Balises | Signals | Buffers | Switches | Derailers | AxleCounters | ElectrificationEnds | RailInsulations | GroupingInsulators | StopBoards | StationBoundaryMarks | PantographMonitoringCameras | RFIDReaders | HotboxDetectors | WheelForceIndicators |
    ElectrificationGroups | SeparationSections | SeparationFields
  deriving (Show, Typeable, Generic, NFData, Eq, Ord)

data LayerType = LayerType Infra
  deriving (Show, Typeable, Generic, NFData, Eq, Ord)
