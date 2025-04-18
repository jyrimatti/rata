{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE DuplicateRecordFields        #-}
{-# LANGUAGE PolyKinds                    #-}
{-# LANGUAGE RankNTypes                   #-}
module Maps.Types (
  module Maps.Types
) where

import Control.DeepSeq
import Data.Aeson                 (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX
import GHC.Generics               (Generic)
import GHCJS.Marshal              (FromJSVal (..), ToJSVal (..))
import Numeric.Natural            (Natural)
import Prelude                    (Bool, Double, Int, Maybe (..), Eq,
                                             Show, String,
                                             (.))
import React.Flux.Rn.Events       (fromJSON, fromNativeJSON)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data LatLng = LatLng {
    latitude :: Double,
    longitude :: Double
} deriving (Show,Generic)
instance FromJSON LatLng
instance FromJSVal LatLng where fromJSVal = fromJSON
instance ToJSON LatLng
instance ToJSVal LatLng where toJSVal = toJSVal . toJSON

data Region = Region {
  latitude :: Double,
  longitude :: Double,
  latitudeDelta :: Maybe Double,
  longitudeDelta :: Maybe Double
} deriving (Show, Generic, NFData, Eq)
instance ToJSON Region
instance ToJSVal Region where toJSVal = toJSVal . toJSON
instance FromJSON Region
instance FromJSVal Region where fromJSVal = fromJSON

data Camera = Camera {
  center :: LatLng,
  pitch :: Double,
  heading :: Double,
  altitude :: Double,
  zoom :: Natural
} deriving (Show,Generic)
instance ToJSON Camera
instance ToJSVal Camera where toJSVal = toJSVal . toJSON

data Location = Location {
  latitude :: Double,
  longitude :: Double,
  altitude :: Double,
  timestamp :: POSIXTime,
  accuracy :: Double,
  altitudeAccuracy :: Double,
  speed :: Double
} deriving (Show,Generic)
instance ToJSON Location
instance ToJSVal Location where toJSVal = toJSVal . toJSON

data Point = Point {
  x :: Int,
  y :: Int
} deriving (Show,Generic)
instance ToJSON Point
instance ToJSVal Point where toJSVal = toJSVal . toJSON
instance FromJSON Point
instance FromJSVal Point where fromJSVal = fromJSON

data Frame = Frame {
  x :: Int,
  y :: Int,
  width :: Int,
  height :: Int
} deriving (Show,Generic)
instance ToJSON Frame
instance ToJSVal Frame where toJSVal = toJSVal . toJSON

data MapType = Standard | Satellite | Hybrid | Terrain | None | MutedStandard
  deriving (Show, Generic)
instance ToJSVal MapType where
    toJSVal Standard      = toJSVal "standard"
    toJSVal Satellite     = toJSVal "satellite"
    toJSVal Hybrid        = toJSVal "hybrid"
    toJSVal Terrain       = toJSVal "terrain"
    toJSVal None          = toJSVal "none"
    toJSVal MutedStandard = toJSVal "mutedStandard"

data PaddingAdjustmentBehavior = Always | Automatic | Never
  deriving (Show, Generic)
instance ToJSVal PaddingAdjustmentBehavior where
    toJSVal Always    = toJSVal "always"
    toJSVal Automatic = toJSVal "automatic"
    toJSVal Never     = toJSVal "never"

data KmlMarker = KmlMarker {
  id :: String,
  coordinate :: LatLng,
  title :: String,
  description :: String
} deriving (Show,Generic)
instance ToJSON KmlMarker
instance ToJSVal KmlMarker where toJSVal = toJSVal . toJSON

newtype KmlContainer = KmlContainer {
  markers :: [KmlMarker]
} deriving (Show,Generic)
instance ToJSON KmlContainer
instance ToJSVal KmlContainer where toJSVal = toJSVal . toJSON

data IndoorBuilding = IndoorBuilding {
  underground :: Bool,
  activeLevelIndex :: Natural,
  levels :: [IndoorLevel]
} deriving (Show,Generic)
instance ToJSON IndoorBuilding
instance ToJSVal IndoorBuilding where toJSVal = toJSVal . toJSON

data IndoorLevel = IndoorLevel {
  activeLevelIndex :: Natural,
  name :: String,
  shortName :: String
} deriving (Show,Generic)
instance ToJSON IndoorLevel
instance ToJSVal IndoorLevel where toJSVal = toJSVal . toJSON

data OnPress = OnPress {
  coordinate :: LatLng,
  position :: Point
} deriving (Show, Generic)
instance FromJSON OnPress
instance FromJSVal OnPress where fromJSVal = fromNativeJSON
