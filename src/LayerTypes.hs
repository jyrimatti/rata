{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ExistentialQuantification        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE QuasiQuotes                      #-}
module LayerTypes where

import Data.Aeson
import           Control.DeepSeq
import           Data.Typeable
import           GHC.Generics                   ( Generic )
import Data.Geospatial
import Numeric.Natural


data LayerSource = LayerSource {
  path :: String,
  typeNames :: Maybe String,
  propertyName :: [String]
} deriving (Typeable, Generic, NFData, Eq, Ord)

data Visibility = Visibility {
  minZoom   :: Natural,
  limitZoom :: Natural,
  maxZoom   :: Natural
} deriving (Typeable, Generic, NFData, Eq, Ord)

data LayerState = LayerHidden | LayerShown
  deriving (Show, Typeable, Generic, NFData, Eq)

data Oid = Oid { getOid :: String }
  deriving (FromJSON, Show, Typeable, Generic, NFData, Eq, Ord)

data Feature props = Feature (Maybe Oid) props GeospatialGeometry
  deriving (Show, Typeable, Generic, NFData, Eq)
