{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Layer where

import Control.DeepSeq
import Data.Typeable                  ( Typeable )
import GHC.Generics                   ( Generic )
import LayerTypes

import Numeric.Natural

data LayerState = LayerHidden | WMTS | Vector
  deriving (Show, Typeable, Generic, NFData, Eq)

data Layer = Layer {
    layerType :: LayerType,
    minZoom :: Natural,
    limitZoom :: Natural,
    maxZoom :: Natural
} deriving (Typeable, Generic, NFData, Eq, Ord)

instance Show Layer where
  show = show . layerType

layerName :: Layer -> String
layerName = show . layerType

wmtsSuffix = "/{z}/{y}/{x}.png?propertyName=geometria"
vectorSuffix propertyName =
  ".geojson?propertyName=" <> propertyName <> "&srsName=epsg:4326"

wmtsUrl baseURL (layerPath, typename, _) =
  baseURL <> layerPath <> wmtsSuffix <> typeNames typename

vectorUrl baseURL (layerPath, typename, propertyName) =
  baseURL <> layerPath <> vectorSuffix propertyName <> typeNames typename

typeNames :: Maybe String -> String
typeNames Nothing  = ""
typeNames (Just x) = "&typeNames=" <> x
