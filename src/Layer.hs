{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
module Layer where

import Control.DeepSeq
import Data.Typeable                  ( Typeable )
import GHC.Generics                   ( Generic )
import LayerTypes

import Numeric.Natural

data LayerState = LayerHidden | WMTS | Vector | VectorFetching
  deriving (Show, Typeable, Generic, NFData, Eq)

data Layer = Layer {
    layerType :: LayerType,
    minZoom :: Natural,
    limitZoom :: Natural,
    maxZoom :: Natural
} deriving (Typeable, Generic, NFData, Eq, Ord)

instance Show Layer where
  show = show . layerType

layerName :: Layer -> LayerState -> String
layerName layer state = show (layerType layer) <> case state of VectorFetching -> "!"; _ -> ""

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

wmtsVisible _ _ LayerHidden                                         = False
wmtsVisible zoomLevel (Layer _ minZoom _ _) _ | zoomLevel < minZoom = False
wmtsVisible zoomLevel (Layer _ _ _ maxZoom) _ | zoomLevel > maxZoom = False
wmtsVisible a b c                                                   = not $ vectorVisible a b c

vectorVisible _ _ LayerHidden                                         = False
vectorVisible _ _ WMTS                                                = False
vectorVisible zoomLevel (Layer _ minZoom _ _) _ | zoomLevel < minZoom = False
vectorVisible zoomLevel (Layer _ _ _ maxZoom) _ | zoomLevel > maxZoom = False
vectorVisible zoomLevel (Layer _ _ limitZoom maxZoom) Vector          = zoomLevel >= limitZoom && zoomLevel <= maxZoom