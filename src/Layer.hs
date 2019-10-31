{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Layer where

import Control.DeepSeq
import Data.Aeson
import Data.ByteString.Lazy.UTF8
import Data.Typeable                  ( Typeable )
import GHC.Generics                   ( Generic )
import LayerTypes
import Numeric.Natural
import Transform

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

wmtsSuffix = "/MERCATOR/{z}/{y}/{x}.png"

vectorSuffix propertyName region =
  ".geojson?bbox=" <> (show $ region2bbox region) <> "&" <> "propertyName=" <> propertyName <> "&srsName=epsg:4326"

vectorSuffix2 propertyName bbox =
  ".geojson?bbox=" <> bbox2string bbox <> "&" <> "propertyName=" <> propertyName <> "&srsName=epsg:4326"

wmtsUrl baseURL (layerPath, typename, _) =
  baseURL <> layerPath <> wmtsSuffix <> fmap (\x -> if x == '&' then '&' else x) (typeNames typename)

vectorUrl baseURL (layerPath, typename, propertyName) region =
  baseURL <> layerPath <> vectorSuffix propertyName region <> typeNames typename

vectorUrl2 baseURL (layerPath, typename, propertyName) bbox =
  baseURL <> layerPath <> vectorSuffix2 propertyName bbox <> typeNames typename

typeNames :: Maybe String -> String
typeNames Nothing  = ""
typeNames (Just x) = "&typeNames=" <> x

wmtsVisible _ _ LayerHidden                                         = False
wmtsVisible _ _ VectorFetching                                      = False
wmtsVisible zoomLevel (Layer _ minZoom _ _) _ | zoomLevel < minZoom = False
wmtsVisible zoomLevel (Layer _ _ _ maxZoom) _ | zoomLevel > maxZoom = False
wmtsVisible a b c                                                   = not $ vectorVisible a b c

vectorVisible _ _ LayerHidden                                         = False
vectorVisible _ _ WMTS                                                = False
vectorVisible zoomLevel (Layer _ minZoom _ _) _ | zoomLevel < minZoom = False
vectorVisible zoomLevel (Layer _ _ _ maxZoom) _ | zoomLevel > maxZoom = False
vectorVisible zoomLevel (Layer _ _ limitZoom maxZoom) Vector          = zoomLevel >= limitZoom && zoomLevel <= maxZoom
vectorVisible zoomLevel (Layer _ _ limitZoom maxZoom) VectorFetching  = zoomLevel >= limitZoom && zoomLevel <= maxZoom
