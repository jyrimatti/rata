{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
module Layer where

import Data.List (intercalate)
import Control.DeepSeq
import Data.Typeable                  ( Typeable )
import GHC.Generics                   ( Generic )
import LayerTypes
import Maps.Types (Region(..))
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

wmtsSuffix = "/MERCATOR/{z}/{y}/{x}.png?profile"

vectorSuffix propertyName region =
  ".geojson?bbox=" <> intercalate "," (coords region) <> "&" <> "profile&propertyName=" <> propertyName <> "&srsName=epsg:4326"
  where coords (Region latitude longitude (Just latitudeDelta) (Just longitudeDelta)) = [
            show $ latitude - latitudeDelta/2
          , show $ longitude - longitudeDelta/2
          , show $ latitude + latitudeDelta/2
          , show $ longitude + longitudeDelta/2]
        coords (Region latitude longitude Nothing Nothing) = [
            show $ latitude
          , show $ longitude
          , show $ latitude
          , show $ longitude]

wmtsUrl baseURL (layerPath, typename, _) =
  baseURL <> layerPath <> wmtsSuffix <> fmap (\x -> if x == '&' then '&' else x) (typeNames typename)

vectorUrl baseURL (layerPath, typename, propertyName) region =
  baseURL <> layerPath <> vectorSuffix propertyName region <> typeNames typename

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