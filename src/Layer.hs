{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Layer where

import Control.DeepSeq
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Typeable                  ( Typeable )
import GHC.Generics                   ( Generic )
import LayerTypes
import Numeric.Natural
import Prelude (($), String, Show(..), Maybe(..), Eq, Ord, Bool(..), (.), (<>), (==), (>), (<), (>=), fmap)
import Transform

type LayerSource = (String, Maybe String, [String])

data LayerState = LayerHidden | LayerShown
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
layerName layer state = show (layerType layer)

wmtsSuffix :: String
wmtsSuffix = "/MERCATOR/{z}/{y}/{x}.png"

vectorSuffix :: String
vectorSuffix = ".geojson" 

wmtsUrl :: String -> LayerSource -> Bool -> String
wmtsUrl baseURL (layerPath, typename, _) diagram =
  baseURL <> layerPath <> wmtsSuffix <> case intercalate "&" (catMaybes [presentation diagram, typeNames typename]) of
    "" -> ""
    x  -> "?" <> x

vectorUrl :: String -> LayerSource -> BBox -> Bool -> String
vectorUrl baseURL (layerPath, typename, pn) bbox diagram =
  baseURL <> layerPath <> vectorSuffix <> case intercalate "&" (catMaybes [Just ("bbox=" <> bbox2string bbox), presentation diagram, propertyName pn, Just "srsName=epsg:4326", typeNames typename]) of
    "" -> ""
    x  -> "?" <> x

propertyName :: [String] -> Maybe String
propertyName [] = Nothing
propertyName xs = Just $ "propertyName=" <> intercalate "," xs

presentation :: Bool -> Maybe String
presentation True = Just "presentation=diagram"
presentation False = Nothing

typeNames :: Maybe String -> Maybe String
typeNames = fmap ("typeNames=" <>)

wmtsVisible :: Natural -> Layer -> LayerState -> Bool
wmtsVisible _ _ LayerHidden                                              = False
wmtsVisible zoomLevel (Layer _ minZoom _   _) _ | zoomLevel < minZoom    = False
wmtsVisible zoomLevel (Layer _ _ limitZoom _) _ | zoomLevel >= limitZoom = False
wmtsVisible _ _ _                                                        = True

vectorVisible :: Natural -> Layer -> LayerState -> Bool
vectorVisible _ _ LayerHidden                                             = False
vectorVisible zoomLevel (Layer _ _ limitZoom _) _ | zoomLevel < limitZoom = False
vectorVisible zoomLevel (Layer _ _ _ maxZoom  ) _ | zoomLevel > maxZoom   = False
vectorVisible _ _ _                                                       = True
