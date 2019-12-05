{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Views where

import           Control.Arrow ((&&&))
import           Data.Foldable                  ( concatMap )
import           Data.Geospatial as Geospatial
import           Data.LinearRing
import           Data.LineString
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Maybe                     ( isJust )
import           Data.Monoid ((<>))
import           Debug.Trace (trace)
import           Dispatcher
import           GHCJS.Types                    ( JSVal )
import qualified Icons.Icon as Icon
import           Icons.Icon (name, size)
import           Infra
import           Layer
import           LayerTypes
import           Maps.Circle
import           Maps.MapView
import           Maps.Marker
import           Maps.Polygon
import           Maps.Polyline
import           Maps.UrlTile as UrlTile
import           Numeric.Natural
import           Prelude                        ( ($)
                                                , mapM, zip
                                                , uncurry
                                                , show
                                                , error
                                                , const
                                                , mempty
                                                , IO
                                                , (*)
                                                , (.)
                                                , fmap
                                                , mapM_
                                                , Bool(True, False)
                                                , (==)
                                                , (<=)
                                                , (>)
                                                , (<)
                                                , (>=)
                                                , (&&)
                                                , (||)
                                                , not
                                                , return
                                                , Int
                                                )
import           React.Flux (StoreField(..))
import           React.Flux.Rn.APIs             ( log )
import           React.Flux.Rn.APIs             ( Platform(..)
                                                , platform
                                                )
import           React.Flux.Rn.Components.Button as Button
import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.View

import           React.Flux.Rn.Events (This(..))
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import           React.Flux.Rn.Views
import           Store
import Svg.SvgUri

emptyView :: ReactView ()
emptyView = mkControllerView @'[] "emptyView" $ \() -> mempty


app :: JSVal -> Bool -> ReactView ()
app cms useDiagram =
  mkControllerView @'[StoreField AppState "layerStates" (Map.Map Layer LayerState),
                      StoreField AppState "layerDataMap" (Map.Map Layer [Feature]),
                      StoreField AppState "layerDataDiagram" (Map.Map Layer [Feature]),
                      StoreField AppState "zoomLevel" Natural,
                      StoreField AppState "viewport" Region
                     ] "Rata" $ \layerStates layerDataMap layerDataDiagram zoomLevel viewport () ->
        view [ style [ height (Perc 100)
                     , flex 1
                     , flexDirection Row]]
          $ 
              view [ style [ position Absolute
                           , left 0
                           , right 0
                           , top 0
                           , bottom 0]] $ do
                mapWrapper cms useDiagram (zoomLevel, viewport, layerStates, if useDiagram  then layerDataDiagram else layerDataMap)
                text [] "menu" {-Icon.entypo [ name "menu"
                            , size 20
                            , Icon.onPress (dispatch ToggleLayerMenu)
                            ]-}

mapWrapper cms useDiagram = mkView "MapWrapper" $ \(zoomLevel, viewport, layerStates, layerData) ->
  mapView
      [ mapType $ if platform == IOS then MutedStandard else Standard
      , showsUserLocation (not useDiagram)
      , initialRegion viewport
      , customMapStyle cms
      , ref (dispatch . SaveMapView)
      , onRegionChangeComplete (dispatch . RegionChangeComplete)
      ]
    $ do
      emptyBackground useDiagram
      mapM_
          (\(i, layer) -> wmtsLayer useDiagram 
            (zoomLevel, i, layer, layerStates Map.! layer)
          )
        $ zip [1 ..] allLayers
      mapM_
        (\layer -> vectorLayer
          ( zoomLevel
          , layer
          , layerStates Map.! layer
          , layerData Map.!? layer
          )
        )
        allLayers

wmtsLayer :: Bool -> (Natural, Int, Layer, LayerState) -> ReactElementM eventHandler ()
wmtsLayer useDiagram = mkView "wmtsLayer" $ \(zoomLevel, index, layer, state) ->
  if wmtsVisible zoomLevel layer state
  then urlTile
      [ UrlTile.zIndex index
      , urlTemplate $ wmtsUrl apiBase (case layerType layer of LayerType x -> layerPath x) useDiagram
      , shouldReplaceMapContent False
      ]
  else view [] mempty

emptyBackground :: Bool -> ReactElementM eventHandler ()
emptyBackground useDiagram =
  if useDiagram
  then urlTile
      [ UrlTile.zIndex 0
      , urlTemplate "https://placehold.it/256/ffffff?text=%20&w=256&h=256"
      , shouldReplaceMapContent False
      ]
  else view [] mempty

vectorLayer :: (Natural, Layer, LayerState, Maybe [Feature]) -> ReactElementM eventHandler ()
vectorLayer = mkView "vectorLayer" $ \(zoomLevel, layer, state, layerData) ->
  if vectorVisible zoomLevel layer state && isJust layerData
  then
    mapM_ (\(Feature oid geom) -> mapM_ (renderGeometry oid) geom) $ fromJust layerData
  else view [] mempty

--renderGeometry :: Maybe Oid -> GeospatialGeometry -> ReactElementM eventHandler ()
renderGeometry oid = \case
  Geospatial.Point geom -> marker (
      [ (coordinate . toLatLng . retrieveXY . _unGeoPoint) geom
      ] <> case oid of
        Nothing -> []
        Just o  -> [Maps.Marker.onPress (const $ dispatch $ ObjectTapped o)]
    ) $ svgUri [ uri "https://rata.digitraffic.fi/infra-api/0.3/icons/vaihde_yvv.svg" ]
  Geospatial.MultiPoint geom -> mapM_ (\geopoint -> marker (
        [ (coordinate . toLatLng . retrieveXY . _unGeoPoint) geopoint
        ] <> case oid of
          Nothing -> []
          Just o  -> [Maps.Marker.onPress (const $ dispatch $ ObjectTapped o)]
      ) "x"
    ) $ splitGeoMultiPoint geom
  Geospatial.Line geom -> polyline $
    [ strokeWidth 1
    , (coordinates . fmap (toLatLng . retrieveXY) . fromLineString . _unGeoLine) geom
    ] <> case oid of
      Nothing -> []
      Just o  -> [Maps.Polyline.onPress (const $ dispatch $ ObjectTapped o)]
  Geospatial.MultiLine geom -> mapM_ (\geoline -> polyline $
      [ strokeWidth 1
      , (coordinates . fmap (toLatLng . retrieveXY) . fromLineString . _unGeoLine) geoline
      ] <> case oid of
        Nothing -> []
        Just o  -> [Maps.Polyline.onPress (const $ dispatch $ ObjectTapped o)]
    ) $ splitGeoMultiLine geom
  Geospatial.Polygon geom -> polygon $
    [ (coordinates . fmap (toLatLng . retrieveXY) . concatMap fromLinearRing . _unGeoPolygon) geom
    ] <> case oid of
      Nothing -> []
      Just o  -> [Maps.Polygon.onPress (const $ dispatch $ ObjectTapped o)]
  Geospatial.MultiPolygon geom -> mapM_ (\geopolygon -> polygon $
      [ (coordinates . fmap (toLatLng . retrieveXY) . concatMap fromLinearRing . _unGeoPolygon) geopolygon
      ] <> case oid of
        Nothing -> []
        Just o  -> [Maps.Polygon.onPress (const $ dispatch $ ObjectTapped o)]
    ) $ splitGeoMultiPolygon geom
  _ -> error "Not implemented!"

toLatLng (PointXY x y) = LatLng x y