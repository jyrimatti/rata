{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
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
import           Data.Foldable                  ( concatMap , traverse_)
import           Data.Geospatial as Geospatial
import           Data.LinearRing
import           Data.LineString
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Maybe                     ( isJust )
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Data.Traversable (traverse)
import           Debug.Trace (trace)
import           Dispatcher
import           GHCJS.Types                    ( JSVal )
import qualified Icons.Icon as Icon
import           Icons.Icon (name, size)
import           Layer
import           LayerTypes
import           Maps.Callout
import           Maps.Circle
import           Maps.LocalTile as LocalTile
import           Maps.MapView as MapView
import           Maps.Marker as Marker
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
import           React.Flux (StoreField(..),elemString)
import           React.Flux.Rn.APIs             ( Platform(..)
                                                , platform
                                                )
import           React.Flux.Rn.Components.Button as Button
import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.Text as Text
import           React.Flux.Rn.Components.TouchableWithoutFeedback as TWF
import           React.Flux.Rn.Components.View as View
import           React.Flux.Rn.Events (This(..))

import           React.Flux.Rn.Properties (Props)
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import           React.Flux.Rn.Util (log)
import           React.Flux.Rn.Views
import           Store
import           Svg.SvgUri
import           Svg.SvgXml

emptyView :: ReactView ()
emptyView = mkControllerView @'[] "emptyView" $ \() -> mempty


app :: JSVal -> Bool -> ReactView ()
app cms useDiagram =
  mkControllerView @'[StoreField AppState "layerStates" (Map.Map Layer LayerState),
                      StoreField AppState "layerDataMap" (Map.Map Layer [Feature FeatureProperties]),
                      StoreField AppState "layerDataDiagram" (Map.Map Layer [Feature FeatureProperties]),
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
                           , bottom 0
                           , backgroundColor $ Rgb 0 0 0 ]] $ do
                mapWrapper cms useDiagram (zoomLevel, viewport, layerStates, if useDiagram  then layerDataDiagram else layerDataMap)
                view [ style [ marginTop (Pt 20) ] ] $ 
                  text [ Text.onPress (dispatch ToggleLayerMenu) ] "menu" {-Icon.entypo [ name "menu"
                            , size 20
                            , Icon.onPress (dispatch ToggleLayerMenu)
                            ]-}

mapWrapper cms useDiagram = mkView "MapWrapper" $ \(zoomLevel, viewport, layerStates, layerData) ->
  mapView
      [ MapView.mapType $ if platform == IOS then MutedStandard else Standard
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
      , urlTemplate $ wmtsUrl (layerBase layer) (layerPath layer) useDiagram
      , shouldReplaceMapContent False
      ]
  else view [] mempty

emptyBackground :: Bool -> ReactElementM eventHandler ()
emptyBackground useDiagram =
  if useDiagram
  then urlTile
      [ UrlTile.zIndex 0
      , urlTemplate "https://placehold.it/256/ffffff?text=%20&w=256&h=256"
      ]
  else view [] mempty

vectorLayer :: (Natural, Layer, LayerState, Maybe [Feature FeatureProperties]) -> ReactElementM eventHandler ()
vectorLayer = mkView "vectorLayer" $ \(zoomLevel, layer, state, layerData) ->
  if vectorVisible zoomLevel layer state && isJust layerData
  then
    traverse_ (\(Feature oid properties geom) -> renderGeometry layer oid properties geom) $ fromJust layerData
  else view [] mempty

tap Nothing = []
tap (Just oid) = [Marker.onPress (dispatch $ ObjectTapped oid)]

mkCallout :: Maybe Oid -> FeatureProperties -> ReactElementM handler ()
mkCallout oid properties =
  text [ style [ fontSize 10, flexWrap NoWrap, minWidth (Pt 130), maxWidth (Pt 200), maxHeight (Pt 80) ]] $
    calloutContent oid properties

calloutContent :: Maybe Oid -> FeatureProperties -> ReactElementM handler ()
calloutContent oid properties = traverse_ (elemString . getOid) oid

mkMarker :: Layer -> Maybe Oid -> FeatureProperties -> LatLng -> ReactElementM _ ()
mkMarker layer oid properties c =
  marker ([ coordinate c ] <> tap oid) $ do
    layerIcon c layer properties
    callout [] $ mkCallout oid properties

mkPolyline :: Layer -> Maybe Oid -> FeatureProperties -> GeoLine -> ReactElementM _ ()
mkPolyline layer oid properties geom = polyline $
    [ tappable True
    , (coordinates . fmap (toLatLng . retrieveXY) . fromLineString . _unGeoLine) geom
    ] <> lineStyle layer properties
      <> tap oid

mkPolygon :: Layer -> Maybe Oid -> FeatureProperties -> GeoPolygon -> ReactElementM _ ()
mkPolygon layer oid properties geom = polygon $
    [ tappable True
    , (coordinates . fmap (toLatLng . retrieveXY) . concatMap fromLinearRing . _unGeoPolygon) geom
    ] <> polygonStyle layer properties
      <> tap oid

renderGeometry :: Layer -> Maybe Oid -> FeatureProperties -> GeospatialGeometry -> ReactElementM _ ()
renderGeometry layer oid properties = \case
  Geospatial.Point        geom ->           (mkMarker layer oid properties . toLatLng . retrieveXY . _unGeoPoint) geom
  Geospatial.MultiPoint   geom -> traverse_ (mkMarker layer oid properties . toLatLng . retrieveXY . _unGeoPoint) $ splitGeoMultiPoint geom
  Geospatial.Line         geom ->            mkPolyline layer oid properties geom
  Geospatial.MultiLine    geom -> traverse_ (mkPolyline layer oid properties) $ splitGeoMultiLine geom
  Geospatial.Polygon      geom ->            mkPolygon layer oid properties geom
  Geospatial.MultiPolygon geom -> traverse_ (mkPolygon layer oid properties) $ splitGeoMultiPolygon geom
  Geospatial.Collection   geom -> traverse_ (renderGeometry layer oid properties) geom
  Geospatial.NoGeometry        -> mempty

toLatLng (PointXY x y) = LatLng x y