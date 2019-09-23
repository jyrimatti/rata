{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE LambdaCase      #-}
module Views where

import           Data.Foldable                  ( toList, concatMap )
import           Data.Geospatial as Geospatial
import           Data.LineString
import           Data.LinearRing
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Maybe                     ( isJust )
import qualified Data.Sequence                 as Seq
import           Dispatcher
import           Data.JSString                  ( pack )
import           GHCJS.Types                    ( JSVal )
import           Infra
import           Layer
import           LayerTypes
import           Maps.MapView
import           Maps.Circle
import           Maps.Polyline
import           Maps.Polygon
import           Maps.Marker
import           Maps.UrlTile as UrlTile
import           Numeric.Natural
import           Prelude                        ( ($)
                                                , zip
                                                , show
                                                , error
                                                , mempty
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
                                                , Int
                                                )
import           React.Flux.Rn.APIs             ( Platform(..)
                                                , platform
                                                )
import           React.Flux.Rn.Components.Button
import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.View
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import           React.Flux.Rn.Views
import           React.Flux.Rn.APIs             ( log )
import           Store

testView :: ReactView ()
testView = mkControllerView @'[] "testView" $ \() ->
    "testView"

initialReg = Region 61.4858493 23.9091292 0.5 0.5



app :: JSVal -> ReactView ()
app cms =
  mkControllerView @'[StoreArg AppState] "Rata"
    $ \(AppState layerStates layerData zoomLevel) () ->
        view
            [ style
                [height (Perc 100), flex 1, flexDirection Row]
            ]
          $ do
              view [style [ position Absolute
                          , left 0
                          , right 0
                          , top 0
                          , bottom 0]] $ do
                mapView
                    [ mapType
                      $ if platform == IOS then MutedStandard else Standard
                    , showsUserLocation True
                    , region initialReg
                    , customMapStyle cms
                    , onRegionChangeComplete (dispatch . RegionChangeComplete)
                    ]
                  $ do
                      mapM_
                          (\(i, layer) -> wmtsLayer
                            (zoomLevel, ((-1)*i), layer, (layerStates Map.! layer))
                          )
                        $ zip [1 ..] allLayers
                      mapM_
                        (\layer -> vectorLayer
                          ( zoomLevel
                          , layer
                          , (layerStates Map.! layer)
                          , (layerData Map.!? layer)
                          )
                        )
                        allLayers

wmtsLayer :: (Natural, Int, Layer, LayerState) -> ReactElementM eventHandler ()
wmtsLayer = mkView "wmtsLayer" $ \(zoomLevel, index, layer, state) ->
  if wmtsVisible zoomLevel layer state
    then urlTile
      [ UrlTile.zIndex index
      , urlTemplate $ wmtsUrl apiBase $ case layerType layer of LayerType x -> layerPath x
      , shouldReplaceMapContent False
      ]
    else view [] mempty

vectorLayer
  :: (Natural, Layer, LayerState, Maybe [GeospatialGeometry])
  -> ReactElementM eventHandler ()
vectorLayer = mkView "vectorLayer" $ \(zoomLevel, layer, state, layerData) ->
  if vectorVisible zoomLevel layer state && isJust layerData
  then
      mapM_ (\case
              Geospatial.Point (GeoPoint geom) -> circle [ center $ toLatLng $ retrieveXY geom, radius 50 ]
              Geospatial.MultiLine geom -> polyline
                [ strokeWidth 0.5
                , (coordinates . fmap (toLatLng . retrieveXY) . concatMap (fromLineString . _unGeoLine) . splitGeoMultiLine) geom
                ]
              Geospatial.Polygon geom -> polygon
                [ (coordinates . fmap (toLatLng . retrieveXY) . concatMap fromLinearRing . _unGeoPolygon) geom
                ]
              {-Geospatial.MultiPolygon geom -> polygon
                [ (coordinates . fmap (toLatLng . retrieveXY) . concatMap (concatMap fromLinearRing . _unGeoPolygon) . splitGeoMultiPolygon) geom
                ]-}
              _ -> error "Not implemented!"
          )
        $ fromJust layerData
  else view [] mempty

toLatLng (PointXY x y) = LatLng x y