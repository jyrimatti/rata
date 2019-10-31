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
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Views where

import           Data.Foldable                  ( concatMap )
import           Data.Geospatial as Geospatial
import           Data.LinearRing
import           Data.LineString
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Maybe                     ( isJust )
import           Debug.Trace (trace)
import           Dispatcher
import           GHCJS.Types                    ( JSVal )
import           Infra
import           Layer
import           LayerTypes
import           Maps.Circle
import           Maps.MapView
import           Maps.Polygon
import           Maps.Polyline
import           Maps.UrlTile as UrlTile
import           Numeric.Natural
import           Prelude                        ( ($)
                                                , mapM, zip
                                                , show
                                                , error
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
import           React.Flux.Rn.APIs             ( log )
import           React.Flux.Rn.APIs             ( Platform(..)
                                                , platform
                                                )
import           React.Flux.Rn.Components.Button as Button
import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.View
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import           React.Flux.Rn.Views

import           Store

emptyView :: ReactView ()
emptyView = mkControllerView @'[] "emptyView" $ \() -> mempty


app :: JSVal -> ReactView ()
app cms =
  mkControllerView @'[StoreArg AppState] "Rata" $ \(AppState _ layerStates layerData zoomLevel initialReg _) () ->
        view [ style [ height (Perc 100)
                     , flex 1
                     , flexDirection Row]]
          $ do
              view [ style [ position Absolute
                           , left 0
                           , right 0
                           , top 0
                           , bottom 0]] $ do
                mapView
                        [ mapType $ if platform == IOS then MutedStandard else Standard
                        , showsUserLocation True
                        , region initialReg
                        , customMapStyle cms
                        , onRegionChangeComplete (\this region -> dispatch $ RegionChangeComplete region)
                        ]
                      $ do
                      mapM_
                          (\(i, layer) -> wmtsLayer
                            (zoomLevel, i, layer, (layerStates Map.! layer))
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
                button [ title "Menu"
                       , Button.onPress (dispatch ToggleLayerMenu)
                       ]

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
              Geospatial.Point geom -> trace "point" $ circle
                [ radius 50
                , (center . toLatLng . retrieveXY . _unGeoPoint) geom
                ]
              Geospatial.MultiPoint geom -> trace "multipoint" $ mapM_ (\geopoint -> circle
                [ radius 50
                , (center . toLatLng . retrieveXY . _unGeoPoint) geopoint
                ]) $ splitGeoMultiPoint geom
              Geospatial.Line geom -> trace "line" $ polyline
                [ strokeWidth 0.5
                , (coordinates . fmap (toLatLng . retrieveXY) . fromLineString . _unGeoLine) geom
                ]
              Geospatial.MultiLine geom -> trace "multiline" $ mapM_ (\geoline -> polyline
                [ strokeWidth 0.5
                , (coordinates . fmap (toLatLng . retrieveXY) . fromLineString . _unGeoLine) geoline
                ]) $ splitGeoMultiLine geom
              Geospatial.Polygon geom -> trace "polygon" $ polygon
                [ (coordinates . fmap (toLatLng . retrieveXY) . concatMap fromLinearRing . _unGeoPolygon) geom
                ]
              Geospatial.MultiPolygon geom -> trace "multipolygon" $ mapM_ (\geopolygon -> polygon
                [ (coordinates . fmap (toLatLng . retrieveXY) . concatMap fromLinearRing . _unGeoPolygon) geopolygon
                ]) $ splitGeoMultiPolygon geom
              _ -> error "Not implemented!"
          )
        $ fromJust layerData
  else view [] mempty

toLatLng (PointXY x y) = LatLng x y