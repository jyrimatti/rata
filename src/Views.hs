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
module Views where

import           Data.Foldable                  ( toList )
import           Data.Geospatial
import           Data.LineString
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Maybe                     ( isJust )
import qualified Data.Sequence                 as Seq
import           Dispatcher
import           GHCJS.Types                    ( JSVal )
import           Infra
import           Layer
import           LayerTypes
import           Maps.MapView
import           Maps.Polyline
import           Maps.UrlTile as UrlTile
import           Numeric.Natural
import           Prelude                        ( ($)
                                                , zip
                                                , mempty
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
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import           React.Flux.Rn.Views
import           Store

testView :: ReactView ()
testView = mkControllerView @'[] "testView" $ \() ->
    "testView"

initialReg = Region 61.4858493 23.9091292 0.5 0.5

wmtsVisible _ _ LayerHidden = False
wmtsVisible zoomLevel (Layer _ minZoom _ _) _ | zoomLevel < minZoom = False
wmtsVisible zoomLevel (Layer _ _ _ maxZoom) _ | zoomLevel > maxZoom = False
wmtsVisible a b c      = not $ vectorVisible a b c

vectorVisible _ _ LayerHidden = False
vectorVisible _ _ WMTS   = False
vectorVisible zoomLevel (Layer _ minZoom _ _) _ | zoomLevel < minZoom = False
vectorVisible zoomLevel (Layer _ _ _ maxZoom) _ | zoomLevel > maxZoom = False
vectorVisible zoomLevel (Layer _ _ limitZoom maxZoom) Vector =
  zoomLevel >= limitZoom && zoomLevel <= maxZoom

app :: JSVal -> ReactView ()
app cms =
  mkControllerView @'[StoreArg AppState] "Rata"
    $ \(AppState layerStates layerData zoomLevel) () ->
        view
            [ style
                [height (Perc 100), flex 1, flexDirection Row]
            ]
          $ do
              scrollView
                  [ style
                    [ minWidth 50
                    , width (Perc 21)
                    , flex 1
                    , flexDirection Column
                    ]
                  , contentContainerStyle
                    [ justifyContent FlexStart__
                    , alignItems FlexStart___
                    ]
                  ]
                $ do
                    mapM_
                      (\layer -> layerButton
                        (zoomLevel, layer, layerStates Map.! layer)
                      )
                      allLayers
              view [style [height 500]] $ do
                mapView
                    [ mapType
                      $ if platform == IOS then MutedStandard else Standard
                    , showsUserLocation True
                    , region initialReg
                    , customMapStyle cms
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

layerButton = mkView "layerButton" $ \(zoomLevel, layer, state) -> do
  view [style []] $ do
    button
      [ title (layerName layer)
      , disabled $ disabledForLayer zoomLevel layer
      , onPress (dispatch $ ChangeLayerState layer)
      , color $ buttonColor state
      ]

disabledForLayer zoomLevel (Layer _ minZoom _ maxZoom) =
  zoomLevel < minZoom || zoomLevel > maxZoom

buttonColor LayerHidden = Rgba 42 42 42 42
buttonColor WMTS        = Rgba 0 0 255 128
buttonColor Vector      = Rgb 0 0 255

wmtsLayer :: (Natural, Int, Layer, LayerState) -> ReactElementM eventHandler ()
wmtsLayer = mkView "wmtsLayer" $ \(zoomLevel, index, layer, state) ->
  if wmtsVisible zoomLevel layer state
    then urlTile
      [ UrlTile.zIndex index
      , urlTemplate $ wmtsUrl apiBase $ case layerType layer of
        LayerType x -> layerPath x
      ]
    else view [] mempty

vectorLayer
  :: (Natural, Layer, LayerState, Maybe (Seq.Seq GeoLine))
  -> ReactElementM eventHandler ()
vectorLayer = mkView "vectorLayer" $ \(zoomLevel, layer, state, layerData) ->
  if vectorVisible zoomLevel layer state && isJust layerData
    then
      mapM_
          (\geoline -> polyline
            [ strokeWidth 0.5
            , coordinates
            $ toList
            $ fmap ((\(PointXY x y) -> LatLng x y) . retrieveXY)
            $ toSeq
            $ _unGeoLine geoline
            ]
          )
        $ fromJust layerData
    else view [] mempty
