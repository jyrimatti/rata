{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Menu where

import qualified Data.Map as Map

import           Dispatcher
import           Infra

import           Layer

import           Prelude                             (mapM_, ($), (<), (>),
                                                      (||))
import           React.Flux.Rn.Components.Button

import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.View
import           React.Flux.Rn.Views
import           Store


menu = mkControllerView @'[StoreArg AppState] "menu" $ \(AppState _ layerStates _ zoomLevel _ _) () ->
    scrollView [] $
        mapM_ (\layer -> layerButton (zoomLevel, layer, layerStates Map.! layer)) allLayers

layerButton = mkView "layerButton" $ \(zoomLevel, layer, state) ->
    view [style []] $
        button [ title (layerName layer state)
               , disabled $ disabledForLayer zoomLevel layer
               , onPress (dispatch $ ChangeLayerState layer)
               , color $ buttonColor state
               ]

disabledForLayer zoomLevel (Layer _ minZoom _ maxZoom) =
    zoomLevel < minZoom || zoomLevel > maxZoom

buttonColor LayerHidden    = Rgba 42 42 42 42
buttonColor WMTS           = Rgb 0 255 0
buttonColor Vector         = Rgb 0 0 255
buttonColor VectorFetching = Rgb 0 0 128