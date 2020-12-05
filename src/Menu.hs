{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
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

import           Layer
import           LayerTypes

import           Prelude                             (mapM_, ($), (<), (>),
                                                      (||))
import           React.Flux                                        (elemShow,
                                                                    elemString)
import           React.Flux.Rn.Components.Button
import           React.Flux.Rn.Components.ScrollView

import           React.Flux.Rn.Components.Text as Text
import           React.Flux.Rn.Components.TouchableHighlight as TH
import           React.Flux.Rn.Components.View as V
import           React.Flux.Rn.Types.AlignContent as AC
import           React.Flux.Rn.Views
import           Store

menu = mkControllerView @'[StoreArg AppState] "menu" $ \state () ->
    scrollView [ style [ marginTop (Pt 20) ] ] $
        mapM_ (\layer -> layerButton (zoomLevel state, layer, (layerStates state) Map.! layer)) allLayers

layerButton = mkView "layerButton" $ \(zoomLevel, layer, state) ->
    view [style [ alignContent AC.FlexStart ]] $
        touchableHighlight [ TH.onPress (dispatch $ ChangeLayerState layer)
                           , TH.disabled $ disabledForLayer zoomLevel layer
                                ] $
            text [ style [ Text.color $ buttonColor state
                         , fontSize 12
                         , paddingTop (Pt 3)
                         ] ] $
                elemString $ layerName layer state

disabledForLayer zoomLevel Layer{visibility = Visibility {minZoom, maxZoom}} =
    zoomLevel < minZoom || zoomLevel > maxZoom

buttonColor LayerHidden    = Rgba 42 42 42 42
buttonColor LayerShown     = Rgb 0 255 0