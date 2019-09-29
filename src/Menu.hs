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
module Menu where

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

import qualified Data.Map as Map
import Numeric.Natural

import React.Flux.Rn.Views

import React.Flux.Rn.Components.Button
import React.Flux.Rn.Components.View
import React.Flux.Rn.Components.ScrollView

import Infra
import Store
import Layer
import Dispatcher

menu = mkControllerView @'[StoreArg AppState] "menu" $ \(AppState _ layerStates _ zoomLevel _) () -> do
    scrollView [] $ do
        mapM_ (\layer -> layerButton (zoomLevel, layer, layerStates Map.! layer)) allLayers

layerButton = mkView "layerButton" $ \(zoomLevel, layer, state) -> do
    view [style []] $ do
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