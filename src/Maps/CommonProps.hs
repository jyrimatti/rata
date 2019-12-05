{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.CommonProps
  ( module Maps.CommonProps
  , LatLng(..)
  )
where

import Maps.Types
import Prelude                        ( Double, Bool )
import React.Flux.Rn.Events     (EventHandlerType, invoke1, This(..), on1)
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                )

coordinates :: Has c "coordinates" => [LatLng] -> Props c handler
coordinates = prop "coordinates"

strokeWidth :: Has c "strokeWidth" => Double -> Props c handler
strokeWidth = prop "strokeWidth"

tappable :: Has c "tappable" => Bool -> Props c handler
tappable = prop "tappable"


-- Events:

onPress :: Has c "onPress" => (OnPress -> EventHandlerType handler) -> Props c handler
onPress = on1 "onPress"
