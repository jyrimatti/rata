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
  , Color(..)
  )
where

import Maps.Types
import Prelude                        ( Double, Bool )
import React.Flux.Rn.Events     (EventHandlerType, invoke1, This(..), on0)
import React.Flux.Rn.Types (Color(..))
import Numeric.Natural (Natural)
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                )

coordinates :: Has c "coordinates" => [LatLng] -> Props c handler
coordinates = prop "coordinates"

strokeWidth :: Has c "strokeWidth" => Natural -> Props c handler
strokeWidth = prop "strokeWidth"

strokeColor :: Has c "strokeColor" => Color -> Props c handler
strokeColor = prop "strokeColor"

fillColor :: Has c "fillColor" => Color -> Props c handler
fillColor = prop "fillColor"

tappable :: Has c "tappable" => Bool -> Props c handler
tappable = prop "tappable"


-- Events:

onPress :: Has c "onPress" => EventHandlerType handler -> Props c handler
onPress = on0 "onPress"
