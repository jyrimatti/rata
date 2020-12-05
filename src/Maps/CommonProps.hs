{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.CommonProps
  ( module Maps.CommonProps
  , module Maps.Types
  , module React.Flux.Rn.Types.Color
  )
where

import Maps.Types (LatLng(..))
import Numeric.Natural (Natural)
import Prelude                        ( Double, Bool )
import React.Flux.Rn.Events     (EventHandlerType, invoke1, This(..), on0)
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                )
import React.Flux.Rn.Types ()
import React.Flux.Rn.Types.Color

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
