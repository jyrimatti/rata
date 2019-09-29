{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Overlay
  ( module Maps.Overlay
  , LatLng(..)
  , ImageSource
  )
where

import           Prelude                        ( Double
                                                , fmap
                                                , (.)
                                                , mempty
                                                , Bool
                                                , ($)
                                                )
import           React.Flux                     ( foreign_
                                                , ReactElementM
                                                , EventHandlerType
                                                )
import           React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                , props
                                                )
import           Maps.Types
import           React.Flux.Rn.Events
import React.Flux.Rn.Types (ImageSource)

data Overlay
overlay :: [Props Overlay handler] -> ReactElementM handler a -> ReactElementM handler a
overlay = foreign_ "MapView.Overlay" . fmap props

image :: Has c "image" => ImageSource -> Props c handler
image = prop "image"

bounds :: Has c "bounds" => [LatLng] -> Props c handler
bounds = prop "bounds"

tappable :: Has c "tappable" => Bool -> Props c handler
tappable = prop "tappable"

-- Events
onPress :: Has c "onPress" => EventHandlerType handler -> Props c handler
onPress = on0 "onPress"

instance Has Overlay "style"
instance Has Overlay "position"
instance Has Overlay "top"
instance Has Overlay "bottom"
instance Has Overlay "left"
instance Has Overlay "right"

instance Has Overlay "image"
instance Has Overlay "bounds"
instance Has Overlay "tappable"

instance Has Overlay "onPress"
