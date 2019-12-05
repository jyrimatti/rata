{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Callout
  ( module Maps.Callout
  )
where
import Maps.Types
import Numeric.Natural
import Prelude                        (fmap
                                                , (.)
                                                , mempty
                                                , ($)
                                                , Bool
                                                )
import React.Flux                     ( foreign_
                                                , ReactElementM
                                                )
import React.Flux.Rn.Events     (EventHandlerType, invoke1, This(..), on1)
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                , props
                                                )

data Callout
callout :: [Props Callout handler] -> ReactElementM handler a -> ReactElementM handler a
callout = foreign_ "MapView.Callout" . fmap props

tooltip :: Has c "tooltip" => Bool -> Props c handler
tooltip = prop "tooltip"

alphaHitTest :: Has c "alphaHitTest" => Natural -> Props c handler
alphaHitTest = prop "alphaHitTest"

instance Has Callout "tooltip"
instance Has Callout "alphaHitTest"


-- Events:

onPress :: Has c "onPress" => EventHandlerType handler -> Props c handler
onPress = on0 "onPress"

instance Has Callout "onPress"