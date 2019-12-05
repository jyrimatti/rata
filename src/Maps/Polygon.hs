{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Polygon
  ( module Maps.Polygon
  , CommonProps.coordinates, CommonProps.strokeWidth, CommonProps.tappable, CommonProps.onPress
  , LatLng(..)
  )
where

import Maps.CommonProps as CommonProps
import Maps.Types
import Prelude                        ( fmap
                                                , (.)
                                                , mempty
                                                , ($)
                                                , Bool
                                                )
import React.Flux                     ( foreign_
                                                , ReactElementM
                                                )
import React.Flux.Rn.Events     (EventHandlerType, invoke1, This(..), on0)
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , props
                                                , prop
                                                )

data Polygon
polygon :: [Props Polygon handler] -> ReactElementM handler ()
polygon = ($ mempty) . foreign_ "MapView.Polygon" . fmap props

instance Has Polygon "tappable"
instance Has Polygon "coordinates"
instance Has Polygon "strokeWidth"


-- Events:

instance Has Polygon "onPress"