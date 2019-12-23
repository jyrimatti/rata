{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Polyline
  ( module Maps.Polyline
  , CommonProps.coordinates
  , CommonProps.strokeWidth
  , CommonProps.strokeColor
  , CommonProps.tappable
  , CommonProps.onPress
  , LatLng(..)
  , Color(..)
  )
where

import Maps.CommonProps as CommonProps
import Prelude                        ( fmap
                                                , (.)
                                                , mempty
                                                , ($)
                                                )
import React.Flux                     ( foreign_
                                                , ReactElementM
                                                )
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , props
                                                )
import React.Flux.Rn.Types (Color(..))

data Polyline
polyline :: [Props Polyline handler] -> ReactElementM handler ()
polyline = ($ mempty) . foreign_ "MapView.Polyline" . fmap props

instance Has Polyline "tappable"
instance Has Polyline "coordinates"
instance Has Polyline "strokeWidth"
instance Has Polyline "strokeColor"


-- Events:

instance Has Polyline "onPress"