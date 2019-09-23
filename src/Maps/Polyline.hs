{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Polyline
  ( module Maps.Polyline
  , CommonProps.coordinates, CommonProps.strokeWidth
  , LatLng(..)
  )
where

import           Prelude                        ( Double
                                                , fmap
                                                , (.)
                                                , mempty
                                                , ($)
                                                )
import           React.Flux                     ( foreign_
                                                , ReactElementM
                                                )
import           React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                , props
                                                )
import           Maps.Types
import Maps.CommonProps as CommonProps

data Polyline
polyline :: [Props Polyline handler] -> ReactElementM handler ()
polyline = ($ mempty) . foreign_ "MapView.Polyline" . fmap props

instance Has Polyline "coordinates"
instance Has Polyline "strokeWidth"
