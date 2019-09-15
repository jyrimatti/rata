{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Polyline
  ( module Maps.Polyline
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

data Polyline
polyline :: [Props Polyline handler] -> ReactElementM handler ()
polyline = ($ mempty) . foreign_ "MapView.Polyline" . fmap props

coordinates :: Has c "coordinates" => [LatLng] -> Props c handler
coordinates = prop "coordinates"

strokeWidth :: Has c "strokeWidth" => Double -> Props c handler
strokeWidth = prop "strokeWidth"

instance Has Polyline "coordinates"
instance Has Polyline "strokeWidth"
