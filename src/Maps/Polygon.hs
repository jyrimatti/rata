{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Polygon
  ( module Maps.Polygon
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

data Polygon
polygon :: [Props Polygon handler] -> ReactElementM handler ()
polygon = ($ mempty) . foreign_ "MapView.Polygon" . fmap props

instance Has Polygon "coordinates"
instance Has Polygon "strokeWidth"
