{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Circle
  ( module Maps.Circle
  , LatLng(..)
  , Color(..)
  , CommonProps.strokeWidth
  , CommonProps.strokeColor
  , CommonProps.fillColor
  )
where
import Numeric.Natural
import           Prelude                        (fmap
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
import           React.Flux.Rn.Types (Color(..))
import           Maps.Types
import           Maps.CommonProps as CommonProps

data Circle
circle :: [Props Circle handler] -> ReactElementM handler ()
circle = ($ mempty) . foreign_ "MapView.Circle" . fmap props

center :: Has c "center" => LatLng -> Props c handler
center = prop "center"

radius :: Has c "radius" => Natural -> Props c handler
radius = prop "radius"

instance Has Circle "center"
instance Has Circle "radius"
instance Has Circle "strokeWidth"
instance Has Circle "strokeColor"
instance Has Circle "fillColor"
