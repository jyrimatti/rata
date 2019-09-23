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

coordinates :: Has c "coordinates" => [LatLng] -> Props c handler
coordinates = prop "coordinates"

strokeWidth :: Has c "strokeWidth" => Double -> Props c handler
strokeWidth = prop "strokeWidth"
