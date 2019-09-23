{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Marker
  ( module Maps.Marker
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

data Marker
marker :: [Props Marker handler] -> ReactElementM handler a -> ReactElementM handler a
marker = foreign_ "MapView.Marker" . fmap props

coordinate :: Has c "coordinate" => LatLng -> Props c handler
coordinate = prop "coordinate"

instance Has Marker "coordinate"
