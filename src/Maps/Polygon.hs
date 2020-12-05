{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Maps.Polygon
  ( module Maps.Polygon
  , module Maps.CommonProps
  , module Maps.Types
  )
where

import Maps.CommonProps (coordinates, strokeColor, Color(..), strokeColor, fillColor, tappable, onPress)
import Maps.Types (LatLng(..))
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
instance Has Polygon "strokeColor"
instance Has Polygon "fillColor"


-- Events:

instance Has Polygon "onPress"