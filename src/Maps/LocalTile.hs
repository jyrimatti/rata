{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Maps.LocalTile (
  module Maps.LocalTile
) where

import Maps.Types()
import Prelude    (String,fmap,(.),mempty,($),Bool,Int)
import React.Flux                      (ReactElementM, foreign_)
import React.Flux.Rn.Properties        (Has, Props, prop, props)

data LocalTile
localTile :: [Props LocalTile handler] -> ReactElementM handler ()
localTile = ($ mempty) . foreign_ "MapView.LocalTile" . fmap props

pathTemplate :: Has c "pathTemplate" => String -> Props c handler
pathTemplate = prop "pathTemplate"

tileSize :: Has c "tileSize" => Bool -> Props c handler
tileSize = prop "tileSize"

zIndex :: Has c "zIndex" => Int -> Props c handler
zIndex = prop "zIndex"

instance Has LocalTile "pathTemplate"
instance Has LocalTile "tileSize"
instance Has LocalTile "zIndex"
