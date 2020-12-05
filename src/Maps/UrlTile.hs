{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Maps.UrlTile (
  module Maps.UrlTile
) where

import Maps.Types()
import Numeric.Natural
import Prelude    (String,fmap,(.),mempty,($),Bool,Int)
import React.Flux                      (ReactElementM, foreign_)
import React.Flux.Rn.Properties        (Has, Props, prop, props)
import React.Flux.Rn.Types ()

data UrlTile
urlTile :: [Props UrlTile handler] -> ReactElementM handler ()
urlTile = ($ mempty) . foreign_ "MapView.UrlTile" . fmap props

urlTemplate :: Has c "urlTemplate" => String -> Props c handler
urlTemplate = prop "urlTemplate"

zIndex :: Has c "zIndex" => Int -> Props c handler
zIndex = prop "zIndex"

minimumZ :: Has c "minimumZ" => Natural -> Props c handler
minimumZ = prop "minimumZ"

maximumZ :: Has c "maximumZ" => Natural -> Props c handler
maximumZ = prop "maximumZ"

shouldReplaceMapContent :: Has c "shouldReplaceMapContent" => Bool -> Props c handler
shouldReplaceMapContent = prop "shouldReplaceMapContent"

tileSize :: Has c "tileSize" => Bool -> Props c handler
tileSize = prop "tileSize"

flipY :: Has c "flipY" => Bool -> Props c handler
flipY = prop "flipY"

instance Has UrlTile "urlTemplate"
instance Has UrlTile "zIndex"
instance Has UrlTile "minimumZ"
instance Has UrlTile "maximumZ"
instance Has UrlTile "shouldReplaceMapContent"
instance Has UrlTile "tileSize"
instance Has UrlTile "flipY"