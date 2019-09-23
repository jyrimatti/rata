{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes            #-}
module Svg.SvgUri (
  module Svg.SvgUri,
  LayoutStyleProps.width,
  LayoutStyleProps.height
) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           GHC.Generics               (Generic)
import           GHCJS.Marshal                  
import           GHCJS.Types                    
import           Numeric.Natural            (Natural)
import           Prelude                    (Bool, Double, IO, Int, Maybe (..),
                                             Num, Show, String, error, fmap,
                                             fromIntegral, id, init, last, pure,
                                             read, undefined, ($), (+), (++), mempty,
                                             (.), (<$>), (==), (>>=))
import React.Flux.Rn.Types (str)
import           React.Flux       hiding (on)
import           React.Flux.Rn.Properties
import           React.Flux.Rn.Views
import React.Flux.Rn.StyleProps.LayoutStyleProps as LayoutStyleProps

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data SvgUri
svgUri :: [Props SvgUri handler] -> ReactElementM handler ()
svgUri = ($ mempty) . foreign_ "Svg.SvgUri" . fmap props

uri :: Has c "uri" => String -> Props c handler
uri = prop "uri"

instance Has SvgUri "width"
instance Has SvgUri "height"
instance Has SvgUri "uri"