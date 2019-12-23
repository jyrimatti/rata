{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes            #-}
module Svg.SvgXml (
  module Svg.SvgXml,
  Transform(..), Angle(..),
  LayoutStyleProps.width,
  LayoutStyleProps.height,
  CommonProps.style,
  TransformsStyleProps.transform
) where

import Data.String (fromString)
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
import React.Flux.Rn.StyleProps.TransformsStyleProps as TransformsStyleProps
import React.Flux.Rn.Props.CommonProps as CommonProps
import React.Flux.Rn.Types(Transform(..), Angle(..))
import React.Flux.Rn.Util (js_lookupWindow)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data SvgXml
svgXml :: [Props SvgXml handler] -> ReactElementM handler ()
svgXml = ($ mempty) . foreign_ "Svg.SvgXml" . fmap props

xml :: Has c "xml" => String -> Props c handler
xml = prop "xml" . js_lookupWindow . fromString

rotation :: Has c "rotation" => Double -> Props c handler
rotation = prop "rotation"

scale :: Has c "scale" => Double -> Props c handler
scale = prop "scale"

originX :: Has c "originX" => Double -> Props c handler
originX = prop "originX"

originY :: Has c "originY" => Double -> Props c handler
originY = prop "originY"

instance Has SvgXml "width"
instance Has SvgXml "height"
instance Has SvgXml "style"
instance Has SvgXml "transform"
instance Has SvgXml "xml"
instance Has SvgXml "rotation"
instance Has SvgXml "scale"
instance Has SvgXml "originX"
instance Has SvgXml "originY"
