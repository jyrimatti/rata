{-# LANGUAGE DataKinds                        #-}
{-# LANGUAGE DuplicateRecordFields            #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude                #-}
{-# LANGUAGE RankNTypes                       #-}
module Navigation.Types where

import Data.Aeson (Object)
import Data.Function
import GHCJS.Marshal                  
import GHCJS.Types
import Prelude                        (Either(..),pure)                     
import React.Flux       hiding (on)
import React.Flux.Internal
import React.Flux.View

type Navigation = ReactViewRef Object

instance IsJSVal (View ())

instance (IsJSVal a, IsJSVal b) => ToJSVal (Either a b) where
    toJSVal (Left a) = pure $ jsval a
    toJSVal (Right b) = pure $ jsval b
    