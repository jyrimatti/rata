
{-# LANGUAGE DataKinds                        #-}
{-# LANGUAGE DeriveGeneric                    #-}
{-# LANGUAGE DuplicateRecordFields            #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE NamedFieldPuns                   #-}
{-# LANGUAGE NoImplicitPrelude                #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE RankNTypes                       #-}
module Navigation.BottomTabNavigator (
      module Navigation.BottomTabNavigator
    , CommonProps.style, CommonProps.ref
    
) where

import           Control.Monad
import           Data.Aeson (FromJSON, ToJSON, toJSON, Object)
import           Data.Foldable (traverse_)
import           Data.Function
import           Data.Map (Map,toList)
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           GHCJS.Marshal                  
import           GHCJS.Marshal.Pure                    
import           GHCJS.Types
import qualified Data.JSString as JSString
import Debug.Trace (trace)
import qualified JavaScript.Object as JSO
import           Numeric.Natural
import           Prelude                        ( String
                                                , Int
                                                , Bool
                                                , fromIntegral
                                                , return
                                                , fmap
                                                , IO
                                                , mapM_
                                                , error
                                                , (.)
                                                , Show(..)
                                                , Either(..)
                                                , mempty
                                                )                     
import           React.Flux       hiding (on)
import           React.Flux.Internal
import React.Flux.Rn.Events       (fromJSON, fromNativeJSON, This, invoke)
import           React.Flux.Rn.Properties
import           React.Flux.Rn.Props.CommonProps as CommonProps
                                                ( style, ref )
import           React.Flux.Rn.Types            ( str)
import           React.Flux.View
import           React.Flux.Rn.Util (call2)
import GHCJS.Foreign.Callback
import Unsafe.Coerce
import Navigation.Types
import Navigation.Navigation


data BottomTabNavigatorConfig = BottomTabNavigatorConfig {
    initialRouteName :: String,
    navigationOptionsTab :: Maybe NavigationOptions
} deriving (Generic)
instance ToJSVal BottomTabNavigatorConfig where
  toJSVal BottomTabNavigatorConfig{initialRouteName, navigationOptionsTab} = do
    o <- JSO.create
    s <- toJSVal initialRouteName
    JSO.setProp "initialRouteName" s o
    traverse_ (\x -> toJSVal x >>= \xx -> JSO.setProp "navigationOptions" xx o) navigationOptionsTab
    return $ jsval o

defaultBottomTabNavigatorConfig initialRouteName = BottomTabNavigatorConfig initialRouteName Nothing



createBottomTabNavigator :: [(String, Either (View ()) Navigation)] -> BottomTabNavigatorConfig -> IO Navigation
createBottomTabNavigator pages p = do
    o <- JSO.create
    traverse_ (\(k,v) -> do
        vv <- toJSVal v
        JSO.setProp (toJSString k) vv o
      ) pages
    ret :: JSVal <- call2 "navigation_createBottomTabNavigator" (jsval o) p
    r <- unsafeCoerce ret
    return r
