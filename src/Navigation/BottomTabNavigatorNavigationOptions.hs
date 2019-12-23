{-# LANGUAGE DataKinds                        #-}
{-# LANGUAGE DeriveGeneric                    #-}
{-# LANGUAGE DuplicateRecordFields            #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE NamedFieldPuns                   #-}
{-# LANGUAGE NoImplicitPrelude                #-}
{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE RankNTypes                       #-}
module Navigation.BottomTabNavigatorNavigationOptions where

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
import Control.Applicative ((<$>),(<*>))
import qualified JavaScript.Object.Internal as JSO
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
import React.Flux.Rn.Util (js_invoke1_)
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal
import Navigation.Types
import Data.Coerce

data BottomTabNavigatorNavigationOptions = BottomTabNavigatorNavigationOptions {
    tabBarOnPress :: TabBarOnPress -> ViewEventHandler
} deriving (Generic)
instance ToJSVal BottomTabNavigatorNavigationOptions where
    toJSVal BottomTabNavigatorNavigationOptions{tabBarOnPress} = do
        o <- JSO.create
        cb <- asyncCallback1 $ \js -> do
            Just aa@(TabBarOnPress _ defaultHandler) <- fromJSVal js
            traverse_ executeAction (tabBarOnPress aa)
            js_invoke1_ (jsval defaultHandler) js

        JSO.setProp "tabBarOnPress" (jsval cb) o
        -- should cb be released somewhere?
        return $ jsval o

data TabBarOnPress = TabBarOnPress {
    navigation :: String,
    defaultHandler :: Callback (TabBarOnPress -> IO ())
} deriving Generic
instance FromJSVal TabBarOnPress where
    fromJSVal js = do
        let jso = JSO.Object js
        nav <- JSO.getProp "navigation" jso
        dh <- JSO.getProp "defaultHandler" jso
        nav2 <- fromJSVal nav
        return $ TabBarOnPress <$> nav2 <*> Just (Callback dh)

instance ToJSVal TabBarOnPress where
    toJSVal TabBarOnPress{navigation, defaultHandler} = do
        o <- JSO.create
        nav <- toJSVal navigation
        JSO.setProp "navigation" nav o
        JSO.setProp "defaultHandler" (jsval defaultHandler) o
        return $ jsval o