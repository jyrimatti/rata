{-# LANGUAGE CPP                              #-}
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
{-# LANGUAGE ScopedTypeVariables              #-}
module Navigation.DrawerNavigator (
      module Navigation.DrawerNavigator
    , module React.Flux.Rn.Props.CommonProps
    
) where

import           Control.Monad
import           Data.Aeson (FromJSON, ToJSON, toJSON, Object)
import           Data.Foldable (traverse_)
import           Data.Function
import qualified Data.JSString as JSString
import           Data.Map (Map,toList)
import           Data.Maybe
import           Debug.Trace (trace)                  
import           GHC.Generics                   ( Generic )                    
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types
import qualified JavaScript.Object as JSO
import           Navigation.Navigation                     
import           Navigation.Types
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
import           React.Flux (executeAction)
import           React.Flux.Internal
import           React.Flux.Rn.Events       (fromJSON, fromNativeJSON, This, invoke)
import           React.Flux.Rn.Properties
import           React.Flux.Rn.Props.CommonProps ( style, ref )
import           React.Flux.Rn.Types            ( str)
import           React.Flux.View
import           Unsafe.Coerce
    
data DrawerNavigatorConfig = DrawerNavigatorConfig {
    drawerWidth :: Maybe Natural,
    initialRouteName :: String,
    contentComponent :: Maybe (View ()),
    contentOptions :: Maybe ContentOptions,
    navigationOptionsDrawer :: Maybe NavigationOptions
} deriving (Generic)
instance ToJSVal DrawerNavigatorConfig where
  toJSVal DrawerNavigatorConfig{drawerWidth, initialRouteName, contentComponent, contentOptions, navigationOptionsDrawer} = do
    o <- JSO.create
    traverse_ (($ o) . JSO.setProp "drawerWidth" . pToJSVal . (fromIntegral :: Natural -> Int)) drawerWidth
    JSO.setProp "initialRouteName" (pToJSVal initialRouteName) o
    traverse_ (\x -> JSO.setProp "contentComponent" (jsval x) o) contentComponent
    traverse_ (\x -> toJSVal x >>= \xx -> JSO.setProp "contentOptions" xx o) contentOptions
    traverse_ (\x -> toJSVal x >>= \xx -> JSO.setProp "navigationOptions" xx o) navigationOptionsDrawer
    return $ jsval o

defaultDrawerNavigatorConfig :: String -> DrawerNavigatorConfig
defaultDrawerNavigatorConfig initialRouteName = DrawerNavigatorConfig Nothing initialRouteName Nothing Nothing Nothing

data OnItemPress = OnItemPress {
    route :: String,
    focused :: Bool
} deriving (Show,Generic)
instance FromJSON OnItemPress
instance FromJSVal OnItemPress where fromJSVal = fromJSON
instance ToJSON OnItemPress
instance ToJSVal OnItemPress where toJSVal = toJSVal . toJSON

data ContentOptions = ContentOptions {
    onItemPress :: (OnItemPress -> ViewEventHandler)
} deriving (Generic)
instance ToJSVal ContentOptions where
  toJSVal ContentOptions{onItemPress} = do
    o <- JSO.create
    cb <- asyncCallback1 $ \js -> do
        Just aa <- fromJSVal js
        traverse_ executeAction (onItemPress aa)
    JSO.setProp "onItemPress" (jsval cb) o
    -- should cb be released somewhere?
    return $ jsval o



createDrawerNavigator :: [(String, Either (View ()) Navigation)] -> DrawerNavigatorConfig -> IO Navigation
createDrawerNavigator pages p = do
    pp <- toJSVal p
    o <- JSO.create
    traverse_ (\(k,v) -> do
        vv <- toJSVal v
        JSO.setProp (toJSString k) vv o
      ) pages
    js_createDrawerNavigator o pp

data DrawerAction = OpenDrawer | CloseDrawer | ToggleDrawer
instance Show DrawerAction where
    show OpenDrawer   = "openDrawer"
    show CloseDrawer  = "closeDrawer"
    show ToggleDrawer = "toggleDrawer"

drawerAction :: DrawerAction -> This Navigation -> IO ()
drawerAction a this = invoke this (show a)


#ifdef __GHCJS__
 
foreign import javascript unsafe 
    "navigation_createDrawerNavigator($1,$2)"
    js_createDrawerNavigator :: JSO.Object -> JSVal -> IO Navigation

#else

js_createDrawerNavigator :: JSO.Object -> JSVal -> IO Navigation
js_createDrawerNavigator _ = error "js_createDrawerNavigator only works with GHCJS"

#endif
