{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DuplicateRecordFields            #-}
module Navigation.Navigation (
      module Navigation.Navigation
    , CommonProps.style, CommonProps.ref
    
) where

import           Control.Monad
import           Data.Aeson                     ( ToJSON(..)
                                                , Object
                                                )
import           Data.Function
import           Data.Map (Map,toList)
import           Data.Maybe
import           GHC.Generics                   ( Generic )                  
import           GHCJS.Marshal                    
import           GHCJS.Types
import qualified JavaScript.Object as JSO
import           Prelude                        ( String
                                                , return
                                                , fmap
                                                , IO
                                                , mapM_
                                                , error
                                                , (.)
                                                , Show
                                                , Either(..)
                                                , mempty
                                                )                     
import           React.Flux       hiding (on)
import           React.Flux.Internal
import           React.Flux.Rn.Properties
import           React.Flux.Rn.Events (This)
import           React.Flux.Rn.Types            ( str)
import           React.Flux.View
import           React.Flux.Rn.Props.CommonProps as CommonProps
                                                ( style, ref )

type Navigation = ReactViewRef Object

data DrawerNavigatorConfig = DrawerNavigatorConfig {
    initialRouteName :: String,
    contentComponent :: Maybe (View ())
} deriving (Generic)
instance ToJSVal DrawerNavigatorConfig where
  toJSVal (DrawerNavigatorConfig initialRouteName contentComponent) = do
    o <- JSO.create
    s <- toJSVal initialRouteName
    JSO.setProp "initialRouteName" s o
    mapM_ (\x -> JSO.setProp "contentComponent" (jsval x) o) contentComponent
    return $ jsval o

data BottomTabNavigatorConfig = BottomTabNavigatorConfig {
    initialRouteName :: String
} deriving (Generic)
instance ToJSVal BottomTabNavigatorConfig where
  toJSVal (BottomTabNavigatorConfig initialRouteName) = do
    o <- JSO.create
    s <- toJSVal initialRouteName
    JSO.setProp "initialRouteName" s o
    --mapM_ (\x -> JSO.setProp "contentComponent" (jsval x) o) contentComponent
    return $ jsval o

instance IsJSVal (View ())

data DrawerAction = OpenDrawer | CloseDrawer | ToggleDrawer
    deriving (Show,Generic)
instance ToJSON DrawerAction
instance ToJSVal DrawerAction where
  toJSVal OpenDrawer   = str "openDrawer"
  toJSVal CloseDrawer  = str "closeDrawer"
  toJSVal ToggleDrawer = str "toggleDrawer"

createDrawerNavigator :: [(String, Either (View ()) Navigation)] -> DrawerNavigatorConfig -> IO Navigation
createDrawerNavigator pages p = do
    pp <- toJSVal p
    o <- JSO.create
    mapM_ (\(k,v) -> JSO.setProp (toJSString k) (either2jsval v) o) $ pages
    js_createDrawerNavigator o pp

createBottomTabNavigator :: [(String, Either (View ()) Navigation)] -> BottomTabNavigatorConfig -> IO Navigation
createBottomTabNavigator pages p = do
    pp <- toJSVal p
    o <- JSO.create
    mapM_ (\(k,v) -> JSO.setProp (toJSString k) (either2jsval v) o) $ pages
    js_createBottomTabNavigator o pp

either2jsval :: Either (View ()) Navigation -> JSVal
either2jsval (Left v)  = jsval v
either2jsval (Right n) = jsval n

createAppContainer :: Navigation -> [Props Navigation handler] -> IO (ReactElementM handler ())
createAppContainer n p = do
    aa <- js_createAppContainer n
    return $ elementToM () $ ForeignElement (Right aa) (fmap props p) mempty

data NavigateProps = NavigateProps {
    routeName :: String,
    --,params :: Object
    --action :: () -> (),
    key :: Maybe String
} deriving (Show,Generic)
instance ToJSON NavigateProps
instance ToJSVal NavigateProps where
  toJSVal = toJSVal . toJSON

navigate :: NavigateProps -> IO ()
navigate = js_navigate <=< toJSVal



drawerAction :: DrawerAction -> This Navigation -> IO ()
drawerAction a n = do
    act <- toJSVal a
    js_drawerAction act n

instance Has Navigation "ref"



#ifdef __GHCJS__
 
foreign import javascript unsafe 
    "navigation_createDrawerNavigator($1,$2)"
    js_createDrawerNavigator :: JSO.Object -> JSVal -> IO Navigation

foreign import javascript unsafe 
    "navigation_createBottomTabNavigator($1,$2)"
    js_createBottomTabNavigator :: JSO.Object -> JSVal -> IO Navigation

foreign import javascript unsafe
    "navigation_createAppContainer($1)"
    js_createAppContainer :: Navigation -> IO (ReactViewRef Object)

foreign import javascript unsafe
    "navigation_NavigationActions.navigate($1)"
    js_navigate :: JSVal -> IO ()

foreign import javascript unsafe
    "window[$2].dispatch(navigation_DrawerActions[$1]())"
    js_drawerAction :: JSVal -> This Navigation -> IO ()

#else

js_createDrawerNavigator :: JSO.Object -> JSVal -> IO Navigation
js_createDrawerNavigator _ = error "js_createDrawerNavigator only works with GHCJS"

js_createBottomTabNavigator :: JSO.Object -> JSVal -> IO Navigation
js_createBottomTabNavigator _ = error "js_createBottomTabNavigator only works with GHCJS"

js_createAppContainer :: Navigation -> IO (ReactViewRef Object)
js_createAppContainer _ = error "js_createAppContainer only works with GHCJS"

js_navigate :: JSVal -> IO ()
js_navigate _ = error "js_navigate only works with GHCJS"

js_drawerAction :: JSVal -> This Navigation -> IO ()
js_drawerAction _ = error "js_drawerAction only works with GHCJS"

#endif

