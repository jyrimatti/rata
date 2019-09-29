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
module Navigation.Navigation where

import Control.Monad
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , (.:)
                                                , (.=)
                                                , Object
                                                )
import           Data.Function
import Data.JSString (unpack)
import           Data.Map (Map,singleton,mapWithKey,toList)
import           GHC.Generics                   ( Generic )
import           GHCJS.Marshal                  
import           GHCJS.Types                    
import Data.Maybe
import qualified JavaScript.Object as JSO
import           Numeric.Natural
import           Prelude                        ( String
                                                , Double
                                                , return
                                                , fmap
                                                , IO
                                                , mapM_
                                                , error
                                                , (.)
                                                , Show
                                                , (<>)
                                                , Bool
                                                , Either(..)
                                                , mempty
                                                )                     
import           React.Flux       hiding (on)
import React.Flux.View
import React.Flux.Internal
import           React.Flux.Rn.Properties
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import qualified React.Flux.Rn.StyleProps.LayoutStyleProps
                                               as LayoutStyleProps
import           React.Flux.Rn.Types            ( Color(..)
                                                , Inset(..)
                                                , str)
import           React.Flux.Rn.Views
import           System.IO.Unsafe (unsafePerformIO)
import Maps.Types

type Navigation = ReactViewRef Object
type NavigationId = JSString

data NavigationProps = NavigationProps {
    initialRouteName :: String,
    contentComponent :: Maybe (View ())
} deriving (Generic)
instance ToJSVal NavigationProps where
  toJSVal (NavigationProps initialRouteName contentComponent) = do
    o <- JSO.create
    s <- toJSVal initialRouteName
    JSO.setProp "initialRouteName" s o
    mapM_ (\x -> JSO.setProp "contentComponent" (jsval x) o) contentComponent
    return $ jsval o

instance IsJSVal (View ())

data DrawerAction = OpenDrawer | CloseDrawer | ToggleDrawer
    deriving (Show,Generic)
instance ToJSON DrawerAction
instance ToJSVal DrawerAction where
  toJSVal OpenDrawer   = str "openDrawer"
  toJSVal CloseDrawer  = str "closeDrawer"
  toJSVal ToggleDrawer = str "toggleDrawer"

createDrawerNavigator :: Map String (View ()) -> NavigationProps -> IO Navigation
createDrawerNavigator pages p = do
    pp <- toJSVal p
    o <- JSO.create
    mapM_ (\(k,v) -> JSO.setProp (toJSString k) (jsval v) o) $ toList pages
    js_createDrawerNavigator o pp

createAppContainer :: Navigation -> [Props Navigation handler] -> IO (ReactElementM handler ())
createAppContainer n p = do
    aa <- js_createAppContainer n
    return $ elementToM () $ ForeignElement (Right aa) (fmap props p) mempty

getNavigation :: String -> IO Navigation
getNavigation navigationId = do
    ii <- toJSVal navigationId
    js_getNavigation ii

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

ref :: Has c "ref" => (NavigationId -> EventHandlerType handler) -> Props c handler
ref f = prop_ $ CallbackPropertyWithSingleArgument "ref" $ \h -> do
    ret <- js_ref h
    return $ f ret

drawerAction :: DrawerAction -> NavigationId -> IO ()
drawerAction a n = do
    act <- toJSVal a
    js_drawerAction act n

instance Has Navigation "ref"

#ifdef __GHCJS__
 
foreign import javascript unsafe 
    "navigation_createDrawerNavigator($1,$2)"
    js_createDrawerNavigator :: JSO.Object -> JSVal -> IO Navigation

foreign import javascript unsafe
    "navigation_createAppContainer($1)"
    js_createAppContainer :: Navigation -> IO (ReactViewRef Object)

foreign import javascript unsafe
    "navigation_NavigationActions.navigate($1)"
    js_navigate :: JSVal -> IO ()

foreign import javascript unsafe
    "window[$2].dispatch(navigation_DrawerActions[$1]())"
    js_drawerAction :: JSVal -> NavigationId -> IO ()

foreign import javascript unsafe
    "window[$1]"
    js_getNavigation :: JSVal -> IO Navigation

foreign import javascript unsafe
    "$r = ''+Math.random(); window[$r] = $1"
    js_ref :: HandlerArg -> IO NavigationId

#else

js_createDrawerNavigator :: JSO.Object -> JSVal -> IO Navigation
js_createDrawerNavigator _ = error "js_createDrawerNavigator only works with GHCJS"

js_createAppContainer :: Navigation -> IO (ReactViewRef Object)
js_createAppContainer _ = error "js_createAppContainer only works with GHCJS"

js_navigate :: JSVal -> IO ()
js_navigate _ = error "js_navigate only works with GHCJS"

js_drawerAction :: JSVal -> NavigationId -> IO ()
js_drawerAction _ = error "js_drawerAction only works with GHCJS"

js_getNavigation :: JSVal -> IO Navigation
js_getNavigation _ = error "js_getNavigation only works with GHCJS"

js_ref :: HandlerArg -> IO NavigationId
js_ref _ = error "js_ref only works with GHCJS"

#endif

