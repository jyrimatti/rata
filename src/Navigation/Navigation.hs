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
                                                )
import           Data.Function
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
                                                )
import           React.Flux.Rn.Views

type Navigation = JSVal

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

createDrawerNavigator :: Map String (View ()) -> NavigationProps -> IO Navigation
createDrawerNavigator pages p = do
    pp <- toJSVal p
    o <- JSO.create
    mapM_ (\(k,v) -> JSO.setProp (toJSString k) (jsval v) o) $ toList pages
    js_createDrawerNavigator o pp

createAppContainer :: Navigation -> IO (View ())
createAppContainer = js_createAppContainer

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

#ifdef __GHCJS__
 
foreign import javascript unsafe 
    "$r = navigation_createDrawerNavigator($1,$2)"
    js_createDrawerNavigator :: JSO.Object -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$r = navigation_createAppContainer($1)"
    js_createAppContainer :: Navigation -> IO (View ())

foreign import javascript unsafe
    "navigation_NavigationActions.navigate($1)"
    js_navigate :: JSVal -> IO ()

#else

js_createDrawerNavigator :: JSO.Object -> JSVal -> IO JSVal
js_createDrawerNavigator _ = error "js_createDrawerNavigator only works with GHCJS"

js_createAppContainer :: Navigation -> IO (View ())
js_createAppContainer _ = error "js_createAppContainer only works with GHCJS"

js_navigate :: JSVal -> IO ()
js_navigate _ = error "js_navigate only works with GHCJS"

#endif

