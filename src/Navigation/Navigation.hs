{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Navigation.Navigation where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Function
import           Data.Map (Map,singleton)
import           GHC.Generics                   ( Generic )
import           GHCJS.Marshal                  
import           GHCJS.Types                    
import qualified JavaScript.Object as JSO
import           Numeric.Natural
import           Prelude                        ( String
                                                , Double
                                                , return
                                                , fmap
                                                , IO
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

} deriving (Show,Generic)
instance ToJSON NavigationProps
instance ToJSVal NavigationProps where
  toJSVal = toJSVal . toJSON

instance IsJSVal (View ())

createDrawerNavigator :: View () -> NavigationProps -> IO Navigation
createDrawerNavigator app p = do
    pp <- toJSVal p
    o <- JSO.create
    JSO.setProp "koti" (jsval app) o
    js_createDrawerNavigator o pp

createAppContainer :: Navigation -> IO (View ())
createAppContainer = js_createAppContainer

#ifdef __GHCJS__
 
foreign import javascript unsafe 
    "$r = navigation_createDrawerNavigator($1,$2)"
    js_createDrawerNavigator :: JSO.Object -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$r = navigation_createAppContainer($1)"
    js_createAppContainer :: Navigation -> IO (View ())

#else

js_createDrawerNavigator :: JSO.Object -> JSVal -> IO JSVal
js_createDrawerNavigator _ = error "js_createDrawerNavigator only works with GHCJS"

js_createAppContainer :: Navigation -> IO (View ())
js_createAppContainer _ = error "js_createAppContainer only works with GHCJS"

#endif

