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
module Navigation.Navigation (
      module Navigation.Navigation
    , module Navigation.Types
    , module Navigation.BottomTabNavigatorNavigationOptions
    , CommonProps.style, CommonProps.ref
    
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
import           Navigation.BottomTabNavigatorNavigationOptions                     
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
import           React.Flux.Internal
import           React.Flux.Rn.Events       (fromJSON, fromNativeJSON, This, invoke)
import           React.Flux.Rn.Properties
import           React.Flux.Rn.Props.CommonProps as CommonProps
                                                ( style, ref )
import           React.Flux.Rn.Types            ( str)
import           React.Flux.Rn.Util
import           React.Flux.View
import           Unsafe.Coerce

data NavigationOptions = StackNavigationOptions
                       | DrawerNavigationOptions
                       | BottomTabNavigationOptions BottomTabNavigatorNavigationOptions
                       | MaterialBottomTabNavigationOptions
                       | MaterialTopTabNavigationOptions
    deriving (Generic)
instance ToJSVal NavigationOptions where
    toJSVal (BottomTabNavigationOptions x) = toJSVal x

createAppContainer :: Navigation -> [Props Navigation handler] -> IO (ReactElementM handler ())
createAppContainer n p = do
    aa <- call1_jsval "navigation_createAppContainer" n
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
navigate = call1 "navigation_NavigationActions.navigate"



instance Has Navigation "ref"
