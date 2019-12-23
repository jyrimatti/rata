{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

import Data.Aeson                     ( decode, Value)
import Data.ByteString.Lazy.Char8     ( pack )
import Data.Map (singleton, fromList)                     
import Data.Maybe
import Dispatcher
import GHCJS.Marshal                  ( ToJSVal(..) )
import Menu
import Navigation.Navigation
import Navigation.BottomTabNavigator
import Navigation.DrawerNavigator
import Prelude                        ( IO , ($), (.), Either(..), Bool(..) , putStrLn)

import React.Flux
import React.Flux.Ajax                ( initAjax )
import React.Flux.Rn.App              ( registerApp )
import Store
import Views                          ( app )

cmsJson :: Value
cmsJson =
  fromJust
    $ decode @Value
    $ pack
        "[ { \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#f5f5f5 \" } ] }, { \"elementType \": \"labels.icon \", \"stylers \": [ { \"visibility \": \"off \" } ] }, { \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#616161 \" } ] }, { \"elementType \": \"labels.text.stroke \", \"stylers \": [ { \"color \": \"#f5f5f5 \" } ] }, { \"featureType \": \"administrative.land_parcel \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#bdbdbd \" } ] }, { \"featureType \": \"poi \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#eeeeee \" } ] }, { \"featureType \": \"poi \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#757575 \" } ] }, { \"featureType \": \"poi.park \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#e5e5e5 \" } ] }, { \"featureType \": \"poi.park \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#9e9e9e \" } ] }, { \"featureType \": \"road \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#ffffff \" } ] }, { \"featureType \": \"road.arterial \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#757575 \" } ] }, { \"featureType \": \"road.highway \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#dadada \" } ] }, { \"featureType \": \"road.highway \", \"elementType \": \"geometry.fill \", \"stylers \": [ { \"color \": \"#ebebeb \" } ] }, { \"featureType \": \"road.highway \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#616161 \" } ] }, { \"featureType \": \"road.local \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#9e9e9e \" } ] }, { \"featureType \": \"transit.line \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#e5e5e5 \" } ] }, { \"featureType \": \"transit.station \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#eeeeee \" } ] }, { \"featureType \": \"water \", \"elementType \": \"geometry \", \"stylers \": [ { \"color \": \"#c9c9c9 \" } ] }, { \"featureType \": \"water \", \"elementType \": \"geometry.fill \", \"stylers \": [ { \"color \": \"#d4e3ff \" } ] }, { \"featureType \": \"water \", \"elementType \": \"labels.text.fill \", \"stylers \": [ { \"color \": \"#9e9e9e \" } ] } ]"

main :: IO ()
main = do
  -- parse map styling
  cms <- toJSVal cmsJson

  -- initialize future ajax requests
  initAjax

  -- register store
  registerInitialStore appStore

  let mainView = app cms

  -- create layer menu
  navigationMap     <- createDrawerNavigator [("main", Left $ mainView False)] $ (defaultDrawerNavigatorConfig "main") {
    contentComponent = Just menu,
    navigationOptionsDrawer = Just $ BottomTabNavigationOptions $ BottomTabNavigatorNavigationOptions {
      tabBarOnPress = \_ -> dispatch (MapChanged Map)
    }
  }
  navigationDiagram <- createDrawerNavigator [("main", Left $ mainView True)] $ (defaultDrawerNavigatorConfig "main") {
    contentComponent = Just menu,
    navigationOptionsDrawer = Just $ BottomTabNavigationOptions $ BottomTabNavigatorNavigationOptions {
      tabBarOnPress = \_ -> dispatch (MapChanged Diagram)
    }
  }

  -- create bottom tabs
  tabs <- createBottomTabNavigator [("map", Right navigationMap), ("diagram", Right navigationDiagram)] $ defaultBottomTabNavigatorConfig "map"

  -- initialize layer menu, and save reference for future use
  ac <- createAppContainer tabs [ ref (dispatch . SaveLayerMenu) ] 

  putStrLn "Registering Rata..."

  -- wrap the app to a contoller-view, and register
  registerApp "rnproject" $ mkControllerView @'[] "wrapper" $ \() -> ac

 