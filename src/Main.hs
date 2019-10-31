{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

import Data.Aeson                     ( decode, Value)
import Data.ByteString.Lazy.Char8     ( pack )
import Data.Map (singleton)                     
import Data.Maybe
import Dispatcher
import GHCJS.Marshal                  ( ToJSVal(..) )
import Menu
import Navigation.Navigation
import Prelude                        ( IO , ($), (.) )

import React.Flux
import React.Flux.Ajax                ( initAjax )
import React.Flux.Rn.App              ( registerApp )
import Store
import Store                          ( appStore )
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
  navigation <- createDrawerNavigator (singleton "main" mainView) $ NavigationProps "main" $ Just menu

  -- initialize layer menu, and save reference for future use
  ac <- createAppContainer navigation [ ref (dispatch . SaveLayerMenu) ]

  -- wrap the app to a contoller-view, and register
  registerApp "rnproject" $ mkControllerView @'[] "wrapper" $ \() -> ac

 