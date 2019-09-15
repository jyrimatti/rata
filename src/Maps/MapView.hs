{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Maps.MapView
  ( module Maps.MapView
  , Region(..)
  , Camera(..)
  , Inset(..)
  , PaddingAdjustmentBehavior(..)
  , MapType(..)
  , Color(..)
  , Point(..)
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , (.:)
                                                , (.=)
                                                )
import           GHC.Generics                   ( Generic )

import           GHCJS.Marshal                  ( FromJSVal(..)
                                                , ToJSVal(..)
                                                )
import           GHCJS.Types                    ( JSString
                                                , JSVal
                                                )
import           Maps.Types                     ( Region(..)
                                                , Camera(..)
                                                , LatLng(..)
                                                , Location(..)
                                                , Point(..)
                                                , Frame(..)
                                                , MapType(..)
                                                , Inset(..)
                                                , Marker(..)
                                                , KmlContainer(..)
                                                , IndoorBuilding(..)
                                                , IndoorLevel(..)
                                                , PaddingAdjustmentBehavior(..)
                                                )
import           Numeric.Natural
import           Prelude                        ( String
                                                , Double
                                                , fmap
                                                , (.)
                                                , Show
                                                , (<>)
                                                , Bool
                                                )
import           React.Flux                     ( foreign_ )
import           React.Flux                     ( ReactElementM
                                                , foreign_
                                                )
import           React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                , props
                                                )
import           React.Flux.Rn.Props.CommonProps
                                                ( style )
import qualified React.Flux.Rn.StyleProps.LayoutStyleProps
                                               as LayoutStyleProps
import           React.Flux.Rn.Types            ( Color(..)
                                                , Inset(..)
                                                )

data MapView
-- needs dimensions to show up
mapView
  :: [Props MapView handler]
  -> ReactElementM handler a
  -> ReactElementM handler a
mapView =
  foreign_ "MapView"
    . fmap props
    . (style
        [ LayoutStyleProps.width (LayoutStyleProps.Perc 100)
        , LayoutStyleProps.height (LayoutStyleProps.Perc 100)
        ] :
      )

provider :: Has c "provider" => String -> Props c handler
provider = prop "provider"

region :: Has c "region" => Region -> Props c handler
region = prop "region"

initialRegion :: Has c "initialRegion" => Region -> Props c handler
initialRegion = prop "initialRegion"

camera :: Has c "camera" => Camera -> Props c handler
camera = prop "camera"

initialCamera :: Has c "initialCamera" => Camera -> Props c handler
initialCamera = prop "initialCamera"

mapPadding :: Has c "mapPadding" => Inset -> Props c handler
mapPadding = prop "mapPadding"

paddingAdjustmentBehavior
  :: Has c "paddingAdjustmentBehavior"
  => PaddingAdjustmentBehavior
  -> Props c handler
paddingAdjustmentBehavior = prop "paddingAdjustmentBehavior"

liteMode :: Has c "liteMode" => Bool -> Props c handler
liteMode = prop "liteMode"

mapType :: Has c "mapType" => MapType -> Props c handler
mapType = prop "mapType"

customMapStyle :: Has c "customMapStyle" => JSVal -> Props c handler
customMapStyle = prop "customMapStyle"

showsUserLocation :: Has c "showsUserLocation" => Bool -> Props c handler
showsUserLocation = prop "showsUserLocation"

userLocationAnnotationTitle
  :: Has c "userLocationAnnotationTitle" => String -> Props c handler
userLocationAnnotationTitle = prop "userLocationAnnotationTitle"

followsUserLocation :: Has c "followsUserLocation" => Bool -> Props c handler
followsUserLocation = prop "followsUserLocation"

showsMyLocationButton
  :: Has c "showsMyLocationButton" => Bool -> Props c handler
showsMyLocationButton = prop "showsMyLocationButton"

showsPointsOfInterest
  :: Has c "showsPointsOfInterest" => Bool -> Props c handler
showsPointsOfInterest = prop "showsPointsOfInterest"

showsCompass :: Has c "showsCompass" => Bool -> Props c handler
showsCompass = prop "showsCompass"

showsScale :: Has c "showsScale" => Bool -> Props c handler
showsScale = prop "showsScale"

showsBuildings :: Has c "showsBuildings" => Bool -> Props c handler
showsBuildings = prop "showsBuildings"

showsTraffic :: Has c "showsTraffic" => Bool -> Props c handler
showsTraffic = prop "showsTraffic"

showsIndoors :: Has c "showsIndoors" => Bool -> Props c handler
showsIndoors = prop "showsIndoors"

showsIndoorLevelPicker
  :: Has c "showsIndoorLevelPicker" => Bool -> Props c handler
showsIndoorLevelPicker = prop "showsIndoorLevelPicker"

zoomEnabled :: Has c "zoomEnabled" => Bool -> Props c handler
zoomEnabled = prop "zoomEnabled"

zoomTapEnabled :: Has c "zoomTapEnabled" => Bool -> Props c handler
zoomTapEnabled = prop "zoomTapEnabled"

zoomControlEnabled :: Has c "zoomControlEnabled" => Bool -> Props c handler
zoomControlEnabled = prop "zoomControlEnabled"

minZoomLevel :: Has c "minZoomLevel" => Natural -> Props c handler
minZoomLevel = prop "minZoomLevel"

maxZoomLevel :: Has c "maxZoomLevel" => Natural -> Props c handler
maxZoomLevel = prop "maxZoomLevel"

rotateEnabled :: Has c "rotateEnabled" => Bool -> Props c handler
rotateEnabled = prop "rotateEnabled"

scrollEnabled :: Has c "scrollEnabled" => Bool -> Props c handler
scrollEnabled = prop "scrollEnabled"

pitchEnabled :: Has c "pitchEnabled" => Bool -> Props c handler
pitchEnabled = prop "pitchEnabled"

toolbarEnabled :: Has c "toolbarEnabled" => Bool -> Props c handler
toolbarEnabled = prop "toolbarEnabled"

cacheEnabled :: Has c "cacheEnabled" => Bool -> Props c handler
cacheEnabled = prop "cacheEnabled"

loadingEnabled :: Has c "loadingEnabled" => Bool -> Props c handler
loadingEnabled = prop "loadingEnabled"

loadingIndicatorColor
  :: Has c "loadingIndicatorColor" => Color -> Props c handler
loadingIndicatorColor = prop "loadingIndicatorColor"

loadingBackgroundColor
  :: Has c "loadingBackgroundColor" => Color -> Props c handler
loadingBackgroundColor = prop "loadingBackgroundColor"

moveOnMarkerPress :: Has c "moveOnMarkerPress" => Bool -> Props c handler
moveOnMarkerPress = prop "moveOnMarkerPress"

legalLabelInsets :: Has c "legalLabelInsets" => Inset -> Props c handler
legalLabelInsets = prop "legalLabelInsets"

kmlSrc :: Has c "kmlSrc" => String -> Props c handler
kmlSrc = prop "kmlSrc"

compassOffset :: Has c "compassOffset" => Point -> Props c handler
compassOffset = prop "compassOffset"

instance Has MapView "style"
instance Has MapView "provider"
instance Has MapView "region"
instance Has MapView "initialRegion"
instance Has MapView "camera"
instance Has MapView "initialCamera"
instance Has MapView "mapPadding"
instance Has MapView "paddingAdjustmentBehavior"
instance Has MapView "liteMode"
instance Has MapView "mapType"
instance Has MapView "customMapStyle"
instance Has MapView "showsUserLocation"
instance Has MapView "userLocationAnnotationTitle"
instance Has MapView "followsUserLocation"
instance Has MapView "showsMyLocationButton"
instance Has MapView "showsPointsOfInterest"
instance Has MapView "showsCompass"
instance Has MapView "showsScale"
instance Has MapView "showsBuildings"
instance Has MapView "showsTraffic"
instance Has MapView "showsIndoors"
instance Has MapView "showsIndoorLevelPicker"
instance Has MapView "zoomEnabled"
instance Has MapView "zoomTapEnabled"
instance Has MapView "zoomControlEnabled"
instance Has MapView "minZoomLevel"
instance Has MapView "maxZoomLevel"
instance Has MapView "rotateEnabled"
instance Has MapView "scrollEnabled"
instance Has MapView "pitchEnabled"
instance Has MapView "toolbarEnabled"
instance Has MapView "cacheEnabled"
instance Has MapView "loadingEnabled"
instance Has MapView "loadingIndicatorColor"
instance Has MapView "loadingBackgroundColor"
instance Has MapView "moveOnMarkerPress"
instance Has MapView "legalLabelInsets"
instance Has MapView "kmlSrc"
instance Has MapView "compassOffset"

instance Has MapView "width"
instance Has MapView "height"