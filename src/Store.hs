{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE LambdaCase                   #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
module Store where

import           Control.Concurrent (forkIO)
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad (forM_)
import qualified Data.Aeson                    as A
import           Data.Aeson.Types (parseEither)
import           Data.Foldable                  ( toList, elem , traverse_)
import           Data.Geospatial
import           Data.JSString                  ( pack )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Sequence                 as Seq hiding (filter)
import qualified Data.Text as T
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           GHCJS.Fetch
import           Layer
import           LayerTypes
import           Maps.Types (Region(..),LatLng(..))
import           Navigation.DrawerNavigator
import           Navigation.Navigation
import           Numeric.Natural
import           Prelude                        ((/=),  Show
                                                , Eq
                                                , Bool(..)
                                                , pure
                                                , (||)
                                                , fmap
                                                , ($)
                                                , (-)
                                                , (+)
                                                , return
                                                , filter
                                                , not
                                                , (.)
                                                , show
                                                , Maybe(Nothing)
                                                , Either(..)
                                                , String
                                                , IO
                                                , (>>=)
                                                , (<>)
                                                , concatMap
                                                , (==)
                                                , (&&)
                                                )
import           React.Flux                     ( StoreData(..), action, SomeStoreAction, HasField(..), executeAction )
import           React.Flux.Rn.Events            (This(..))
import           React.Flux.Rn.Util             ( log, logJSVal )
import           Transform

hasVectorData :: AppState -> Bool -> Layer -> Bool
hasVectorData st diagram layer = Map.member layer (if diagram then layerDataDiagram st else layerDataMap st)

data MapType = Map | Diagram
    deriving (Show, Typeable, Generic, NFData, Eq)

data AppState = AppState {
    layerStates :: Map.Map Layer LayerState,
    layerDataMap :: Map.Map Layer [Feature FeatureProperties],
    layerDataDiagram :: Map.Map Layer [Feature FeatureProperties],
    zoomLevel :: Natural,
    viewport :: Region,
    tileCacheMap :: Map.Map Layer [Tile],
    tileCacheDiagram :: Map.Map Layer [Tile],
    layerMenu :: This Navigation,
    map :: This MapView,
    showDetails :: Maybe Oid,
    mapType :: MapType
} deriving (Show, Typeable, Eq)

appStore :: AppState
appStore = AppState (Map.fromList $ fmap (, LayerHidden) allLayers)
                    (Map.fromList $ fmap (, []) allLayers)
                    (Map.fromList $ fmap (, []) allLayers)
                    12
                    initialReg
                    (Map.fromList $ fmap (, []) allLayers)
                    (Map.fromList $ fmap (, []) allLayers)
                    (This "")
                    (This "")
                    Nothing
                    Map

instance HasField "layerStates" AppState (Map.Map Layer LayerState) where
    getField = layerStates
instance HasField "layerDataMap" AppState (Map.Map Layer [Feature FeatureProperties]) where
    getField = layerDataMap
instance HasField "layerDataDiagram" AppState (Map.Map Layer [Feature FeatureProperties]) where
    getField = layerDataDiagram
instance HasField "zoomLevel" AppState Natural where
    getField = zoomLevel
instance HasField "viewport" AppState Region where
    getField = viewport

data AppAction = SaveLayerMenu (This Navigation)
               | SaveMapView (This MapView)
               | ChangeLayerState Layer
               | VectorFetchFailed Layer T.Text T.Text
               | VectorFetchSucceeded MapType Layer Tile (Seq (GeoFeature FeatureProperties))
               | RegionChangeComplete Region
               | ToggleLayerMenu
               | ObjectTapped Oid
               | MapChanged MapType
    deriving (Show, Typeable, Generic, NFData, Eq)

showAppAction (VectorFetchSucceeded maptype layer tile _) = show $ VectorFetchSucceeded maptype layer tile Seq.empty
showAppAction x = show x

toFeature :: GeoFeature FeatureProperties -> Feature FeatureProperties
toFeature (GeoFeature _ geom  props _) = Feature (toOid props) props geom

disp :: AppAction -> SomeStoreAction
disp a = action @AppState a

fetchVectorLayers :: [Tile] -> MapType -> Natural -> Region -> Layer -> IO ()
fetchVectorLayers tileCache maptype level region layer = traverse_ (\tile -> fetchVectorLayer maptype tile layer) $ filter (not . (`elem` tileCache)) $ region2Tiles level region

fetchVectorLayer :: MapType -> Tile -> Layer -> IO ()
fetchVectorLayer maptype tile layer = let
    url = vectorUrl (layerBase layer) (layerPath layer) tile (maptype == Diagram)
    doFetch = do
        log . pack $ "Fetching vector data from: " <> url
        resp <- fetch (Request (pack url) defaultRequestOptions)
        json <- responseJSON resp
        executeAction $ case parseEither A.parseJSON json of
            Left msg                                -> disp $ VectorFetchFailed layer (T.pack url) (T.pack msg)
            Right (GeoFeatureCollection _ features) -> disp $ VectorFetchSucceeded maptype layer tile features
  in do
    _ <- forkIO $ catch doFetch $ \(JSPromiseException e) -> do
        logJSVal (pack $ "Got an exception while fetching url " <> url <> ": ") e
    return ()

initialReg :: Region        
initialReg = Region 61.4858254 24.0470175 (Just 0.5) (Just 0.5)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

instance StoreData AppState where
    type StoreAction AppState = AppAction

    transform action st = do
        log . pack $ "Store action: " <> showAppAction action
        catchAny (doTransform action st) $ \e -> do
            log . pack $ "Got an exception while performing action " <> show action <> ": " <> show e
            return st

doTransform :: AppAction -> AppState -> IO AppState
doTransform action st@(AppState layerStates layerDataMap layerDataDiagram zoomLevel region tileCacheMap tileCacheDiagram layerMenu _ showDetails maptype) =
  let tileCacheFor Map     = tileCacheMap
      tileCacheFor Diagram = tileCacheDiagram
  in case action of
    ChangeLayerState layer -> do
        let state = layerStates Map.! layer
        case state of
            LayerHidden | vectorVisible zoomLevel layer LayerShown -> do
                log . pack $ "retrieving vector data for layer " <> show layer <> " on level " <> show zoomLevel <> " for region " <> show region
                fetchVectorLayers (Map.findWithDefault [] layer $ tileCacheFor maptype) maptype zoomLevel region layer
                return $ st { layerStates = Map.insert layer LayerShown layerStates }
            LayerHidden | wmtsVisible zoomLevel layer state    -> return $ st { layerStates = Map.insert layer LayerShown layerStates }
            LayerHidden                                        -> return   st
            _                                                  -> return $ st { layerStates = Map.insert layer LayerHidden layerStates }

    VectorFetchFailed layer _ _ ->
        return st { layerStates = Map.insert layer LayerHidden layerStates }

    VectorFetchSucceeded maptype layer tile features ->
        return st { layerDataMap     = if maptype == Diagram then layerDataMap     else Map.adjust (\old -> old <> fmap toFeature (toList features)) layer layerDataMap
                  , layerDataDiagram = if maptype == Map     then layerDataDiagram else Map.adjust (\old -> old <> fmap toFeature (toList features)) layer layerDataDiagram
                  , tileCacheMap     = if maptype == Diagram then tileCacheMap     else Map.adjust (tile :) layer tileCacheMap
                  , tileCacheDiagram = if maptype == Map     then tileCacheDiagram else Map.adjust (tile :) layer tileCacheDiagram
                  }

    RegionChangeComplete newViewport -> do
        let keys = Map.keys . Map.filterWithKey (\layer state -> state /= LayerHidden && vectorVisible zoomLevel layer state) $ layerStates
        forM_ keys (\layer -> fetchVectorLayers (Map.findWithDefault [] layer $ tileCacheFor maptype) maptype zoomLevel newViewport layer)
        return st { viewport = newViewport }

    ToggleLayerMenu -> do
        drawerAction ToggleDrawer layerMenu
        return st

    SaveLayerMenu navigation ->
        return st { layerMenu = navigation }

    SaveMapView map ->
        return st { map = map }

    ObjectTapped oid -> do
        let showDetailsNew = case showDetails of
             Nothing                         -> Just oid
             old@(Just other) | other == oid -> old
             _                               -> Just oid
        return st { showDetails = showDetailsNew }

    MapChanged maptype -> do
        let keys = Map.keys . Map.filterWithKey (\layer state -> state /= LayerHidden && vectorVisible zoomLevel layer state) $ layerStates
        forM_ keys (\layer -> fetchVectorLayers (Map.findWithDefault [] layer $ tileCacheFor maptype) maptype zoomLevel region layer)
        return st { mapType = maptype }