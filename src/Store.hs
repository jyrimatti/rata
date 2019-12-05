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

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad (forM_,mapM_)
import qualified Data.Aeson                    as A
import           Data.Foldable                  ( toList, elem )
import           Data.Geospatial
import           Data.JSString                  ( pack )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Sequence                 as Seq hiding (elem,filter)
import qualified Data.Text as T
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           GHCJS.Fetch
import           Infra
import           Layer
import           LayerTypes
import           Maps.MapView (MapView)
import           Maps.Types (Region(..))
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
import           React.Flux                     ( StoreData(..), action, SomeStoreAction, HasField(..) )
import           React.Flux.Ajax                ( jsonAjax, RequestTimeout(..) )
import           React.Flux.Rn.APIs             ( log )
import           React.Flux.Rn.Events            (This(..))
import           Transform
import qualified Maps.MapView as MapView
import Maps.Types (LatLng(..))

hasVectorData :: AppState -> Bool -> Layer -> Bool
hasVectorData st diagram layer = Map.member layer (if diagram then layerDataDiagram st else layerDataMap st)

data AppState = AppState {
    layerStates :: Map.Map Layer LayerState,
    layerDataMap :: Map.Map Layer [Feature],
    layerDataDiagram :: Map.Map Layer [Feature],
    zoomLevel :: Natural,
    viewport :: Region,
    tileCacheMap :: Map.Map Layer [Tile],
    tileCacheDiagram :: Map.Map Layer [Tile],
    layerMenu :: This Navigation,
    map :: This MapView,
    showDetails :: Maybe Oid
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

instance HasField "layerStates" AppState (Map.Map Layer LayerState) where
    getField = layerStates
instance HasField "layerDataMap" AppState (Map.Map Layer [Feature]) where
    getField = layerDataMap
instance HasField "layerDataDiagram" AppState (Map.Map Layer [Feature]) where
    getField = layerDataDiagram
instance HasField "zoomLevel" AppState Natural where
    getField = zoomLevel
instance HasField "viewport" AppState Region where
    getField = viewport

data AppAction = SaveLayerMenu (This Navigation)
               | SaveMapView (This MapView)
               | ChangeLayerState Layer
               | VectorFetchFailed Layer T.Text T.Text
               | VectorFetchSucceeded Bool Layer Tile (Seq (GeoFeature A.Value))
               | RegionChangeComplete Region
               | ToggleLayerMenu
               | ObjectTapped Oid
    deriving (Show, Typeable, Generic, NFData, Eq)

showAppAction (VectorFetchSucceeded diagram layer tile _) = show $ VectorFetchSucceeded diagram layer tile Seq.empty
showAppAction x = show x

toFeature :: GeoFeature a -> Feature
toFeature (GeoFeature _ (Collection geom) _ oid) = Feature (toOid oid) $ toList geom
toFeature (GeoFeature _             geom  _ oid) = Feature (toOid oid) [geom]

toOid :: Maybe FeatureID -> Maybe Oid
toOid Nothing                    = Nothing
toOid (Just (FeatureIDText str)) = Just $ Oid $ T.unpack str
toOid (Just (FeatureIDNumber i)) = Nothing

disp :: AppAction -> [SomeStoreAction]
disp a = [action @AppState a]

fetchVectorLayers :: [Tile] -> Bool -> Natural -> Region -> Layer -> IO ()
fetchVectorLayers tileCache diagram level region layer = mapM_ (\tile -> fetchVectorLayer diagram tile layer) $ filter (not . (`elem` tileCache)) $ region2Tiles level region

fetchVectorLayer :: Bool -> Tile -> Layer -> IO ()
fetchVectorLayer diagram tile layer = let
    url = T.pack $ vectorUrl apiBase (let LayerType x = layerType layer in layerPath x) tile diagram
  in do
    log . pack $ "Fetching vector data from: " <> show url
    jsonAjax (TimeoutMilliseconds 180000) "GET" url [] () $ \case
        (_, Left msg)                                -> return $ disp $ VectorFetchFailed layer url msg
        (_, Right (GeoFeatureCollection _ features)) -> return $ disp $ VectorFetchSucceeded diagram layer tile features

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
doTransform action st@(AppState layerStates layerDataMap layerDataDiagram zoomLevel region tileCacheMap tileCacheDiagram layerMenu _ showDetails) = case action of
    ChangeLayerState layer -> do
        let state = layerStates Map.! layer
        case state of
            LayerHidden | vectorVisible zoomLevel layer LayerShown -> do
                log . pack $ "retrieving vector data for layer " <> show layer <> " on level " <> show zoomLevel <> " for region " <> show region
                -- TODO: don't fetch both presentations
                fetchVectorLayers (Map.findWithDefault [] layer tileCacheDiagram) True zoomLevel region layer
                fetchVectorLayers (Map.findWithDefault [] layer tileCacheMap) False zoomLevel region layer
                return $ st { layerStates = Map.insert layer LayerShown layerStates }
            LayerHidden | wmtsVisible zoomLevel layer state    -> return $ st { layerStates = Map.insert layer LayerShown layerStates }
            LayerHidden                                        -> return   st
            _                                                  -> return $ st { layerStates = Map.insert layer LayerHidden layerStates }

    {-ChangeLayerState layer | layerStates Map.! layer == Vector -> do
        log . pack $ "hiding layer " <> show layer
        return $ st { layerStates = Map.insert layer LayerHidden layerStates }

    ChangeLayerState layer | layerStates Map.! layer == WMTS && hasVectorData st layer -> do
        log . pack $ "already got layer " <> show layer
        return st { layerStates = Map.insert layer Vector layerStates }

    ChangeLayerState layer | layerStates Map.! layer == WMTS -> do
        log . pack $ "retrieving vector data for layer " <> show layer <> " on level " <> show zoomLevel <> " for region " <> show region
        fetchVectorLayers (Map.findWithDefault [] layer tileCache) zoomLevel region layer
        return st { layerStates = Map.insert layer VectorFetching layerStates }

    ChangeLayerState layer -> do
        log . pack $ "Showing WMTS layer " <> show layer
        return st { layerStates = Map.insert layer WMTS layerStates }-}

    VectorFetchFailed layer _ _ ->
        return st { layerStates = Map.insert layer LayerHidden layerStates }

    VectorFetchSucceeded diagram layer tile features ->
        return st { layerDataMap     = if diagram     then layerDataMap     else Map.adjust (\old -> old <> fmap toFeature (toList features)) layer layerDataMap
                  , layerDataDiagram = if not diagram then layerDataDiagram else Map.adjust (\old -> old <> fmap toFeature (toList features)) layer layerDataDiagram
                  , tileCacheMap     = if diagram     then tileCacheMap     else Map.adjust (tile :) layer tileCacheMap
                  , tileCacheDiagram = if not diagram then tileCacheDiagram else Map.adjust (tile :) layer tileCacheDiagram
                  }

    RegionChangeComplete newViewport -> do
        let keys = Map.keys . Map.filterWithKey (\layer state -> state /= LayerHidden && vectorVisible zoomLevel layer state) $ layerStates
        -- TODO: don't fetch both presentations
        forM_ keys (\layer -> fetchVectorLayers (Map.findWithDefault [] layer tileCacheDiagram) True zoomLevel newViewport layer)
        forM_ keys (\layer -> fetchVectorLayers (Map.findWithDefault [] layer tileCacheMap) False zoomLevel newViewport layer)
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