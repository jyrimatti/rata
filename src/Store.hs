{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE LambdaCase                   #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
module Store where

import           Control.DeepSeq
import           Control.Exception
import           Control.Exception              ( catch )
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
import           Maps.Types (Region(..))
import           Navigation.Navigation
import           Numeric.Natural
import           Prelude                        ( Show
                                                , Eq
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
import           React.Flux                     ( StoreData(..), action, SomeStoreAction )
import           React.Flux.Ajax                ( jsonAjax, RequestTimeout(..) )
import           React.Flux.Rn.APIs             ( log )
import           Transform

hasVectorData (AppState _ _ layerData _ _ _) layer = Map.member layer layerData

data AppState = AppState {
    layerMenu :: NavigationId,
    layerStates :: Map.Map Layer LayerState,
    layerData :: Map.Map Layer [GeospatialGeometry],
    zoomLevel :: Natural,
    viewport :: Region,
    tileCache :: Map.Map Layer [Tile]
} deriving (Show, Typeable, Eq)

data AppAction = SaveLayerMenu NavigationId
               | ChangeLayerState Layer
               | VectorFetchFailed Layer T.Text T.Text
               | VectorFetchSucceeded Layer Tile (Seq (GeoFeature (A.Value)))
               | RegionChangeComplete Region
               | ToggleLayerMenu
    deriving (Show, Typeable, Generic, NFData, Eq)

featureGeometries (GeoFeature _ (Collection geom) _ _) = toList geom
featureGeometries (GeoFeature _             geom  _ _) = [geom]

disp :: AppAction -> [SomeStoreAction]
disp a = [action @AppState a]

fetchVectorLayers tileCache level region layer = mapM_ (\bbox -> fetchVectorLayer bbox layer) $ filter (not . (`elem` tileCache)) $ region2Tiles level region

fetchVectorLayer tile layer = let
    url = T.pack $ vectorUrl2 apiBase (let LayerType x = layerType layer in (layerPath x)) (tile2bbox tile)
  in
    jsonAjax (TimeoutMilliseconds 180000) "GET" url [] () $ \case
        (_, Left msg)                                -> return $ disp $ VectorFetchFailed layer url msg
        (_, Right (GeoFeatureCollection _ features)) -> return $ disp $ VectorFetchSucceeded layer tile features

initialReg = Region 61.4858254 24.0470175 (Just 0.5) (Just 0.5)

appStore = AppState "" (Map.fromList $ fmap (, LayerHidden) allLayers) Map.empty 12 initialReg Map.empty

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

instance StoreData AppState where
    type StoreAction AppState = AppAction

    transform action st = catchAny (doTransform action st) $ \e -> do
        log . pack $ "Got an exception while performing action " <> show action <> ": " <> show e
        return st

doTransform action st@(AppState layerMenu layerStates layerData zoomLevel region tileCache) = case action of
    ChangeLayerState layer | layerStates Map.! layer == Vector -> do
        log . pack $ "hiding layer " <> show layer
        return $ st { layerStates = Map.insert layer LayerHidden $ layerStates }
    
    ChangeLayerState layer | layerStates Map.! layer == WMTS && hasVectorData st layer -> do
        log . pack $ "already got layer " <> show layer
        return st { layerStates = Map.insert layer Vector $ layerStates }
    
    ChangeLayerState layer | layerStates Map.! layer == WMTS -> do
        log . pack $ "retrieving vector data for layer " <> show layer
        fetchVectorLayers (Map.findWithDefault [] layer tileCache) zoomLevel region layer
        return st { layerStates = Map.insert layer VectorFetching $ layerStates }

    VectorFetchFailed layer url msg -> do
        log . pack $ "Failed retrieving layer " <> show layer <> " with url '" <> show url <> "': " <> show msg
        return st { layerStates = Map.insert layer LayerHidden $ layerStates }

    VectorFetchSucceeded layer tile features -> do
        log . pack $ "Received vector layer for tile " <> show tile
        let ret = st { layerStates = Map.insert layer Vector $ layerStates
                     , layerData = Map.adjust (\old -> old <> concatMap featureGeometries features) layer layerData
                     , tileCache = Map.adjust (\old -> tile:old) layer tileCache
                     }
        return ret

    RegionChangeComplete newViewport -> do
        log . pack $ "Viewport changed: " <> show newViewport
        let keys = Map.keys . Map.filterWithKey (\_ v -> v == Vector || v == VectorFetching) $ layerStates
        forM_ keys $ (\layer -> fetchVectorLayers (Map.findWithDefault [] layer tileCache) zoomLevel newViewport layer)
        return st { viewport = newViewport
                  , layerStates = Map.union (Map.fromList $ fmap (, VectorFetching) keys) layerStates }

    ChangeLayerState layer -> do
        log . pack $ "Showing WMTS layer " <> show layer
        return st { layerStates = Map.insert layer WMTS $ layerStates }

    ToggleLayerMenu -> do
        log "Toggling layer menu"
        drawerAction ToggleDrawer layerMenu
        return st

    SaveLayerMenu navigationId -> do
        log . pack $ "Save layer menu: " <> show navigationId
        return st { layerMenu = navigationId }