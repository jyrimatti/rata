{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE LambdaCase                 #-}
module Store where

import           Control.DeepSeq
import           Control.Exception              ( catch )
import qualified Data.Aeson                    as A
import           Data.Foldable                  ( toList )
import           Data.Geospatial
import qualified Data.Text as T
import           Data.JSString                  ( pack )
import Data.Maybe
import qualified Data.Map                      as Map
import           Data.Sequence                 as Seq
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           GHCJS.Fetch
import           Infra
import Control.Monad (forM_)
import           Layer
import           LayerTypes
import Maps.Types (Region(..))
import           Numeric.Natural
import Navigation.Navigation
import           Prelude                        ( Show
                                                , Eq
                                                , pure
                                                , (||)
                                                , fmap
                                                , ($)
                                                , (-)
                                                , (+)
                                                , return
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
import Control.Exception
import           React.Flux                     ( StoreData(..), action, SomeStoreAction )
import           React.Flux.Ajax                ( jsonAjax, RequestTimeout(..) )
import           React.Flux.Rn.APIs             ( log )

hasVectorData (AppState _ _ layerData _ _) layer = Map.member layer layerData

data AppState = AppState {
    layerMenu :: NavigationId,
    layerStates :: Map.Map Layer LayerState,
    layerData :: Map.Map Layer [GeospatialGeometry],
    zoomLevel :: Natural,
    viewport :: Region
} deriving (Show, Typeable, Eq)

data AppAction = SaveLayerMenu NavigationId
               | ChangeLayerState Layer
               | VectorFetchFailed Layer T.Text
               | VectorFetchSucceeded Layer (Seq (GeoFeature (A.Value)))
               | RegionChangeComplete Region
               | ToggleLayerMenu
    deriving (Show, Typeable, Generic, NFData, Eq)

featureGeometries (GeoFeature _ (Collection geom) _ _) = toList geom
featureGeometries (GeoFeature _             geom  _ _) = [geom]

disp :: AppAction -> [SomeStoreAction]
disp a = [action @AppState a]

fetchVectorLayer region layer = 
    jsonAjax (TimeoutMilliseconds 25000) "GET" (T.pack $ vectorUrl apiBase (let LayerType x = layerType layer in (layerPath x)) region) [] () $ \case
        (_, Left msg)                                -> return $ disp $ VectorFetchFailed layer msg
        (_, Right (GeoFeatureCollection _ features)) -> return $ disp $ VectorFetchSucceeded layer features

initialReg = Region 61.4858254 24.0470175 (Just 0.5) (Just 0.5)

appStore = AppState "" (Map.fromList $ fmap (, LayerHidden) allLayers) Map.empty 12 initialReg

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

instance StoreData AppState where
    type StoreAction AppState = AppAction

    transform action st = catchAny (doTransform action st) $ \e -> do
        log . pack $ "Got an exception while performing action " <> show action <> ": " <> show e
        return st

doTransform action st@(AppState layerMenu layerStates layerData zoomLevel region) = case action of
    ChangeLayerState layer | layerStates Map.! layer == Vector -> do
        log $ "hiding layer " <> pack (show layer)
        return $ st { layerStates = Map.insert layer LayerHidden $ layerStates }
    
    ChangeLayerState layer | layerStates Map.! layer == WMTS && hasVectorData st layer -> do
        log $ "already got layer " <> pack (show layer)
        return st { layerStates = Map.insert layer Vector $ layerStates }
    
    ChangeLayerState layer | layerStates Map.! layer == WMTS -> do
        log $ "retrieving vector data for layer " <> pack (show layer)
        fetchVectorLayer region layer
        return st { layerStates = Map.insert layer VectorFetching $ layerStates }

    VectorFetchFailed layer msg -> do
        log $ "Failed retrieving layer " <> pack (show layer) <> ": " <> pack (show msg)
        return st { layerStates = Map.insert layer LayerHidden $ layerStates }

    VectorFetchSucceeded layer features -> do
        log $ "Deserialized geojson"
        let ret = st { layerStates = Map.insert layer Vector $ layerStates
                     , layerData = Map.insert layer (concatMap featureGeometries features) layerData
                     }
        log $ "Decoded features"
        return ret

    RegionChangeComplete newViewport -> do
        log $ pack $ ("Viewport changed: " <> show newViewport)
        let keys = Map.keys . Map.filterWithKey (\_ v -> v == Vector || v == VectorFetching) $ layerStates
        forM_ keys $ fetchVectorLayer newViewport
        return st { viewport = newViewport, layerStates = Map.union (Map.fromList $ fmap (, VectorFetching) keys) layerStates }

    ChangeLayerState layer -> do
        log $ "Showing WMTS layer " <> pack (show layer)
        return st { layerStates = Map.insert layer WMTS $ layerStates }

    ToggleLayerMenu -> do
        log "Toggling layer menu"
        drawerAction ToggleDrawer layerMenu
        return st

    SaveLayerMenu navigationId -> do
        log . pack $ "Save layer menu: " <> show navigationId
        return st { layerMenu = navigationId }