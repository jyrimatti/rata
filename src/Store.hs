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
import qualified Data.Map                      as Map
import           Data.Sequence                 as Seq
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           GHCJS.Fetch
import           Infra
import           Layer
import           LayerTypes
import Maps.Types (Region(..))
import           Numeric.Natural

import           Prelude                        ( Show
                                                , Eq
                                                , pure
                                                , fmap
                                                , ($)
                                                , return
                                                , not
                                                , show
                                                , Either(..)
                                                , (<>)
                                                , concatMap
                                                , (==)
                                                , (&&)
                                                )
import           React.Flux                     ( StoreData(..), action, SomeStoreAction )
import           React.Flux.Ajax                ( jsonAjax, RequestTimeout(..) )
import           React.Flux.Rn.APIs             ( log )

hasVectorData (AppState _ layerData _) layer = Map.member layer layerData

data AppState = AppState {
    layerStates :: Map.Map Layer LayerState,
    layerData :: Map.Map Layer [GeospatialGeometry],
    zoomLevel :: Natural
} deriving (Show, Typeable, Eq)

data AppAction = ChangeLayerState Layer
               | VectorFetchFailed Layer
               | VectorFetchSucceeded Layer (Seq (GeoFeature ())) 
               | RegionChangeComplete Region
    deriving (Show, Typeable, Generic, NFData, Eq)

featureGeometries (GeoFeature _ (Collection geom) _ _) = toList geom
featureGeometries (GeoFeature _             geom  _ _) = [geom]

disp :: AppAction -> [SomeStoreAction]
disp a = [action @AppState a]

instance StoreData AppState where
    type StoreAction AppState = AppAction
    transform action st@(AppState layerStates layerData zoomLevel) = case action of
        ChangeLayerState layer | layerStates Map.! layer == Vector -> do
            log $ "hiding layer " <> pack (show layer)
            return $ st { layerStates = Map.insert layer LayerHidden $ layerStates }
        
        ChangeLayerState layer | layerStates Map.! layer == WMTS && hasVectorData st layer -> do
            log $ "already got layer " <> pack (show layer)
            return st { layerStates = Map.insert layer Vector $ layerStates }
        
        ChangeLayerState layer | layerStates Map.! layer == WMTS -> do
            log $ "retrieving vector data for layer " <> pack (show layer)
            jsonAjax NoTimeout "GET" (T.pack $ vectorUrl apiBase $ let LayerType x = layerType layer in layerPath x) [] () $ \case
                (_, Left msg)                                -> return $ disp $ VectorFetchFailed layer
                (_, Right (GeoFeatureCollection _ features)) -> return $ disp $ VectorFetchSucceeded layer features
            return st { layerStates = Map.insert layer VectorFetching $ layerStates }

        VectorFetchFailed layer -> do
            log $ "Failed retrieving layer " <> pack (show layer)
            return st { layerStates = Map.insert layer LayerHidden $ layerStates }

        VectorFetchSucceeded layer features -> do
            log $ "Deserialized geojson"
            let ret = st { layerStates = Map.insert layer Vector $ layerStates
                            , layerData = Map.insert layer (concatMap featureGeometries features) layerData
                            }
            log $ "Decoded features"
            return ret

        RegionChangeComplete region -> do
            log $ pack $ ("Region changed: " <> show region)
            return st

            {-resp <- fetch $ Request
                (pack $ vectorUrl apiBase $ case layerType layer of
                    LayerType x -> layerPath x
                )
                defaultRequestOptions
            log $ "received response"
            value <- responseJSON resp `catch` \(e :: JSPromiseException) -> do
                log $ "Exception decoding response: " <> pack (show e)
                return $ A.String "meh..."
            log $ "response was proper json"
            case A.fromJSON value of
                A.Success (GeoFeatureCollection _ features) -> do
                    log $ "Deserialized geojson"
                    let
                        ret = st
                            { layerStates = Map.insert layer Vector $ layerStates st
                            , layerData   =
                                Map.insert
                                        layer
                                        (concatMap
                                            (\case
                                                (GeoFeature _ (Collection geom) (_ :: A.Value) _) -> toList geom
                                                (GeoFeature _             geom  (_ :: A.Value) _) -> [geom]
                                            )
                                            features
                                        )
                                    $ layerData st
                            }
                    log $ "Decoded geometry: " <> pack (show ret)
                    return ret
                A.Error e -> do
                    log $ "Error decoding geometry: " <> pack e
                    return st-}
        
        ChangeLayerState layer -> do
            log $ "Showing WMTS layer " <> pack (show layer)
            return st { layerStates = Map.insert layer WMTS $ layerStates }

appStore = AppState (Map.fromList $ fmap (, LayerHidden) allLayers) Map.empty 12
