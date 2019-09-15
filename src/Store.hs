{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeFamilies                 #-}
module Store where

import           Control.DeepSeq
import           Control.Exception              ( catch )
import qualified Data.Aeson                    as A
import           Data.Foldable                  ( toList )
import           Data.Geospatial
import           Data.JSString                  ( pack )
import qualified Data.Map                      as Map
import           Data.Sequence                 as Seq
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           GHCJS.Fetch
import           Infra
import           Layer
import           LayerTypes
import           Numeric.Natural

import           Prelude                        ( Show
                                                , Eq
                                                , pure
                                                , fmap
                                                , ($)
                                                , return
                                                , not
                                                , show
                                                , (<>)
                                                , concatMap
                                                , (==)
                                                , (&&)
                                                )
import           React.Flux                     ( StoreData(..) )
import           React.Flux.Rn.APIs             ( log )

hasVectorData (AppState _ layerData _) layer = not $ null $ layerData Map.! layer

data AppState = AppState {
    layerStates :: Map.Map Layer LayerState,
    layerData :: Map.Map Layer (Seq GeoLine),
    zoomLevel :: Natural
} deriving (Show, Typeable, Eq)

data AppAction = ChangeLayerState Layer
    deriving (Show, Typeable, Generic, NFData, Eq)

instance StoreData AppState where
    type StoreAction AppState = AppAction
    transform action st = case action of
        ChangeLayerState layer | layerStates st Map.! layer == Vector -> do
            log $ "hiding layer " <> pack (show layer)
            return $ st { layerStates = Map.insert layer LayerHidden $ layerStates st
                        }
        ChangeLayerState layer
            | layerStates st Map.! layer == WMTS && hasVectorData st layer -> do
                log $ "already got layer " <> pack (show layer)
                return st
                    { layerStates = Map.insert layer Vector $ layerStates st
                    }
        ChangeLayerState layer | layerStates st Map.! layer == WMTS -> do
            log $ "retrieving vector data for layer " <> pack (show layer)
            resp <- fetch $ Request
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
                            { layerStates = Map.insert layer Vector
                                                $ layerStates st
                            , layerData   =
                                Map.insert
                                        layer
                                        (fromList $ concatMap
                                            (\(GeoFeature _ (MultiLine ml) (_ :: A.Value) _) ->
                                                toList $ splitGeoMultiLine ml
                                            )
                                            features
                                        )
                                    $ layerData st
                            }
                    log $ "Decoded geometry: " <> pack (show ret)
                    return ret
                A.Error e -> do
                    log $ "Error decoding geometry: " <> pack e
                    return st

appStore = AppState (Map.fromList $ fmap (, LayerHidden) allLayers) Map.empty 5
