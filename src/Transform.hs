{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Transform where

import Control.DeepSeq
import Data.List (intercalate)
import GHC.Generics
import Maps.Types (LatLng(..),Region(..))
import Numeric.Natural
import Prelude (Double, Show, Eq, Maybe(..), Bool, ($), fmap, show, (-), (/), (+), (<$>), filter, (<=), (&&), (>))

data BBox = BBox {
    x1 :: Double,
    y1 :: Double,
    x2 :: Double,
    y2 :: Double
} deriving (Show, Eq, Generic, NFData)

type Tile = BBox

bbox2string (BBox x1 y1 x2 y2) = (intercalate "," $ fmap show [x1, y1, x2, y2])

region2bbox (Region latitude longitude (Just latitudeDelta) (Just longitudeDelta)) = (LatLng (latitude - latitudeDelta/2) (longitude - longitudeDelta/2), LatLng (latitude + latitudeDelta/2) (longitude + longitudeDelta/2))
region2bbox (Region latitude longitude Nothing Nothing)                            = (LatLng latitude longitude, LatLng latitude longitude)

tile2bbox (LatLng latLL1 lonLL1, LatLng latTR1 lonTR1) = BBox latLL1 lonLL1 latTR1 lonTR1

region2Tiles:: Natural -> Region -> [Tile]
region2Tiles level region = tile2bbox <$> filter (anyinteract (region2bbox region)) (tilesForLevel level)

anyinteract :: (LatLng,LatLng) -> (LatLng,LatLng) -> Bool
anyinteract (LatLng latLL1 lonLL1, LatLng latTR1 lonTR1) (LatLng latLL2 lonLL2, LatLng latTR2 lonTR2) = latLL1 <= latTR2 && latLL2 <= latTR1 && lonLL1 <= lonTR2 && lonLL2 <= lonTR1


minLat = 59.8
maxLat = 70.1
minLng = 20.5
maxLng = 31.6
wgs84constraint = (LatLng minLat minLng, LatLng maxLat maxLng)

cornersForLevel level | level <= 9               = [LatLng lat lng | lat <- [minLat, minLat+2 .. maxLat], lng <- [minLng, minLng+2 .. maxLng]]
cornersForLevel level | level > 9 && level <= 13 = [LatLng lat lng | lat <- [minLat, minLat+0.5 .. maxLat], lng <- [minLng, minLng+0.5 .. maxLng]]
cornersForLevel _                                = [LatLng lat lng | lat <- [minLat, minLat+0.1 .. maxLat], lng <- [minLng, minLng+0.1 .. maxLng]]

tilesForLevel level | level <= 9               = fmap (\ll@(LatLng a b) -> (ll, LatLng (a+2)   (b+2)))   (cornersForLevel level)
tilesForLevel level | level > 9 && level <= 13 = fmap (\ll@(LatLng a b) -> (ll, LatLng (a+0.5) (b+0.5))) (cornersForLevel level)
tilesForLevel level                            = fmap (\ll@(LatLng a b) -> (ll, LatLng (a+0.1) (b+0.1))) (cornersForLevel level)