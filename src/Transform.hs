{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Transform where

import Data.Aeson
import Data.List (intercalate)
import Maps.Types (LatLng(..),Region(..))
import Numeric.Natural

type Tile = (Natural,Int,Int)

wgs84bbox = BBox (-20037508.3427892) (-20037508.3427892) 20037508.3427892 20037508.3427892
wgs84bounds = (LatLng (-90) (-180), LatLng 90 180)
wgs84constraint = (LatLng (59.8) (20.5), LatLng (70.1) (31.6))

tilesOnLevelRow :: Natural -> Int
tilesOnLevelRow level = 2 ^ level

between min max = (&&) <$> (>= min) <*> (<= max)

wgs84_pixsize_extent :: [Double]
wgs84_pixsize_extent =
    [ 156543.0339280410 -- 0
    , 78271.51696402048 -- 1
    , 39135.75848201023 -- 2
    , 19567.87924100512 -- 3
    , 9783.939620502561 -- 4
    , 4891.969810251280 -- 5
    , 2445.984905125640 -- 6
    , 1222.992452562820 -- 7
    , 611.4962262814100 -- 8
    , 305.7481131407048 -- 9
    , 152.8740565703525 -- 10
    , 76.43702828517624 -- 11
    , 38.21851414258813 -- 12
    , 19.10925707129406 -- 13
    , 9.554628535647032 -- 14
    , 4.777314267823516 -- 15
    , 2.388657133911758 -- 16
    , 1.194328566955879 -- 17
    , 0.5971642834779395 -- 18
    ]

metresOnLevelTile :: Natural -> Double
metresOnLevelTile level = (wgs84_pixsize_extent !! fromIntegral level) * 256

data BBox = BBox {
    x1 :: Double,
    y1 :: Double,
    x2 :: Double,
    y2 :: Double
} deriving Show

bbox2string (BBox x1 y1 x2 y2) = (intercalate "," $ fmap (show . round) [x1, y1, x2, y2]) <> ",epsg:3857"

relevantTileBounds :: Natural -> ((Int,Int),(Int,Int))
relevantTileBounds  0 = ((0,0),(0,0))
relevantTileBounds  1 = ((0,1),(1,1))
relevantTileBounds  2 = ((0,1),(0,1))
relevantTileBounds  3 = ((1,2),(1,2))
relevantTileBounds  4 = ((3,4),(8,9))
relevantTileBounds  5 = ((7,9),(17,18))
relevantTileBounds  6 = ((14,18),(35,37))
relevantTileBounds  7 = ((28,37),(71,75))
relevantTileBounds  8 = ((57,74),(142,150))
relevantTileBounds  9 = ((114,149),(285,300))
relevantTileBounds 10 = ((228,298),(570,601))
relevantTileBounds 11 = ((456,596),(1140,1203))
relevantTileBounds 12 = ((913,1193),(2281,2407))
relevantTileBounds 13 = ((1827,2387),(4563,4814))
relevantTileBounds 14 = ((3654,4774),(9127,9629))
relevantTileBounds 15 = ((7308,9548),(18254,19259))
relevantTileBounds 16 = ((14617,19096),(36508,38518))
relevantTileBounds 17 = ((29235,38193),(73017,77036))
relevantTileBounds 18 = ((58470,76387),(146035,154072))

levelTiles :: Natural -> [(LatLng,LatLng,Int,Int)]
levelTiles level = let
    ((LatLng latLL lonLL),(LatLng latTR lonTR)) = wgs84bounds
    tiles = tilesOnLevelRow level
    stepX = abs (lonTR - lonLL) / fromIntegral tiles
    stepY = abs (latTR - latLL) / 2.0 / fromIntegral tiles
    ((rowMin,rowMax),(colMin,colMax)) = relevantTileBounds level
    in
    [ (LatLng (latTR - fromIntegral (y+1) * stepY) (lonLL + fromIntegral x * stepX), LatLng (latTR - fromIntegral y * stepY) (lonLL + fromIntegral x * stepX + stepX), y, x) | y <- [rowMin..rowMax], x <- [colMin..colMax] ]

region2Tiles :: Natural -> Region -> [Tile]
region2Tiles level region = fmap (\(_,_,y,x) -> (level,y,x)) $ filter (\(ll,tr,_,_) -> anyinteract (region2bbox region) (ll,tr)) (levelTiles level)

anyinteract :: (LatLng,LatLng) -> (LatLng,LatLng) -> Bool
anyinteract (LatLng latLL1 lonLL1, LatLng latTR1 lonTR1) (LatLng latLL2 lonLL2, LatLng latTR2 lonTR2) = latLL1 <= latTR2 && latLL2 <= latTR1 && lonLL1 <= lonTR2 && lonLL2 <= lonTR1

tile2bbox :: Tile -> BBox
tile2bbox (matrix,row,col) = let
    BBox x1 _ _ y2 = wgs84bbox
    metresOnTile = metresOnLevelTile matrix
    
    x = x1 + (metresOnTile * fromIntegral col)
    y = y2 - (metresOnTile * fromIntegral row)
    in
    BBox x (y - metresOnTile) (x + metresOnTile) y

region2bbox (Region latitude longitude (Just latitudeDelta) (Just longitudeDelta)) = (LatLng (latitude - latitudeDelta/2) (longitude - longitudeDelta/2), LatLng (latitude + latitudeDelta/2) (longitude + longitudeDelta/2))
region2bbox (Region latitude longitude Nothing Nothing)                            = (LatLng latitude longitude, LatLng latitude longitude)