{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ExistentialQuantification        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE DataKinds        #-}
module InfraStyles where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Char (toLower)
import           Data.Maybe
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Tuple (fst)
import           Data.Typeable

import           GHC.Generics                   ( Generic )
import           GHC.TypeLits
import           Infra
import           InfraData
import           LayerTypes
import           Maps.Circle
import           Maps.Polygon
import           Maps.Polyline
import           Numeric (readHex)
import           Numeric.Natural
import           Prelude (Int, (<=), dropWhile, error, mempty, read, (.), (/=), (==), String, Maybe, Bool(..), ($), Double, Show, Eq, Ord, filter, fmap, pure, fail, show, tail, takeWhile, undefined)
import           React.Flux.Rn.Properties (Props)
import           React.Flux.Rn.Views
import           Svg.SvgXml

rotStyle :: Double -> Props SvgXml handler
rotStyle rotaatio = style [transform [Rotate $ Deg rotaatio]]



lineStyle :: InfraProperties -> [Props Polyline handler]
lineStyle (TrafficControlAreasF TrafficControlAreasData{ vari })     = [strokeWidth 5, strokeColor (parseColor vari)]
lineStyle (SpeedRestrictionAreasF _)                                 = [strokeWidth 1, strokeColor (Rgb 89 161 183)]
lineStyle (RailwaysF _)                                              = [strokeWidth 1, strokeColor (Rgb 255 42 42)]
lineStyle (StationIntervalsF _)                                      = [strokeWidth 3, strokeColor (Rgba 42 42 255 0.3)]
lineStyle (AudioFrequencyTrackCircuitsF _)                           = [strokeWidth 1, strokeColor (Rgba 255 0 0 0.5)]
lineStyle (AxleCountingSectionsF _)                                  = [strokeWidth 1, strokeColor (Rgba 0 255 0 0.5)] 
lineStyle (TrackCircuitsF _)                                         = [strokeWidth 1, strokeColor (Rgba 0 0 255 0.5)]
lineStyle (TracksF _)                                                = [strokeWidth 1, strokeColor (Rgb 0 0 0)]
lineStyle (TunnelsF _)                                               = [strokeWidth 1, strokeColor (Rgb 0 128 0)]
lineStyle (BridgesF _)                                               = [strokeWidth 1, strokeColor (Rgb 128 0 0)]
lineStyle (PlatformsF _)                                             = [strokeWidth 1, strokeColor (Rgb 0 0 128)]
lineStyle (ElectrificationGroupsF ElectrificationGroupsData{ vari }) = [strokeWidth 3, strokeColor (parseColor vari)]
lineStyle x                                                          = error $ "Not implemented lineStyle: " <> show x

parseColor :: String -> Color
parseColor [r1,r2,g1,g2,b1,b2] = let [(r,_)] = readHex [r1,r2]
                                     [(g,_)] = readHex [g1,g2]
                                     [(b,_)] = readHex [b1,b2]
                                 in Rgba (read $ show r) (read $ show g) (read $ show b) 0.5

polygonStyle :: InfraProperties -> [Props Polygon handler]
polygonStyle (AccountingRailwaySectionsF _)   = [strokeWidth 1, strokeColor (Rgb 0 0 128), fillColor (Rgba 0 0 128 0.1)]
polygonStyle (TransportationPlanningAreasF _) = [strokeWidth 1, strokeColor (Rgb 0 128 0), fillColor (Rgba 0 128 0 0.1)]
polygonStyle (StationsF _)                    = [strokeWidth 1, strokeColor (Rgb 0 0 255), fillColor (Rgba 0 0 128 0.5)]
polygonStyle x                                = error $ "Not implemented polygonStyle: " <> show x

layerIcon :: LatLng -> InfraProperties -> ReactElementM handler ()
layerIcon coordinate PositioningMarkersF{  } = undefined -- TODO
layerIcon coordinate MilestonesF{  }         = svgXml [ xml "icons/kilometrimerkki" ]
layerIcon coordinate TimetableLocationsF{  } = circle [ center coordinate
                                                      , radius 5
                                                      , strokeColor (Rgba 128 128 128 0.3)
                                                      , fillColor (Rgba 128 128 128 0.3)
                                                      ]
layerIcon coordinate PartsOfStationF{  }     = circle [ center coordinate
                                                      , radius 3
                                                      , strokeColor (Rgba 42 42 255 0.6)
                                                      , fillColor (Rgba 42 42 255 0.6)
                                                      ]
layerIcon coordinate LineSwitchesF{  }       = circle [ center coordinate
                                                      , radius 5
                                                      , strokeColor (Rgba 255 42 42 0.3)
                                                      , fillColor (Rgba 255 42 42 0.3)
                                                      ]
layerIcon coordinate StopsF{  }              = circle [ center coordinate
                                                      , radius 5
                                                      , strokeColor (Rgba 42 255 42 0.3)
                                                      , fillColor (Rgba 42 255 42 0.3)
                                                      ]
layerIcon coordinate StationsF{  }           = circle [ center coordinate
                                                      , radius 5
                                                      , strokeColor (Rgba 42 42 255 0.3)
                                                      , fillColor (Rgba 42 42 255 0.3)
                                                      ]
layerIcon coordinate (AxleCountersF AxleCountersData{ rotaatio })                                                                           = svgXml [ xml "icons/akselinlaskija"          , rotStyle rotaatio ]
layerIcon coordinate (RailInsulationsF RailInsulationsData{ rotaatio })                                                                     = svgXml [ xml "icons/raideeristys"            , rotStyle rotaatio ]
layerIcon coordinate (LevelCrossingsF LevelCrossingsData{  })                                                                               = svgXml [ xml "icons/tasoristeys" {-TODO: , rotStyle rotaatio-} ]
layerIcon coordinate (ElectricalWorkInsulatorsF ElectricalWorkInsulatorsData{ rotaatio })                                                   = svgXml [ xml "icons/tyonaikaineneristin"     , rotStyle rotaatio ]
layerIcon coordinate (GroundingDevicesF GroundingDevicesData{ rotaatio })                                                                   = svgXml [ xml "icons/maadoitin"               , rotStyle rotaatio ]
layerIcon coordinate (SeparationFieldsF SeparationFieldsData{ rotaatio })                                                                   = svgXml [ xml "icons/erotuskentta"            , rotStyle rotaatio ]
layerIcon coordinate (SeparationSectionsF SeparationSectionsData{ rotaatio })                                                               = svgXml [ xml "icons/erotusjakso"             , rotStyle rotaatio ]
layerIcon coordinate (ElectrificationEndsF ElectrificationEndsData{ rotaatio })                                                             = svgXml [ xml "icons/sahkoistyspaattyy"       , rotStyle rotaatio ]
layerIcon coordinate (GroupingInsulatorsF GroupingInsulatorsData{ rotaatio, ryhmityseristin=RyhmityseristinE{ nopeastiAjettava = True }})   = svgXml [ xml "icons/ryhmityseristinNopea"    , rotStyle rotaatio ]
layerIcon coordinate (GroupingInsulatorsF GroupingInsulatorsData{ rotaatio, ryhmityseristin=RyhmityseristinE{ nopeastiAjettava = False }})  = svgXml [ xml "icons/ryhmityseristin"         , rotStyle rotaatio ]
layerIcon coordinate (PantographMonitoringCamerasF PantographMonitoringCamerasData{ rotaatio })                                             = svgXml [ xml "icons/virroitinvalvontakamera" , rotStyle rotaatio ]
layerIcon coordinate (RFIDReadersF RFIDReadersData{ rotaatio })                                                                             = svgXml [ xml "icons/rfidlukija"              , rotStyle rotaatio ]
layerIcon coordinate (WheelForceIndicatorsF WheelForceIndicatorsData{ rotaatio })                                                           = svgXml [ xml "icons/pyoravoimailmaisin"      , rotStyle rotaatio ]
layerIcon coordinate (HotboxDetectorsF HotboxDetectorsData{ rotaatio })                                                                     = svgXml [ xml "icons/kuumakayntiilmaisin"     , rotStyle rotaatio ]
layerIcon coordinate (StopBoardsF StopBoardsData{ rotaatio })                                                                               = svgXml [ xml "icons/seislevy"                , rotStyle rotaatio ]
layerIcon coordinate (DerailersF DerailersData{ raiteensulku=RaiteensulkuE{ kasinAsetettava=True }, rotaatio })                             = svgXml [ xml "icons/raiteensulkuKasin"       , rotStyle rotaatio ]
layerIcon coordinate (DerailersF DerailersData{ raiteensulku=RaiteensulkuE{ kasinAsetettava=False }, rotaatio })                            = svgXml [ xml "icons/raiteensulku"            , rotStyle rotaatio ]
layerIcon coordinate (StationBoundaryMarksF StationBoundaryMarksData{ rotaatio })                                                           = svgXml [ xml "icons/liikennepaikanraja"      , rotStyle rotaatio ]
layerIcon coordinate (BalisesF BalisesData{ baliisi=BaliisiE{ toistopiste=False }, rotaatio })                                              = svgXml [ xml "icons/baliisi"                 , rotStyle rotaatio ]
layerIcon coordinate (BalisesF BalisesData{ baliisi=BaliisiE{ toistopiste=True } , rotaatio })                                              = svgXml [ xml "icons/baliisitoistopiste"      , rotStyle rotaatio ]
layerIcon coordinate (SignalsF SignalsData{ opastin=OpastinE{ puoli, tyyppi }    , rotaatio })                                              = svgXml [ xml ("icons/opastin_" <> tyyppi)    , rotStyle rotaatio , originY (case puoli of Vasen -> 5; Oikea -> -5) ]
layerIcon coordinate (BuffersF BuffersData{ rotaatio })                                                                                     = svgXml [ xml "icons/puskin"                  , rotStyle rotaatio ]
layerIcon coordinate (SwitchesF SwitchesData{rotaatio, vaihde=VaihdeE{ tyyppi, kasikaantoisyys, risteyssuhde }})                            = svgXml [ xml ("icons/" <> vaihdeIcon tyyppi kasikaantoisyys risteyssuhde ), rotStyle rotaatio ]
layerIcon coordinate x                                                                                                                      = error $ "Not implemented layerIcon: " <> show x

vaihdeIcon tyyppi kasikaantoisyys risteyssuhde =  "vaihde_"
                                               <> tyyppi
                                               <> (if kasikaantoisyys == Ei then "" else "_" <> printKasikaantoisyys kasikaantoisyys)
                                               <> (case risteyssuhde of
                                                     Nothing -> ""
                                                     Just rr -> if parseRisteyssuhde rr <= 10.0 then "" else if parseRisteyssuhde rr <= 18 then "_keskinopea" else "_nopea")

parseRisteyssuhde = read . fmap (\c -> if c == ',' then '.' else c) . tail . dropWhile (/= ':')

printKasikaantoisyys Ei       = ""
printKasikaantoisyys Kokonaan = "kokonaan"
printKasikaantoisyys Vasen_   = "vasen"
printKasikaantoisyys Oikea_   = "oikea"