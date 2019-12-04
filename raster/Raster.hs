{-# LANGUAGE OverloadedStrings #-}

module Raster where

import Graphics.Svg
import Data.Text
import Prelude as P
import Debug.Trace

-- helper function for Text type
showI :: Int -> Text
showI i = pack (show i)

-- helper fucntion for RealFloat type
showR :: (Show a, RealFloat a) => a -> Text
showR r = pack (show r)

-- helper function for rad
degToRad :: RealFloat a => a -> a 
degToRad d      = d * pi / 180

-- document root
svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [ Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200" ]

-- SVG element that can be used to hide some parts of another element.  In this
-- case we want to have boundary boxes for all the stuff.
clipPath :: Element
clipPath = clipPath_ [
        Id_ <<- "clip"
        , ClipPathUnits_ <<- "userSpaceOnUse" 
    ] $ rect_ [
        Width_ <<- "30"
        , Height_ <<- "30"
    ]

-- generates the bounding box positions on the page
boxCoordinates :: (Enum a, RealFloat a) => [(a, a)]
boxCoordinates = [ (x,y) | y<-range, x<-range ]
    where
        range = [ 15, 50 .. 155 ]

-- simple raster dot
rasterCircle :: (Show a, RealFloat a) => a -> (a, a) -> Element
rasterCircle factor (x, y) = circle_ [
        Cx_ <<- showR x
        , Cy_ <<- showR y
        , R_ <<- (showR $ (*) factor $ sqrt $ x**2 + y**2)
    ]

-- one rasterization using circles on a rectangular grid
quadRaster :: (Enum a, Show a, RealFloat a) => (a, (a, a)) -> Element
quadRaster (stepsize, (x, y)) = g_ [
            Transform_ <<- translate x y
            , Clip_path_ <<- "url(#clip)"
        ] $ mconcat $ P.map (rasterCircle (0.015 * stepsize)) coordSpace
    where
        range = [ 1, (1+stepsize) .. 31 ]
        coordSpace = [ (x,y) | x<-range, y<-range ]

-- Whole page for quadRaster
quadRasterResult :: Element
quadRasterResult = svg $ clipPath <> (mconcat $ P.map quadRaster $ P.zip [ 1.0,1.1..3.4 ] boxCoordinates)


-- one simple dot which is a hexagon
hexDot :: (Show a, RealFloat a) => a -> (a, a) -> Element
hexDot factor (x, y) = path_ [
        D_ <<- (
            mA x (y -size)
            <> lA (x + xDist) (y - yDist)
            <> lA (x + xDist) (y + yDist)
            <> lA x (y + size)
            <> lA (x - xDist) (y + yDist)
            <> lA (x - xDist) (y - yDist)
            <> lA x (y - size)
            <> z
        )
    ] where
        -- size = (*) factor $ max 30.0 $ sqrt $ x**2 + y**2
        maxDist = (*) 2.0 $ sqrt $ 30**2 + 30**2
        size = (*) factor $ min 0.505 $ (sqrt $ x**2 + y**2) / maxDist
        rad30 = degToRad 30
        xDist = (*) size $ cos rad30
        yDist = (*) size $ sin rad30

-- raster of hexagons on a hex layout
hexRaster :: (Enum a, Show a, RealFloat a) => (a, (a, a)) -> Element
hexRaster (size, (x, y)) = clipPath <> (g_ [
            Transform_ <<- translate x y
            , Clip_path_ <<- "url(#clip)"
        ] $ mconcat $ P.map (hexDot size) coordSpace)
    where
        height = size
        width = (*) size $ cos $ degToRad 30
        maxCenter = 31
        xrange1 = [ 0, width .. maxCenter ]
        xrange2 = [ (0.5 * width), (1.5 * width) .. maxCenter ]
        yrange1 = [ (0.5 * height), (2.0 * height) .. maxCenter ]
        yrange2 = [ (1.25 * height), (2.75 * height) .. maxCenter ]
        coordSpace = [ (x,y) | x<-xrange1, y<-yrange1 ] ++ [ (x,y) | x<-xrange2, y<-yrange2 ]

-- whole page for hexRaster
hexRasterResult :: Element
hexRasterResult = svg $
    mconcat $ P.map hexRaster $ P.zip (P.take 25 [ 2.0,2.20.. ]) boxCoordinates

-- simple raster dot in the shape of a triangle
triangleDot :: (Show a, RealFloat a) =>
        a
        -> ((a, a) -> a)
        -> (a, a)
        -> Element
triangleDot size getBrightness (x, y) = path_ [
        D_ <<- if (f < size) then (
            -- white square with a black triangle
            mA x y
            <> lA (x + f) y
            <> lA x (y + f)
            <> lA x y
            <> z
        ) else (
            -- black square with a white triangle cut out
            mA x y
            <> lA (x + size) y
            <> lA (x + size) (y + f')
            <> lA (x + f') (y + size)
            <> lA x (y + size)
            <> lA x y
            <> z
        )
        , Fill_ <<- "black"
        , Stroke_ <<- "black"
        , Stroke_width_ <<- "0.01px"
    ]
    where
        f = (*) (2 * size) $ getBrightness (x / 30, y / 30)
        f' = f - size

-- raster for triangle dots in a rectangular layout
triangleRaster :: (Enum a, Show a, RealFloat a) =>
        ((a, a) -> a)
        -> (a, (a, a))
        -> Element
-- triangleRaster getBrightness (size, (x, y)) = clipPath <> (g_ [
triangleRaster getBrightness (size, (x, y)) = (g_ [
            Transform_ <<- translate x y
            , Clip_path_ <<- "url(#clip)"
        ] $ mconcat $ P.map (triangleDot size getBrightness) coordSpace)
    where
        range = [ 1, (1+size) .. 31 ]
        coordSpace = [ (x,y) | x<-range, y<-range ]

-- whole page for triangleRaster
triangleRasterResult :: (Show a, Enum a, RealFloat a) => ((a, a) -> a) -> Element
triangleRasterResult getBrightness = svg
    $ (mconcat
        $ P.map (triangleRaster getBrightness)
        $ P.zip (P.take 25 [ 1.0,1.20.. ]) boxCoordinates)

