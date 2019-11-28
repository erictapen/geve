{-# LANGUAGE OverloadedStrings #-}

import Graphics.Svg
import Data.Text
import Prelude as P

showI :: Int -> Text
showI i = pack (show i)

showR :: (Show a, RealFloat a) => a -> Text
showR r = pack (show r)

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [ Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200" ]

maskSegment :: RealFloat a => (a, a) -> Text
maskSegment (x, y) = mA x y
    <> lA x (y+30)
    <> lA (x+30) (y+30)
    <> lA (x+30) y
    <> lA x y
    <> z

boxCoordinates = [ (x,y) | y<-range, x<-range ]
    where
        range = [ 15, 50 .. 155 ]

mask :: Element
mask = path_
    [
        D_ <<- (
            mA 0 0
            <> lA 0 200
            <> lA 200 200
            <> lA 200 0
            <> lA 0 0
            <> z
            <> " "
            <> (mconcat $ P.map maskSegment boxCoordinates)
        )
        , Stroke_ <<- "none"
        , Fill_ <<- "white"
        , Style_ <<- "fill-rule:evenodd"
    ]

rasterCircle :: (Show a, RealFloat a) => a -> (a, a) -> Element
rasterCircle factor (x, y) = circle_ [
        Cx_ <<- showR x
        , Cy_ <<- showR y
        , R_ <<- (showR $ (*) factor $ sqrt $ x**2 + y**2)
    ]

quadRaster :: (Enum a, Show a, RealFloat a) => (a, (a, a)) -> Element
quadRaster (stepsize, (x, y)) = g_ [
            Transform_ <<- translate x y
        ] $ mconcat $ P.map (rasterCircle (0.015 * stepsize)) coordSpace
    where
        range = [ 1, (1+stepsize) .. 31 ]
        coordSpace = [ (x,y) | x<-range, y<-range ]

quadRasterResult :: Element
quadRasterResult = svg $ (mconcat $ P.map quadRaster $ P.zip [ 1.0,1.1..3.4 ] boxCoordinates) <> mask


hexRaster :: (Enum a, Show a, RealFloat a) => (a, (a, a)) -> Element
hexRaster (size, (x, y)) = g_ [
            Transform_ <<- translate x y
        ] $ mconcat $ P.map (rasterCircle (0.015 * size)) coordSpace
    where
        xrange1 = [ 0, size .. 31 ]
        xrange2 = [ (0.5 * size), (1.5 * size) .. 31 ]
        yrange1 = [ (0.5 * size), (2.0 * size) .. 31 ]
        yrange2 = [ (1.25 * size), (2.75 * size) .. 31 ]
        coordSpace = [ (x,y) | x<-xrange1, y<-yrange1 ] ++ [ (x,y) | x<-xrange2, y<-yrange2 ]


hexRasterResult :: Element
hexRasterResult = svg $ (mconcat $ P.map hexRaster $ P.zip (P.take 25 [ 1.0,1.05.. ]) boxCoordinates) <> mask


triangleDot :: (Show a, RealFloat a) => a -> (a, a) -> Element
triangleDot size (x, y) = path_ [
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
        f = (*) (0.053 * size) $ sqrt $ x**2 + y**2
        f' = f - size

triangleRaster :: (Enum a, Show a, RealFloat a) => (a, (a, a)) -> Element
triangleRaster (size, (x, y)) = g_ [
            Transform_ <<- translate x y
        ] $ mconcat $ P.map (triangleDot size) coordSpace
    where
        range = [ 1, (1+size) .. 31 ]
        coordSpace = [ (x,y) | x<-range, y<-range ]

triangleRasterResult :: Element
triangleRasterResult = svg $ (mconcat $ P.map triangleRaster $ P.zip (P.take 25 [ 1.0,1.05.. ]) boxCoordinates) <> mask

main :: IO ()
main = do
    print triangleRasterResult

