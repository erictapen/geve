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

boxCoordinates = [ (x,y) | x<-[ 10, 50, 90, 130 ], y<-[ 10, 50, 90, 130 ] ]

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

quadRasterPoint :: (Show a, RealFloat a) => a -> (a, a) -> Element
quadRasterPoint factor (x, y) = circle_ [
        Cx_ <<- showR x
        , Cy_ <<- showR y
        , R_ <<- (showR $ (*) factor $ sqrt $ x**2 + y**2)
    ]

quadRaster :: (Enum a, Show a, RealFloat a) => a -> (a, a) -> Element
quadRaster stepsize (x, y) = g_ [
            Transform_ <<- translate x y
        ] $ mconcat $ P.map (quadRasterPoint (0.015 * stepsize)) coordSpace
    where
        range = enumFromThenTo 1 (1+stepsize) 31
        coordSpace = [ (x,y) | x<-range, y<-range ]

main :: IO ()
main = do
    print $ svg $ 
           quadRaster 1   ( 10,  10)
        <> quadRaster 1.1 ( 50,  10)
        <> quadRaster 1.2 ( 90,  10)
        <> quadRaster 1.3 (130,  10)
        <> quadRaster 1.4 ( 10,  50)
        <> quadRaster 1.5 ( 50,  50)
        <> quadRaster 1.6 ( 90,  50)
        <> quadRaster 1.7 (130,  50)
        <> quadRaster 1.8 ( 10,  90)
        <> quadRaster 1.9 ( 50,  90)
        <> quadRaster 2.0 ( 90,  90)
        <> quadRaster 2.1 (130,  90)
        <> quadRaster 2.2 ( 10, 130)
        <> quadRaster 2.3 ( 50, 130)
        <> quadRaster 2.4 ( 90, 130)
        <> quadRaster 2.5 (130, 130)
        <> mask

