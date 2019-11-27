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

quadRasterPoint :: (Show a, RealFloat a) => (a, a) -> Element
quadRasterPoint (x, y) = circle_ [
        Cx_ <<- showR x
        , Cy_ <<- showR y
        , R_ <<- (showR $ (*) 0.01 $ sqrt $ x**2 + y**2)
    ]

quadRaster :: (Show a, RealFloat a) => (a, a) -> Element
quadRaster (x, y) = g_ [
        X_ <<- showR x
        , Y_ <<- showR y
    ] $ mconcat $ P.map quadRasterPoint [ (x,y) | x<-[ 1,2..45 ], y<-[ 1,2..45 ] ]

main :: IO ()
main = do
    print $ svg $ 
        quadRaster (10, 10)
        <> mask

