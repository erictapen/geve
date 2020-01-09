{-# LANGUAGE OverloadedStrings #-}

import Graphics.Svg
import Data.Text

boxSize :: RealFloat a => a
boxSize = 30

showI :: Int -> Text
showI i = pack (show i)

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

-- 30*30
basicRect :: RealFloat a => (a, a) -> Element
basicRect (x, y) = g_ [
        Transform_ <<- translate x y
    ]
    $ rect_ [
        Width_ <<- "30",
        Height_ <<- "30",
        Fill_ <<- "black",
        Stroke_ <<- "black" 
    ]

basicRectGrid :: Element
basicRectGrid = mconcat $
    Prelude.map basicRect [ (x,y) | x<-[ 15, 50 .. 155 ], y<-[ 15, 50 .. 155  ] ]

randomQuad :: RealFloat a => (a, a) -> Element
randomQuad (x, y) = g_ [
        Transform_ <<- translate x y
    ] $ path_ [
        D_ <<- (
            mA 0 0
            <> lA 0 10
            <> lA 10 10
            <> lA 10 0
            <> z
        ),
        Fill_ <<- "white",
        Stroke_ <<- "none"
    ]

main :: IO ()
main = do
    print $ svg $
        basicRectGrid <> randomQuad (15, 15)

