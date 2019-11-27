{-# LANGUAGE OverloadedStrings #-}

import Graphics.Svg
import Data.Text
import Prelude as P

showI :: Int -> Text
showI i = pack (show i)

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [ Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200" ]

-- 30*30
basicRect :: RealFloat a => Element -> (a, a) -> Element
basicRect el (x, y) = g_ [
        Transform_ <<- translate x y
    ]
    $ rect_ [
        Width_ <<- "30",
        Height_ <<- "30",
        Fill_ <<- "none",
        Stroke_ <<- "black" 
    ] <> el

basicRectGrid :: Element
basicRectGrid = mconcat $
    P.map
        (basicRect $ circle_ [ R_ <<- "10", Fill_ <<- "none", Stroke_ <<- "black" ])
        [ (x,y) | x<-[ 10, 50, 90, 130 ], y<-[ 10, 50, 90, 130 ] ]

main :: IO ()
main = do
    print $ svg $
        basicRectGrid

