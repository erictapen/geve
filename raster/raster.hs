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
basicMask :: (Int, Int) -> Element
basicMask (x, y) = rect_ [
        X_ <<- (showI x),
        Y_ <<- (showI y),
        Width_ <<- "30",
        Height_ <<- "30",
        Fill_ <<- "white",
        Stroke_ <<- "none" 
    ]

-- mask which consists of regular masks, made to create "windows"
basicRectGrid :: Element
basicRectGrid = mask_ [ Id_ <<- "myMask" ] $
    rect_ [
        X_ <<- "0",
        Y_ <<- "0",
        Width_ <<- "200",
        Height_ <<- "200",
        Fill_ <<- "white"
    ]
    <> (mconcat $
        P.map
            basicMask
            [ (x,y) | x<-[ 10, 50, 90, 130 ], y<-[ 10, 50, 90, 130 ] ])

maskFront :: Element
maskFront = rect_ [
        X_ <<- "0",
        Y_ <<- "0",
        Width_ <<- "200",
        Height_ <<- "200",
        Fill_ <<- "none"
        -- Mask_ <<- "url(#myMask)"
    ]

main :: IO ()
main = do
    print $ svg $ basicRectGrid <> circle_ [ R_ <<- "200" ] <> maskFront

