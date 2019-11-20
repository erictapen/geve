{-# LANGUAGE OverloadedStrings #-}

import Graphics.Svg
import Data.Text

showI :: Int -> Text
showI i = pack (show i)

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

-- 30*30
basicRect :: (Int, Int) -> Element
basicRect (x, y) = rect_ [
    X_ <<- showI x,
    Y_ <<- showI y,
    Width_ <<- "30",
    Height_ <<- "30",
    Fill_ <<- "none",
    Stroke_ <<- "black" ]

basicRectGrid :: Element
basicRectGrid = mconcat $
    Prelude.map basicRect [ (x,y) | x<-[ 10, 50, 90, 130 ], y<-[ 10, 50, 90, 130 ] ]

main :: IO ()
main = do
    print $ svg $
        basicRectGrid

