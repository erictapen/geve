{-# LANGUAGE OverloadedStrings #-}

module Fläche where

import Data.Text
import Graphics.Svg

boxSize :: RealFloat a => a
boxSize = 30

showI :: Int -> Text
showI i = pack (show i)

-- basic function to generate a svg document
svg :: Element -> Element
svg content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

-- 30*30
-- The basic rectangle
basicRect :: RealFloat a => (a, a) -> Element
basicRect (x, y) =
  g_
    [ Transform_ <<- translate x y
    ]
    $ rect_
      [ Width_ <<- "30",
        Height_ <<- "30",
        Fill_ <<- "black",
        Stroke_ <<- "black"
      ]

-- the grid in which basic rectangles are aligned
basicRectGrid :: Element
basicRectGrid =
  mconcat $
    Prelude.map basicRect [(x, y) | x <- [15, 50 .. 155], y <- [15, 50 .. 155]]

-- a quad with random dimensions
randomQuad :: RealFloat a => (a, a) -> Element
randomQuad (x, y) =
  g_
    [ Transform_ <<- translate x y
    ]
    $ path_
      [ D_
          <<- ( mA 0 0
                  <> lA 0 10
                  <> lA 10 10
                  <> lA 10 0
                  <> z
              ),
        Fill_ <<- "white",
        Stroke_ <<- "none"
      ]

generateSvg :: IO ()
generateSvg = do
  let writeSvg f g = renderToFile f g
   in writeSvg "./cache/fläche-01.svg" $ svg $
        basicRectGrid <> randomQuad (15, 15)
