{-# LANGUAGE OverloadedStrings #-}

module Fläche where

import Data.Text
import Graphics.Svg
import Prelude as P

boxSize :: RealFloat a => a
boxSize = 30

showI :: Int -> Text
showI i = pack (show i)

data Quad = Quad Point Point Point Point

data Point = Point Float Float

instance ToElement Quad where
  toElement
    ( Quad
        (Point x1 y1)
        (Point x2 y2)
        (Point x3 y3)
        (Point x4 y4)
      ) =
      path_
        [ D_
            <<- ( mA x1 y1
                    <> lA x2 y2
                    <> lA x3 y3
                    <> lA x4 y4
                    <> z
                ),
          Fill_ <<- "white",
          Stroke_ <<- "none"
        ]

-- 30*30
-- The basic rectangle
basicRect :: (Point, Quad) -> Element
basicRect (Point x y, quad) =
  g_
    [ Transform_ <<- translate x y
    ]
    $ rect_
      [ Width_ <<- "30",
        Height_ <<- "30",
        Fill_ <<- "black",
        Stroke_ <<- "black"
      ]
      <> toElement quad

-- the grid in which basic rectangles are aligned
basicRectGrid :: Element
basicRectGrid =
  mconcat
    $ P.map basicRect
    $ P.zip [Point x y | y <- [15, 50 .. 155], x <- [15, 50 .. 155]] quads

-- a quad with not so random dimensions
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

quads :: [Quad]
quads =
  [ Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10),
    Quad (Point 0 0) (Point 10 0) (Point 10 10) (Point 0 10)
  ]

fläche01 :: Element
fläche01 = basicRectGrid <> randomQuad (15, 15)
