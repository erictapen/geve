{-# LANGUAGE OverloadedStrings #-}

module Fläche where

import Data.Text
import Graphics.Svg
import Text.Printf
import Prelude as P

boxSize :: RealFloat a => a
boxSize = 30

showI :: Int -> Text
showI i = pack (show i)

multi_lA :: [Point] -> Text -> Text
multi_lA [] final = final
multi_lA ((Point x y) : rest) str = do
  multi_lA rest (str <> lA x y)

pts2path :: [Point] -> Text
pts2path ((Point x y) : rest) =
  mA x y <> (multi_lA rest empty) <> z

data Ngon = Quad Point Point Point Point | Ngon [Point]

data Point = Point Float Float

instance ToElement Ngon where
  toElement
    ( Ngon pts
    ) =
    path_
      [ D_
          <<- ( pts2path pts
              ),
        Fill_ <<- "white",
        Stroke_ <<- "none"
      ]

  toElement
    ( Quad p1 p2 p3 p4
      ) = toElement( Ngon [p1, p2, p3, p4])

-- 30*30
-- The basic rectangle
basicRect :: (Point, Ngon) -> Element
basicRect (Point x y, nGon) =
  g_
    [ Transform_ <<- translate x y
    ]
    $ rect_
      [ Width_ <<- "30",
        Height_ <<- "30",
        Fill_ <<- "black",
        Stroke_ <<- "black"
      ]
      <> toElement nGon

-- the grid in which basic rectangles are aligned
basicRectGrid :: [Ngon] -> Element
basicRectGrid nGons =
  mconcat
    $ P.map basicRect
    $ P.zip [Point x y | y <- [15, 50 .. 155], x <- [15, 50 .. 155]] nGons

-- a quad with not so random dimensions
randomQuad :: Point -> Element
randomQuad (Point x y) =
  g_
    [ Transform_ <<- translate x y
    ]
    $ path_
      [ D_
          <<- ( mA 0 0
                  <> lA 0 30
                  <> lA 30 30
                  <> lA 30 0
                  <> z
              ),
        Fill_ <<- "white",
        Stroke_ <<- "none"
      ]

quads :: [Ngon]
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

ptsOnSphere :: Int -> Int -> Float -> Point
ptsOnSphere cur max size =
  let cur_fl = ((fromIntegral cur) :: Float)
    max_fl = ((fromIntegral max) :: Float)
  in
  Point (size * sin ((max_fl - cur_fl) * 2 * pi / max_fl)) (- size * cos ((max_fl - cur_fl) * 2 * pi / max_fl))

nGonFromInt :: Int -> Float -> Ngon
nGonFromInt n size =
  if n < 3
    then error "nGon has to have at least three corners."
    else Ngon P.map ptsOnSphere [0..n] n size

nGonList :: [Ngon]
nGonList = 
    P.map nGonFromInt [3..39]

fläche01 :: Element
fläche01 = basicRectGrid nGonList

fläche02 :: Element
fläche02 = basicRectGrid quads
