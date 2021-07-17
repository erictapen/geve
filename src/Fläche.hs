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

multiLA :: [Point] -> Text -> Text
multiLA [] final = final
multiLA ((Point x y) : rest) str = do
  multiLA rest (str <> lA x y)

pts2path :: [Point] -> Text
pts2path ((Point x y) : rest) =
  mA x y <> multiLA rest empty <> z

data Ngon = Quad Point Point Point Point | Ngon [Point]

data Point = Point Float Float

instance ToElement Ngon where
  toElement (Ngon pts) =
    path_
      [ D_ <<-  pts2path pts,
        Fill_ <<- "white",
        Stroke_ <<- "none"
      ]
  toElement (Quad p1 p2 p3 p4) = toElement (Ngon [p1, p2, p3, p4])

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
basicRectGrid ngons = mconcat $
  P.zipWith
    (curry basicRect)
    ([Point x y | y <- [15, 50 .. 155], x <- [15, 50 .. 155]]) ngons

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

ptsOnSphere :: Float -> Point -> Int -> Int -> Point
ptsOnSphere size (Point x y) max cur =
  let cur_fl = (fromIntegral cur :: Float)
      max_fl = (fromIntegral max :: Float)
   in Point
        (size * sin ((max_fl - cur_fl) * 2 * pi / max_fl) + x)
        (- size * cos ((max_fl - cur_fl) * 2 * pi / max_fl) + y)

nGonFromInt :: Float -> Point -> Int -> Ngon
nGonFromInt size point n =
  if n < 3
    then error "nGon has to have at least three corners."
    else Ngon $ P.map (ptsOnSphere size point n) [0 .. n]

nGonsInc :: [Ngon]
nGonsInc =
  P.map (nGonFromInt 10 (Point 15 15)) [3 .. 39]

fläche01 :: Element
fläche01 = basicRectGrid nGonsInc

fläche02 :: Element
fläche02 = basicRectGrid quads
