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

size = 15

-- concat the sphere points for an nGone
nGone_pts :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
nGone_pts 0 _ final = final
nGone_pts lft max lst =
  if max < 3
    then error "nGone has to have at least three corners."
    else
      nGone_pts
        (lft - 1)
        max
        ( lst
            <> [ ( size * sin ((max - lft) * 2 * pi / max),
                   - size * cos ((max - lft) * 2 * pi / max)
                 )
               ]
        )

multi_lA :: [(Float, Float)] -> Text -> Text
multi_lA [] final = final
multi_lA (to_lA : rest) str = do
  multi_lA rest (str <> lA (fst to_lA) (snd to_lA))

nGone_pts_toString :: [(Float, Float)] -> Text
nGone_pts_toString ((first, second) : rest) =
  mA first second <> (multi_lA rest empty) <> z

nGone_toString :: Int -> Text
nGone_toString n =
  nGone_pts_toString $ nGone_pts ((fromIntegral n) :: Float) ((fromIntegral n) :: Float) []

-- arbitrary polygone with n corners
nGone :: (Int, Point) -> Element
nGone (n, (Point x y)) =
  g_
    [ Transform_ <<- translate x y
    ]
    $ path_
      [ D_
          <<- ( nGone_toString n
              ),
        Fill_ <<- "white",
        Stroke_ <<- "none"
      ]

incGongrid :: Int -> Element
incGongrid n =
  mconcat
    $ P.map nGone
    $ P.zip [3 .. 39] [Point x y | y <- [30, 65 .. 170], x <- [30, 65 .. 170]]

fläche01 :: Element
fläche01 = basicRectGrid <> incGongrid 7
