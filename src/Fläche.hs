{-# LANGUAGE OverloadedStrings #-}

module Fläche where

import Data.Text
import Graphics.Svg
import Text.Printf

boxSize :: RealFloat a => a
boxSize = 30

showI :: Int -> Text
showI i = pack (show i)

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
                  <> lA 0 30
                  <> lA 30 30
                  <> lA 30 0
                  <> z
              ),
        Fill_ <<- "white",
        Stroke_ <<- "none"
      ]

size = 15
-- concat the sphere points for an nGone
nGone_pts :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
nGone_pts 0 _ final = final
nGone_pts lft max lst = 
  if max < 3
    then error "nGone has to have at least three corners."
  else
    nGone_pts (lft - 1) max (lst <> [(size*sin((max-lft) * 2 * pi/max),
                                      -size*cos((max-lft) * 2 * pi/max))])

multi_lA :: [(Float, Float)] -> Text -> Text
multi_lA [] final = final
multi_lA (to_lA : rest) str = do
  multi_lA rest (str <> lA (fst to_lA) (snd to_lA))

nGone_pts_toString :: [(Float, Float)] -> Text
nGone_pts_toString ((first, second) : rest) =
  mA first second <>  (multi_lA rest empty) <> z

nGone_toString :: Int -> Text
nGone_toString n =
  nGone_pts_toString $ nGone_pts ((fromIntegral n) :: Float) ((fromIntegral n) :: Float) []

-- arbitrary polygone with n corners
nGone :: RealFloat a => (Int, (a, a)) -> Element
nGone (n, (x, y)) =
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
  mconcat $
    Prelude.map nGone $ Prelude.zip [3..39] [(x, y) | y <- [30, 65 .. 170], x <- [30, 65 .. 170] ]

fläche01 :: Element
fläche01 = basicRectGrid <> incGongrid 7
