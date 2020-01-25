{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Debug.Trace
import Graphics.Svg
import Numeric.Noise
import Numeric.Noise.Perlin
import Prelude as P

-- helper function for Text type
showI :: Int -> Text
showI i = pack $ show i

-- helper fucntion for RealFloat type
showR :: (Show a, RealFloat a) => a -> Text
showR r = pack $ show r

-- document root
svg :: Element -> Element
svg content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

xySpace :: (Enum a, RealFloat a) => [(a, a)]
xySpace = [(x, y) | x <- range, y <- range]
  where
    range = [1, 4 .. 200]

xSpace :: (Enum a, RealFloat a) => [a]
xSpace = [1, 4 .. 200]

-- function that introduces noise
move :: (Double, Double) -> (Double, Double)
move (x, y) = ((val x), (val y))
  where
    factor = 200.0
    seed :: Int
    seed = round $ 5
    scale = 0.001
    octaves = 10
    val old = (+) old $ (*) factor $ noiseValue (perlin seed octaves scale 0.5) (x, y, 0)

dot :: (Show a, RealFloat a) => ((a, a) -> (a, a)) -> (a, a) -> Element
dot move xy =
  let (movedX, movedY) = move xy
   in circle_
        [ Cx_ <<- showR movedX,
          Cy_ <<- showR movedY,
          R_ <<- showR 0.5
        ]

dots :: Element
dots = g_ [] $ mconcat $ P.map (dot move) xySpace

lineSegment :: (Double, Double) -> Double
lineSegment (x, y) = (distort x)
  where
    factor = 300.0
    seed :: Int
    seed = 500
    -- scale = 0.05
    scale = 0.001
    octaves = 5
    noise = noiseValue (perlin seed octaves scale 0.5) (x, y, 0)
    distort old = (+) old $ (*) factor $ trace (show noise) $ noise

line :: Double -> Element
line x =
  path_
    [ D_
        <<- ( mA (lineSegment (x, 0)) 0
                <> (mconcat $ P.map (\y -> lA (lineSegment (x, y)) y) $ P.tail [0, 2 .. 200])
            ),
      Fill_ <<- "none",
      Stroke_ <<- "black",
      Stroke_width_ <<- "1px"
    ]

lineSet :: Element
lineSet = g_ [] $ mconcat $ P.map line xSpace

main :: IO ()
main =
  let writeSvg f g = writeFile f $ show $ svg g
   in do
        writeSvg "14.svg" $ lineSet
