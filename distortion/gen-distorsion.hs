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
move :: Perlin -> (Double, Double) -> (Double, Double)
move pNoise (x, y) = ((val x), (val y))
  where
    factor = 200.0
    val old = (+) old $ (*) factor $ noiseValue pNoise (x, y, 0)

dot :: (Show a, RealFloat a) => Perlin -> (Perlin -> (a, a) -> (a, a)) -> (a, a) -> Element
dot pNoise move xy =
  let (movedX, movedY) = move pNoise xy
   in circle_
        [ Cx_ <<- showR movedX,
          Cy_ <<- showR movedY,
          R_ <<- showR 0.5
        ]

lineSegment :: Perlin -> (Double, Double) -> Double
lineSegment pNoise (x, y) = (distort x)
  where
    factor = 300.0
    noise = noiseValue pNoise (x, y, 0)
    distort old = (+) old $ (*) factor noise

line :: Perlin -> Double -> Element
line pNoise x =
  path_
    [ D_
        <<- ( mA (lineSegment pNoise (x, 0)) 0
                <> (mconcat $ P.map (\y -> lA (lineSegment pNoise (x, y)) y) $ P.tail [0, 2 .. 200])
            ),
      Fill_ <<- "none",
      Stroke_ <<- "black",
      Stroke_width_ <<- "1px"
    ]

type Octaves = Int

data Distorsion
  = LineDistorsion Perlin
  | DotDistorsion Perlin

instance ToElement Distorsion where
  toElement (LineDistorsion pNoise) = g_ [] $ mconcat $ P.map (line pNoise) xSpace
  toElement (DotDistorsion pNoise) = g_ [] $ mconcat $ P.map (dot pNoise move) xySpace

main :: IO ()
main =
  let writeSvg f g = renderToFile f $ svg $ toElement g
      mkPerlin seed octaves = perlin seed octaves 0.001 0.5
   in do
        writeSvg "10.svg" $ DotDistorsion $ mkPerlin 5 10
        writeSvg "12.svg" $ LineDistorsion $ mkPerlin 5 10
        writeSvg "13.svg" $ LineDistorsion $ mkPerlin 500 5
        writeSvg "14.svg" $ LineDistorsion $ mkPerlin 5 5
