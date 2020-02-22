{-# LANGUAGE NamedFieldPuns #-}
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

-- A single dot, that is moved by a function, e.g. noise
dot :: (Show a, RealFloat a) => Perlin -> (Perlin -> (a, a) -> (a, a)) -> (a, a) -> Element
dot pNoise move xy =
  let (movedX, movedY) = move pNoise xy
   in circle_
        [ Cx_ <<- showR movedX,
          Cy_ <<- showR movedY,
          R_ <<- showR 0.5
        ]

-- Part of a line, moved by some noise function
lineSegment :: Perlin -> (Double, Double) -> Double
lineSegment pNoise (x, y) = (distort x)
  where
    factor = 300.0
    noise = noiseValue pNoise (x, y, 0)
    distort old = (+) old $ (*) factor noise

-- A whole Svg line, with a movement function, x position and thickness
line :: Perlin -> LineThickness -> Double -> Element
line pNoise thickness x =
  path_
    [ D_
        <<- ( mA (lineSegment pNoise (x, 0)) 0
                <> (mconcat $ P.map (\y -> lA (lineSegment pNoise (x, y)) y) $ P.tail [0, 2 .. 200])
            ),
      Fill_ <<- "none",
      Stroke_ <<- "black",
      Stroke_width_ <<- showR thickness
    ]

-- Octaves value for Perlin noise
type Octaves = Int

-- Thickness of a line, in whatever units
type LineThickness = Float

-- Variants of the distorsion graphics
data Distorsion
  = LineDistorsion
      { pNoise :: Perlin,
        thickness :: LineThickness
      }
  | DotDistorsion
      { pNoise :: Perlin
      }

instance ToElement Distorsion where
  toElement (LineDistorsion {pNoise, thickness}) = g_ [] $ mconcat $ P.map (line pNoise thickness) xSpace
  toElement (DotDistorsion {pNoise}) = g_ [] $ mconcat $ P.map (dot pNoise move) xySpace

main :: IO ()
main =
  let writeSvg f g = renderToFile f $ svg $ toElement g
      mkPerlin seed octaves = perlin seed octaves 0.001 0.5
   in do
        writeSvg "10.svg" $
          DotDistorsion
            { pNoise = mkPerlin 5 10
            }
        -- we have no 11.svg, as it was an accident
        writeSvg "12.svg" $
          LineDistorsion
            { pNoise = mkPerlin 5 10,
              thickness = 1
            }
        writeSvg "13.svg" $
          LineDistorsion
            { pNoise = mkPerlin 500 5,
              thickness = 1
            }
        writeSvg "14.svg" $
          LineDistorsion
            { pNoise = mkPerlin 5 5,
              thickness = 0.5
            }
        writeSvg "15.svg" $
          LineDistorsion
            { pNoise = mkPerlin 100 5,
              thickness = 0.2
            }
        writeSvg "16.svg" $
          LineDistorsion
            { pNoise = mkPerlin 100 5,
              thickness = 0.2
            }
