{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Distorsion where

import Control.Monad
import Data.Text
import Debug.Trace
import Graphics.Svg
import Numeric.Noise
import Numeric.Noise.Perlin
import System.Directory
import Prelude as P

-- helper fucntion for RealFloat type
showR :: (Show a, RealFloat a) => a -> Text
showR r = pack $ show r

-- function that introduces noise
move :: Perlin -> (Double, Double) -> (Double, Double)
move pNoise (x, y) = ((val x), (val y))
  where
    factor = 200.0
    val old = (+) old $ (*) factor $ noiseValue pNoise (x, y, 0)

-- A single dot, that is moved by a function, e.g. noise
dot :: (Show a, RealFloat a) => Perlin -> DotRadius -> (Perlin -> (a, a) -> (a, a)) -> (a, a) -> Element
dot pNoise radius move xy =
  let (movedX, movedY) = move pNoise xy
   in circle_
        [ Cx_ <<- showR movedX,
          Cy_ <<- showR movedY,
          R_ <<- showR radius
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

type DotRadius = Float

-- Variants of the distorsion graphics
data Distorsion
  = LineDistorsion
      { pNoise :: Perlin,
        thickness :: LineThickness,
        xSpace :: [Double]
      }
  | DotDistorsion
      { pNoise :: Perlin,
        radius :: DotRadius,
        xySpace :: [(Double, Double)]
      }

instance ToElement Distorsion where
  toElement (LineDistorsion {pNoise, thickness, xSpace}) = g_ [] $ mconcat $ P.map (line pNoise thickness) xSpace
  toElement (DotDistorsion {pNoise, radius, xySpace}) = g_ [] $ mconcat $ P.map (dot pNoise radius move) xySpace

mkPerlin seed octaves = mkPerlinWithScale seed octaves 0.001

mkPerlinWithScale seed octaves scale = perlin seed octaves scale 0.5

defaultXSpace = [100, 104 .. 300]

defaultXYSpace = mkXYSpace 3

mkXYSpace stepSize = [(x, y) | x <- range, y <- range]
  where
    range = [0, stepSize .. 200]

mkXSpace stepSize = [100, 100 + stepSize .. 300]

distorsion10 :: Element
distorsion10 =
  toElement $
    DotDistorsion
      { pNoise = mkPerlin 5 10,
        radius = 0.5,
        xySpace = defaultXYSpace
      }

-- we have no 11.svg, as it was an accident

distorsion12 :: Element
distorsion12 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlin 5 10,
        thickness = 1,
        xSpace = defaultXSpace
      }

distorsion13 :: Element
distorsion13 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlin 500 5,
        thickness = 1,
        xSpace = defaultXSpace
      }

distorsion14 :: Element
distorsion14 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlin 5 5,
        thickness = 0.5,
        xSpace = defaultXSpace
      }

distorsion15 :: Element
distorsion15 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlin 100 5,
        thickness = 0.2,
        xSpace = defaultXSpace
      }

distorsion16 :: Element
distorsion16 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlin 100 5,
        thickness = 0.2,
        xSpace = [100, 101 .. 300]
      }

distorsion17 :: Element
distorsion17 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlin 150 10,
        thickness = 0.2,
        xSpace = [100, 100.5 .. 300]
      }

distorsion18 :: Element
distorsion18 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 300 3 0.002,
        thickness = 5,
        xSpace = [0, 20 .. 200]
      }

distorsion19 :: Element
distorsion19 =
  toElement $
    DotDistorsion
      { pNoise = mkPerlinWithScale 300 3 0.002,
        radius = 1,
        xySpace = mkXYSpace 7
      }

distorsion20 :: Element
distorsion20 =
  toElement $
    DotDistorsion
      { pNoise = mkPerlinWithScale 350 3 0.002,
        radius = 2,
        xySpace = mkXYSpace 7
      }

distorsion21 :: Element
distorsion21 =
  toElement $
    DotDistorsion
      { pNoise = mkPerlinWithScale 400 15 0.0008,
        radius = 0.5,
        xySpace = mkXYSpace 1
      }

distorsion22 :: Element
distorsion22 =
  toElement $
    DotDistorsion
      { pNoise = mkPerlinWithScale 410 15 0.0008,
        radius = 0.45,
        xySpace = mkXYSpace 1
      }

distorsion23 :: Element
distorsion23 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 410 15 0.0008,
        thickness = 0.30,
        xSpace = defaultXSpace
      }

distorsion24 :: Element
distorsion24 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 420 10 0.008,
        thickness = 0.10,
        xSpace = mkXSpace 0.5
      }

distorsion25 :: Element
distorsion25 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 460 3 0.0019,
        thickness = 0.5,
        xSpace = mkXSpace 5
      }

distorsion26 :: Element
distorsion26 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 483 3 0.002,
        thickness = 0.8,
        xSpace = mkXSpace 10
      }

distorsion27 :: Element
distorsion27 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 485 3 0.002,
        thickness = 0.40,
        xSpace = mkXSpace 1
      }

distorsion28 :: Element
distorsion28 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 486 3 0.003,
        thickness = 0.30,
        xSpace = mkXSpace 2.5
      }

distorsion29 :: Element
distorsion29 =
  toElement $
    LineDistorsion
      { pNoise = mkPerlinWithScale 490 13 0.001,
        thickness = 0.10,
        xSpace = mkXSpace 0.5
      }
