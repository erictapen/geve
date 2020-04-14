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

forceRewrite :: Bool
forceRewrite = False

generateSvg :: IO ()
generateSvg =
  let writeSvg f g = renderToFile f $ svg $ toElement g
      lazyWriteSvg f g =
        let file = "./cache/distorsion-" ++ f
         in do
              fileExists <- doesFileExist file
              when (forceRewrite || not fileExists) $ writeSvg file g
      mkPerlin seed octaves = mkPerlinWithScale seed octaves 0.001
      mkPerlinWithScale seed octaves scale = perlin seed octaves scale 0.5
      defaultXSpace = [100, 104 .. 300]
      defaultXYSpace = xySpace 3
      xySpace stepSize = [(x, y) | x <- range, y <- range]
        where
          range = [0, stepSize .. 200]
      xSpace stepSize = [100, 100 + stepSize .. 300]
   in do
        lazyWriteSvg "10.svg" $
          DotDistorsion
            { pNoise = mkPerlin 5 10,
              radius = 0.5,
              xySpace = defaultXYSpace
            }
        -- we have no 11.svg, as it was an accident
        lazyWriteSvg "12.svg" $
          LineDistorsion
            { pNoise = mkPerlin 5 10,
              thickness = 1,
              xSpace = defaultXSpace
            }
        lazyWriteSvg "13.svg" $
          LineDistorsion
            { pNoise = mkPerlin 500 5,
              thickness = 1,
              xSpace = defaultXSpace
            }
        lazyWriteSvg "14.svg" $
          LineDistorsion
            { pNoise = mkPerlin 5 5,
              thickness = 0.5,
              xSpace = defaultXSpace
            }
        lazyWriteSvg "15.svg" $
          LineDistorsion
            { pNoise = mkPerlin 100 5,
              thickness = 0.2,
              xSpace = defaultXSpace
            }
        lazyWriteSvg "16.svg" $
          LineDistorsion
            { pNoise = mkPerlin 100 5,
              thickness = 0.2,
              xSpace = [100, 101 .. 300]
            }
        lazyWriteSvg "17.svg" $
          LineDistorsion
            { pNoise = mkPerlin 150 10,
              thickness = 0.2,
              xSpace = [100, 100.5 .. 300]
            }
        lazyWriteSvg "18.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 300 3 0.002,
              thickness = 5,
              xSpace = [0, 20 .. 200]
            }
        lazyWriteSvg "19.svg" $
          DotDistorsion
            { pNoise = mkPerlinWithScale 300 3 0.002,
              radius = 1,
              xySpace = xySpace 7
            }
        lazyWriteSvg "20.svg" $
          DotDistorsion
            { pNoise = mkPerlinWithScale 350 3 0.002,
              radius = 2,
              xySpace = xySpace 7
            }
        lazyWriteSvg "21.svg" $
          DotDistorsion
            { pNoise = mkPerlinWithScale 400 15 0.0008,
              radius = 0.5,
              xySpace = xySpace 1
            }
        lazyWriteSvg "22.svg" $
          DotDistorsion
            { pNoise = mkPerlinWithScale 410 15 0.0008,
              radius = 0.45,
              xySpace = xySpace 1
            }
        lazyWriteSvg "23.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 410 15 0.0008,
              thickness = 0.30,
              xSpace = defaultXSpace
            }
        lazyWriteSvg "24.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 420 10 0.008,
              thickness = 0.20,
              xSpace = xSpace 0.5
            }
        lazyWriteSvg "25.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 460 3 0.0019,
              thickness = 0.5,
              xSpace = xSpace 5
            }
        lazyWriteSvg "26.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 483 3 0.002,
              thickness = 0.8,
              xSpace = xSpace 10
            }
        lazyWriteSvg "27.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 485 3 0.002,
              thickness = 0.40,
              xSpace = xSpace 1
            }
        writeSvg "28.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 486 3 0.003,
              thickness = 0.30,
              xSpace = xSpace 2.5
            }
        writeSvg "29.svg" $
          LineDistorsion
            { pNoise = mkPerlinWithScale 490 13 0.001,
              thickness = 0.10,
              xSpace = xSpace 0.5
            }
