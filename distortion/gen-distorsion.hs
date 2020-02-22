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
move :: Seed -> (Double, Double) -> (Double, Double)
move seed (x, y) = ((val x), (val y))
  where
    factor = 200.0
    scale = 0.001
    octaves = 10
    val old = (+) old $ (*) factor $ noiseValue (perlin seed octaves scale 0.5) (x, y, 0)

dot :: (Show a, RealFloat a) => Seed -> (Seed -> (a, a) -> (a, a)) -> (a, a) -> Element
dot seed move xy =
  let (movedX, movedY) = move seed xy
   in circle_
        [ Cx_ <<- showR movedX,
          Cy_ <<- showR movedY,
          R_ <<- showR 0.5
        ]

dots :: DistorsionData -> Element
dots (DistorsionData seed) = g_ [] $ mconcat $ P.map (dot seed move) xySpace

lineSegment :: Seed -> (Double, Double) -> Double
lineSegment seed (x, y) = (distort x)
  where
    factor = 300.0
    -- scale = 0.05
    scale = 0.001
    octaves = 5
    noise = noiseValue (perlin seed octaves scale 0.5) (x, y, 0)
    distort old = (+) old $ (*) factor $ trace (show noise) $ noise

line :: Seed -> Double -> Element
line seed x =
  path_
    [ D_
        <<- ( mA (lineSegment seed (x, 0)) 0
                <> (mconcat $ P.map (\y -> lA (lineSegment seed (x, y)) y) $ P.tail [0, 2 .. 200])
            ),
      Fill_ <<- "none",
      Stroke_ <<- "black",
      Stroke_width_ <<- "1px"
    ]

lineSet :: DistorsionData -> Element
lineSet (DistorsionData seed) = g_ [] $ mconcat $ P.map (line seed) xSpace

data DistorsionData = DistorsionData Seed

main :: IO ()
main =
  let writeSvg f g = renderToFile f $ svg g
   in do
        writeSvg "14.svg" $ lineSet $ DistorsionData 5
        writeSvg "13.svg" $ lineSet $ DistorsionData 500
