{-# LANGUAGE OverloadedStrings #-}

import Data.List as DL
import Data.Text
import Debug.Trace
import Graphics.Svg
import Prelude as P

-- import BasicPrelude (tshow)

showI :: Int -> Text
showI i = pack (show i)

showt :: Show a => a -> Text
showt t = pack $ show t

-- basic function to generate a svg document
svg :: Element -> Element
svg content =
  doctype
    <> with
      (svg11_ content)
      [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

data Point = Point Float Float

type Thickness = Float

data Line
  = -- A line with a single thickness
    SimpleLine Thickness Point Point
  | -- A line with two thicknesses, so it can be shaped like the cut of a triangle
    TriangleLine Thickness Thickness Point Point
  | -- A line with varying thickness
    ComplexLine [Thickness] Point Point

-- Implement toElement for Line, so we can always get an SVG element from a Line.
-- All of this can be deduced from ComplexLine
instance ToElement Line where
  toElement (SimpleLine t p1 p2) =
    toElement (ComplexLine [t] p1 p2)
  toElement (TriangleLine t1 t2 p1 p2) =
    toElement (ComplexLine [t1, t2] p1 p2)
  toElement (ComplexLine [] p1 p2) =
    toElement (ComplexLine [1.0, 1.0] p1 p2)
  toElement (ComplexLine (t : []) p1 p2) =
    toElement (ComplexLine [t, t] p1 p2)
  toElement (ComplexLine thicknesses (Point x1 y1) (Point x2 y2)) =
    let angle = atan2 (y2 - y1) (x2 - x1) -- yeah, atan2 takes y first, then x…
        orthAngle = angle - (0.5 * pi) -- the angle of the stroke ends is 90 degrees turned
        fractionalSteps = [0, (1 / (fromRational $ fromIntegral $ P.length thicknesses - 1)) .. 1]
        xs = P.map (\f -> x1 + f * (x2 - x1)) fractionalSteps -- x positions of points along the line
        ys = P.map (\f -> y1 + f * (y2 - y1)) fractionalSteps
        dxs = P.map (\t -> cos orthAngle * (t / 2)) thicknesses -- deltas to move points in x direction so we get thickness
        dys = P.map (\t -> sin orthAngle * (t / 2)) thicknesses
        pathPoint factor (x, y, dx, dy) = lA (x + (factor * dx)) (y + (factor * dy))
        pathPoints = DL.zip4 xs ys dxs dys
     in path_
          [ D_
              <<- ( mA x1 y1
                      <> (mconcat $ P.map (pathPoint (-1)) pathPoints)
                      <> lA x2 y2
                      <> (mconcat $ P.reverse $ P.map (pathPoint 1) pathPoints)
                      <> z
                  ),
            Fill_ <<- "black",
            Stroke_ <<- "none"
          ]

line :: Element -> Element
line e = e

linesGraphic :: Element -> Element
linesGraphic e = line e

main :: IO ()
main =
  let writeSvg f g = writeFile f $ show $ svg g
      p1 = Point 0 0
      p2 = Point 0 100
      l1 = toElement $ SimpleLine 10 p1 p2
      p3 = Point 100 0
      p4 = Point 100 100
      l2 = toElement $ TriangleLine 10 20 p3 p4
      p5 = Point 200 0
      p6 = Point 200 100
      l3 = toElement $ ComplexLine [5, 10, 5, 10, 5, 30] p5 p6
   in do
        writeSvg "./lines.svg" $ linesGraphic $ l1 <> l2 <> l3
