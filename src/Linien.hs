{-# LANGUAGE OverloadedStrings #-}

module Linien where

import Data.List as DL
import Data.Text
import Graphics.Svg
import Prelude as P

-- import BasicPrelude (tshow)

showI :: Int -> Text
showI i = pack (show i)

showt :: Show a => a -> Text
showt t = pack $ show t

data Point = Point Float Float

instance Semigroup Point where
  (<>) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

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
        -- deltas to move points in x direction so we get thickness
        dxs = P.map (\t -> cos orthAngle * (t / 2)) thicknesses
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

type Radius = Float

type Amount = Float

type Center = Point

data Circle
  = -- Just a data type to talk about circles.
    Circle Center Radius

-- LineCircles are circles that consist of lines…
data LineCircle
  = -- A simple line circle with an outer and an inner radius and a constant thickness.
    LineCircle
      Circle
      Circle
      Amount
      Thickness
  | -- A LineCircle which allows two thickness functions for both ends of the lines.
    VariableThicknessLineCircle
      Circle
      Circle
      Amount
      (Float -> Thickness)
      (Float -> Thickness)

instance ToElement LineCircle where
  toElement (LineCircle circle1 circle2 n thickness) =
    toElement $
      VariableThicknessLineCircle
        circle1
        circle2
        n
        (const thickness)
        (const thickness)
  toElement
    ( VariableThicknessLineCircle
        (Circle center1 r1)
        (Circle center2 r2)
        n
        angleToThickness1
        angleToThickness2
      ) =
      let steps = [0, (2 * pi / n) .. 2 * pi]
          angles = steps
          thicknesses1 = P.map angleToThickness1 angles
          thicknesses2 = P.map angleToThickness2 angles
          line (angle, thickness1, thickness2) =
            toElement $
              TriangleLine
                thickness1
                thickness2
                (center1 <> (Point (r1 * cos angle) (r1 * sin angle)))
                (center2 <> (Point (r2 * cos angle) (r2 * sin angle)))
       in mconcat $ P.map line $ zip3 angles thicknesses1 thicknesses2

-- Some points and premanufactured elements. Could be refactored later on.
p1 = Point 0 0

p2 = Point 0 100

l1 = toElement $ SimpleLine 10 p1 p2

p3 = Point 100 0

p4 = Point 100 100

l2 = toElement $ TriangleLine 10 20 p3 p4

p5 = Point 200 0

p6 = Point 200 100

l3 = toElement $ ComplexLine [5, 10, 5, 10, 5, 30] p5 p6

lines :: Element
lines = l1 <> l2 <> l3

linecircle1 :: Element
linecircle1 =
  toElement $
    let centerPoint = Point 100 100
     in LineCircle (Circle centerPoint 100) (Circle centerPoint 50) 64 1

linecircle2 :: Element
linecircle2 =
  let centerPoint = Point 100 100
   in toElement
        ( VariableThicknessLineCircle
            (Circle centerPoint 75)
            (Circle centerPoint 100)
            64
            (\s -> 1 * (1 + sin (s + pi)))
            (\s -> 1 * (1 + sin (s)))
        )
        <> toElement
          ( VariableThicknessLineCircle
              (Circle centerPoint 50)
              (Circle centerPoint 75)
              64
              (\s -> 1 * (1 + cos (s + pi)))
              (\s -> 1 * (1 + cos (s)))
          )

linecircle3 :: Element
linecircle3 =
  let center1 = Point 102 92
      center2 = Point 92 102
      outer = \s -> 1 * (1 + sin (0.5 * pi + s))
      inner = \s -> 1 * (1 + sin (1.0 * pi + s))
      n = 64
   in toElement
        ( VariableThicknessLineCircle
            (Circle center2 100)
            (Circle center1 75)
            n
            outer
            inner
        )
        <> toElement
          ( VariableThicknessLineCircle
              (Circle center1 75)
              (Circle center2 50)
              n
              inner
              outer
          )
