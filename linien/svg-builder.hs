{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Graphics.Svg

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
  = SimpleLine Thickness Point Point
  | TriangleLine Thickness Thickness Point Point

-- implement toElement for Line, so we can always get an SVG element from a line.
instance ToElement Line where
  toElement (SimpleLine t p1 p2) =
    toElement (TriangleLine t t p1 p2)
  toElement (TriangleLine t1 t2 (Point x1 y1) (Point x2 y2)) =
    let angle = atan2 (y2 - y1) (x2 - x1) -- yeah, atan2 takes y first, then x…
        ea = angle - (pi * 0.5) -- the angle of the stroke ends is 90 degrees turned
        ht1 = t1 / 2 -- half thickness1
        ht2 = t2 / 2 -- half thickness2
        dx1 = cos ea * ht1
        dy1 = sin ea * ht1
        dx2 = cos ea * ht2
        dy2 = sin ea * ht2
     in path_
          [ D_
              <<- ( mA (x1 + dx1) (y1 + dy1)
                      <> lA (x1 - dx1) (y1 - dy1)
                      <> lA (x2 - dx2) (y2 - dy2)
                      <> lA (x2 + dx2) (y2 + dy2)
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
      p1 = Point 10 20
      p2 = Point 100 150
      l1 = SimpleLine 10 p1 p2
      p3 = Point 100 20
      p4 = Point 190 150
      l2 = TriangleLine 40 10 p3 p4
   in do
        writeSvg "./lines.svg" $ linesGraphic $ toElement l1 <> toElement l2
