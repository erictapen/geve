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

data Line = SimpleLine Thickness Point Point

-- implement toElement for Line, so we can always get an SVG element from a line.
instance ToElement Line where
  toElement (SimpleLine thickness (Point x1 y1) (Point x2 y2)) =
    let angle = atan2 (y2 - y1) (x2 - x1) -- yeah, atan2 takes y first, then x…
        ea = angle - (pi * 0.5) -- the angle of the stroke ends is 90 degrees turned
        ht = thickness / 2 -- half thickness
     in path_
          [ D_
              <<- ( mA (x1 + (cos ea * ht)) (y1 + (sin ea * ht))
                      <> lA x1 y1
                      <> lA (x1 - (cos ea * ht)) (y1 - (sin ea * ht))
                      <> lA (x2 - (cos ea * ht)) (y2 - (sin ea * ht))
                      <> lA x2 y2
                      <> lA (x2 + (cos ea * ht)) (y2 + (sin ea * ht))
                      <> z
                  ),
            Fill_ <<- "black",
            Stroke_ <<- "black"
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
      l = SimpleLine 10 p1 p2
   in do
        writeSvg "./lines.svg" $ linesGraphic $ toElement l
