{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Text
import Debug.Trace
import Graphics.Svg
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

type BaseSize = Float

type Length = Float

type LengthFactor = Float

data Point = Point Float Float

data Arrow
  = Triangle Point BaseSize Length
  | Arrow Point LengthFactor
  | NormalArrow Point
  | ShortArrow Point
  | LongArrow Point

instance ToElement Arrow where
  toElement (Triangle (Point x y) baseSize length) =
    path_
      [ D_
          <<- ( mA x y
                  <> lA x (y + baseSize)
                  <> lA (x + length) (y + baseSize / 2)
              ),
        Fill_ <<- "black",
        Stroke_ <<- "none"
      ]
  toElement (Arrow (Point x y) lengthFactor) =
    let addXY px py = lA (x + lengthFactor * px) (y + py)
     in path_
          [ D_
              <<- ( mA (x + 0) (y + 0)
                      <> addXY 0 5.292
                      <> addXY 15.875 5.292
                      <> addXY 15.875 10.583
                      <> addXY 26.458 0
                      <> addXY 15.875 (-10.583)
                      <> addXY 15.845 (-5.292)
                      <> addXY 0 (-5.292)
                  ),
            Fill_ <<- "black",
            Stroke_ <<- "none"
          ]
  toElement (NormalArrow p) = toElement $ Arrow p 1.0
  toElement (ShortArrow p) = toElement $ Arrow p 0.66
  toElement (LongArrow p) = toElement $ Arrow p 1.377

forceRewrite :: Bool
forceRewrite = False

-- simple type to represent wether something should be drawn or not
data Draw = Y | N

main :: IO ()
main =
  let writeSvg f g = renderToFile f $ svg g
      lazyWriteSvg f g = do
        fileExists <- doesFileExist f
        when (forceRewrite || not fileExists) $ writeSvg f g
   in do
        writeSvg "01.svg" $
          let mkTriangle (point, Y) = toElement $ Triangle point 60 15
              mkTriangle (_, N) = mempty
           in g_ [] $ mconcat $ P.map mkTriangle
                $ P.zip [(Point x y) | y <- [0, 30 .. 200], x <- [0, 15 .. (15 * 11)]]
                $ [Y, Y, Y, Y, Y, Y, Y, Y, Y, Y, Y, Y]
                  ++ [Y, Y, N, Y, N, N, Y, Y, N, Y, N, Y]
                  ++ [Y, N, Y, N, Y, Y, N, N, Y, N, Y, Y]
                  ++ [Y, Y, N, Y, N, N, Y, Y, N, Y, N, Y]
                  ++ [Y, Y, Y, Y, Y, Y, Y, Y, Y, Y, Y, Y]
        writeSvg "02.svg"
          $ g_
            [ Transform_ <<- rotateAround (-45) 90 80
            ]
          $ mconcat
          $ P.map
            toElement
            [ Triangle (Point 60 60) 160 (-60),
              Triangle (Point 120 60) 160 (-60),
              Triangle (Point 180 60) 160 (-60)
            ]
        writeSvg "03.svg"
          $ g_
            [ Transform_ <<- rotateAround (-45) 90 80
            ]
          $ mconcat
          $ P.map
            toElement
            [ Triangle (Point 70 70) 140 (-70),
              Triangle (Point 140 70) 140 (-70),
              Triangle (Point 210 70) 140 (-70)
            ]
        writeSvg "05.svg" $
          let mkArrow p = toElement $ NormalArrow p
           in g_ [] $ mconcat $
                P.map
                  mkArrow
                  [(Point x y) | y <- [0, 21.167 .. (7 * 21.167)], x <- [0, 26.458 .. (5 * 26.458)]]
        writeSvg "06.svg" $
          let mkArrow :: Float -> [Float] -> Float -> Element
              mkArrow _ [] _ = mempty
              mkArrow x (factor : fs) y = (toElement $ Arrow (Point x y) factor) <> mkArrow (x + factor * 26.458) fs y
           in g_ [] $ mconcat $
                P.map
                  (mkArrow 0 [1.377, 0.66, 1.377, 0.66, 1, 1])
                  [y | y <- [0, 21.167 .. (7 * 21.167)]]
