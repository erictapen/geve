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

data Point = Point Float Float

data Arrow
  = Triangle Point BaseSize Length
  | NormalArrow Point

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
  toElement (NormalArrow (Point x y)) =
    let addXY px py = lA (x + px) (y + py)
     in path_
          [ D_
              <<- ( mA (x + 0) (y + 5.292)
                      <> addXY 0 15.875
                      <> addXY 15.875 15.875
                      <> addXY 15.875 21.167
                      <> addXY 26.458 10.583
                      <> addXY 15.875 0
                      <> addXY 15.845 5.292
                  ),
            Fill_ <<- "black",
            Stroke_ <<- "none"
          ]

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
