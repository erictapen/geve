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

data Arrow = Triangle Point BaseSize Length

data Point = Point Float Float

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

forceRewrite :: Bool
forceRewrite = False

main :: IO ()
main =
  let writeSvg f g = renderToFile f $ svg g
      lazyWriteSvg f g = do
        fileExists <- doesFileExist f
        when (forceRewrite || not fileExists) $ writeSvg f g
      mkTriangle (point, render) =
        if render == 0
          then mempty
          else toElement $ Triangle point 60 15
   in do
        writeSvg "01.svg" $ g_ [] $ mconcat $ P.map mkTriangle
          $ P.zip [(Point x y) | y <- [0, 30 .. 200], x <- [0, 15 .. (15 * 11)]]
          $ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
            ++ [1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1]
            ++ [1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1]
            ++ [1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1]
            ++ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
