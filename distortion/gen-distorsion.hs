{-# LANGUAGE OverloadedStrings #-}

module Raster where

import Graphics.Svg
import Data.Text
import Prelude as P
import Debug.Trace
import Numeric.Noise
import Numeric.Noise.Perlin

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
  <> with (svg11_ content) [ Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200" ]

xySpace :: (Enum a, RealFloat a) => [ (a, a) ]
xySpace = [ (x, y) | x<-range, y<-range ]
    where
        range = [1, 5 .. 200]

-- function that introduces noise
move :: (Double, Double) -> (Double, Double)
move (x, y) = ( (val x), (y) )
    where
        factor = 100.0
        seed :: Int
        seed = round $ x
        -- scale = 0.999
        scale = 0.5
        octaves = 10
        val old = (+) old $ (*) factor $ noiseValue (perlin seed octaves scale 0.5) (1,2,3)

dot :: (Show a, RealFloat a) => ( (a, a) -> (a, a) ) -> (a, a) -> Element
dot move xy = let
        (movedX, movedY) = move xy
    in circle_ [
        Cx_ <<- showR movedX
        , Cy_ <<- showR movedY
        , R_ <<- showR 1
    ]

dots :: Element
dots = g_ [
    ] $ mconcat $ P.map (dot move) xySpace

-- lineSegment :: (Show a, RealFloat a) => ( (a, a) -> (a, a) ) -> (a, a) -> Attribute
-- lineSegment move (x, y) =

-- lines :: Element
-- lines = g_ [
--     ] $ mconcat $ P.map (line move) xSpace

main :: IO ()
main = do
    print $ svg $ dots
    -- print $ svg $ lines

