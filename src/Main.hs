module Main where

import Distorsion
import Fläche
import Linien
import Pfeile
import Raster

main :: IO ()
main = do
  Distorsion.generateSvg
  Fläche.generateSvg
  Linien.generateSvg
  Pfeile.generateSvg
  Raster.generateSvg
