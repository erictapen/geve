{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Distorsion as D
import Fläche
import Graphics.Svg
import Linien
import Pfeile
import Raster
import System.Directory
import Text.Printf

type Page = Element

pages :: [Page]
pages =
  [ D.distorsion10,
    D.distorsion12,
    D.distorsion13,
    D.distorsion14,
    D.distorsion15,
    D.distorsion16,
    D.distorsion17,
    D.distorsion18,
    D.distorsion19,
    D.distorsion20,
    D.distorsion21,
    D.distorsion22,
    D.distorsion23,
    D.distorsion24,
    D.distorsion25,
    D.distorsion26,
    D.distorsion27,
    D.distorsion28,
    D.distorsion29,
    Fläche.fläche01,
    Linien.lines,
    Linien.linecircle1,
    Linien.linecircle2,
    Linien.linecircle3,
    Pfeile.pfeile01,
    Pfeile.pfeile02,
    Pfeile.pfeile03,
    Pfeile.pfeile04,
    Pfeile.pfeile05,
    Pfeile.pfeile06,
    Raster.triangleRasterResult
  ]

lazyWriteSvg :: FilePath -> Page -> IO ()
lazyWriteSvg f g =
  let file = f
   in do
        fileExists <- doesFileExist file
        when (forceRewrite || not fileExists) $ renderToFile file g

forceRewrite :: Bool
forceRewrite = False

type PageNumber = Int

writePages :: PageNumber -> [Page] -> IO ()
writePages _ [] = mempty
writePages number (p : ps) = do
  lazyWriteSvg (printf "cache/page%03d.svg" number) $ svg p
  writePages (number + 1) ps

-- document root
svg :: Element -> Element
svg content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

main :: IO ()
main = do
  writePages 1 pages
