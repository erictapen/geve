{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Distorsion
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
  [ Distorsion.distorsion10,
    Distorsion.distorsion12,
    Distorsion.distorsion13,
    Distorsion.distorsion14,
    Distorsion.distorsion15,
    Distorsion.distorsion16,
    Distorsion.distorsion17,
    Distorsion.distorsion18,
    Distorsion.distorsion19,
    Distorsion.distorsion20,
    Distorsion.distorsion21,
    Distorsion.distorsion22,
    Distorsion.distorsion23,
    Distorsion.distorsion24,
    Distorsion.distorsion25,
    Distorsion.distorsion26,
    Distorsion.distorsion27,
    Distorsion.distorsion28,
    Distorsion.distorsion29
  ]

lazyWriteSvg :: FilePath -> Page -> IO ()
lazyWriteSvg f g =
  let file = f
   in do
        fileExists <- doesFileExist file
        when (Main.forceRewrite || not fileExists) $ renderToFile file g

forceRewrite :: Bool
forceRewrite = False

type PageNumber = Int

writePages :: PageNumber -> [Page] -> IO ()
writePages _ [] = mempty
writePages number (p:ps) = do
    lazyWriteSvg (printf "cache/page%03d.svg" number) $ Main.svg p
    writePages (number+1) ps

-- document root
svg :: Element -> Element
svg content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "200", Height_ <<- "200"]

main :: IO ()
main = do
   writePages 1 pages
-- Distorsion.generateSvg
-- Fläche.generateSvg
-- Linien.generateSvg
-- Pfeile.generateSvg
-- Raster.generateSvg
