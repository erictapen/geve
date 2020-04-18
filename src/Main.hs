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
  [ Fläche.fläche01
    ]

lazyWriteSvg :: FilePath -> Page -> IO ()
lazyWriteSvg f g =
  let file = f
   in do
        fileExists <- doesFileExist file
        when (forceRewrite || not fileExists) $ renderToFile file g

forceRewrite :: Bool
forceRewrite = True

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
