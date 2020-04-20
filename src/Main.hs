{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Text
import Distorsion as D
import Fläche
import Graphics.Svg
import Linien
import Pfeile
import Raster
import System.Directory
import Text.Printf

showF :: Float -> Text
showF f = pack $ show f

type Page = Element

move :: Float -> Float -> Element -> Element
move x y e = g_ [ Transform_ <<- translate x y ] e

pages :: [Page]
pages =
  [ frontPage,
    mempty,
    D.distorsion10,
    Main.move (-200) 0 D.distorsion12,
    D.distorsion13,
    Main.move (-200) 0 D.distorsion14,
    D.distorsion15,
    D.distorsion16,
    D.distorsion17,
    D.distorsion18,
    D.distorsion19,
    D.distorsion20,
    D.distorsion21,
    D.distorsion22,
    Main.move (100) 0 D.distorsion23,
    Main.move (22) 0 D.distorsion24,
    D.distorsion25,
    D.distorsion26,
    D.distorsion27,
    Main.move (-200) 0 D.distorsion28,
    Main.move (150) 0 D.distorsion29,
    Fläche.fläche01,
    Linien.linecircle1,
    Linien.linecircle2,
    Linien.linecircle3,
    Pfeile.pfeile01,
    Pfeile.pfeile02,
    Pfeile.pfeile03,
    Main.move 23 28 Pfeile.pfeile04,
    Main.move 23 28 Pfeile.pfeile05,
    Main.move (-11) (-11) Pfeile.pfeile06
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
    <> with (svg11_ content)
       [Version_ <<- "1.1", Width_ <<- "200mm", Height_ <<- "200mm", ViewBox_ <<- "0 0 200 200"]

frontPage :: Element
frontPage =
  let text :: [Attribute] -> Text -> Text -> Text -> Element
      text attrs x y text =
        text_
          ( attrs
              ++ [ X_ <<- x,
                   Y_ <<- y,
                   Font_family_ <<- "Fira Sans",
                   Text_anchor_ <<- "end",
                   Style_ <<- "line-height: 108%; text-align: end;"
                 ]
          )
          $ toElement text
      heading =
        text
          [ Font_size_ <<- "26.6"
          ]
      footer =
        text
          [ Font_size_ <<- "5.3"
          ]
   in heading "188" "30" "Grafische"
        <> heading "188" "58" "Elemente"
        <> heading "188" "86" "& visuelle"
        <> heading "188" "114" "Effekte"
        <> footer "188" "182" "bei Prof. Klaus Keller, WS 2019/2020, FH Potsdam"
        <> footer "188" "188" "Abgabe Justin Humm, 3. Semester Kommunikationsdesign"

main :: IO ()
main = do
  writePages 1 pages
