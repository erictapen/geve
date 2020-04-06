{-# LANGUAGE OverloadedStrings #-}

import Data.List as DL
import Data.Text
import Graphics.Svg
import Prelude as P

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

main :: IO ()
main =
  let writeSvg f g = writeFile f $ show $ svg g
   in do
        writeSvg "./lines.svg" $ l1 <> l2 <> l3
