{-# LANGUAGE OverloadedStrings #-}

import Graphics.Svg

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "482", Height_ <<- "340"]

logo :: RealFloat a => a -> Element
logo pos = 
    g_ [Transform_ <<- translate pos 0] 
        (path_ [ Fill_ <<- "#000000"
            , D_ <<- ( mA 0 340 <> lA 113 170 <> lA 0 0 <> lA 85 0
                <> lA 198 170 <> lA 85 340 <> lA 0 340 <> z <> mA 0 340 ) ])

chain :: RealFloat a => [a] -> Element
chain positions = mconcat (map (\pos -> g_ [Transform_ <<- translate pos 0] (logo 100)) positions)

main :: IO ()
main = do
    print $ svg $ chain [ 0, 100, 200, 300, 400, 500, 600 ]
