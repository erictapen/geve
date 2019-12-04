import Raster
import Codec.Picture
import Codec.Picture.Types
import Data.Either.Unwrap

import Debug.Trace

showLength :: Either String DynamicImage -> String
showLength (Left str) = str
showLength (Right image) = show $ dynamicMap imageWidth image

--- Get Grey value from a pixel in the image
getPixelValue :: (Show a, RealFloat a) => DynamicImage -> (a, a) -> a
getPixelValue image (x, y) = let
        width = dynamicMap imageWidth image
        height = dynamicMap imageHeight image
        absX = round $ (*) x $ fromIntegral width
        absY = round $ (*) y $ fromIntegral height
        inBounds = (0 <= absX) && (absX < width) && (0 <= absY) && (absY < height)
        getValue (PixelRGB8 r g b) = div ((toInteger r) + (toInteger g) + (toInteger b)) 3
        px :: PixelRGB8
        px = pixelAt (convertRGB8 image) absX absY
        result = (/) ((-) 255 $ fromInteger $ toInteger $ getValue px) 255
    in
    if inBounds
         then result
         else 0.0
         -- then trace ("result: " ++ show result) result
         -- else trace ("outofbounds: " ++ show x ++ ", " ++ show y) 0.0

main :: IO ()
main = do
    image <- readImage "face.png"
    print $ triangleRasterResult $ getPixelValue (fromRight image)
    -- putStrLn $ show $ getPixelValue (fromRight image) (0.0, 0.0)

