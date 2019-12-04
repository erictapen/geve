import Raster
import Codec.Picture
import Codec.Picture.Types
import Data.Either.Unwrap

showLength :: Either String DynamicImage -> String
showLength (Left str) = str
showLength (Right image) = show $ dynamicMap imageWidth image

--- Get Grey value from a pixel in the image
getPixelValue :: DynamicImage -> (Float, Float) -> Pixel8
getPixelValue image (x, y) = let
        absX = round $ (*) x $ fromIntegral $ dynamicMap imageWidth image
        absY = round $ (*) y $ fromIntegral $ dynamicMap imageHeight image
        getValue (PixelRGB8 r g b) = r
        px :: PixelRGB8
        px = pixelAt (convertRGB8 image) absX absY
    in
    getValue px

main :: IO ()
main = do
    image <- readImage "image.png"
    putStrLn $ show $ getPixelValue (fromRight image) (0.0, 0.0)

