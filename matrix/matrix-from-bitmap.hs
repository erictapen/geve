import Codec.Picture
import Codec.Picture.Types
import Data.Either.Unwrap
import Data.Vector.Storable
import Debug.Trace
import Graphics.Svg
import Raster
import Prelude as P

--- Get Grey value from a pixel in the image
getPixelValue :: (Show a, RealFloat a) => DynamicImage -> (a, a) -> a
getPixelValue image (x, y) =
  let width = dynamicMap imageWidth image
      height = dynamicMap imageHeight image
      absX = round $ (*) x $ fromIntegral width
      absY = round $ (*) y $ fromIntegral height
      inBounds = (0 <= absX) && (absX < width) && (0 <= absY) && (absY < height)
      getValue (PixelRGB8 r g b) = div ((toInteger r) + (toInteger g) + (toInteger b)) 3
      px :: PixelRGB8
      px = pixelAt (convertRGB8 image) absX absY
      result = (/) ((-) 255 $ fromInteger $ toInteger $ getValue px) 255
   in if inBounds
        then result
        else 0.0

-- then trace ("result: " ++ show result) result
-- else trace ("outofbounds: " ++ show x ++ ", " ++ show y) 0.0

-- generateSvgFromImage :: Image Pixel8 -> Element
-- generateSvgFromImage (Image w h imageData) = g_ [
--     ] $ mconcat

enrichImage :: Image Pixel8 -> [((Int, Int), Pixel8)]
enrichImage (Image w h imageData) = zip [(x, y) | x <- [0 .. h], y <- [0 .. w]] $ toList imageData

main :: IO ()
main = do
  image <- readImage "face.png"
  print $ enrichImage $ convertRGB8 $ fromRight image
-- putStrLn $ show $ getPixelValue (fromRight image) (0.0, 0.0)
