module Export where

import Base
import Shading.FrameBuffer
import Shading.Color ( Rgb(Rgb) )
import Codec.Picture
import Data.Time.Clock

generatorFunc :: FrameBuffer Rgb -> Int -> Int -> PixelRGBF
generatorFunc (FrameBuffer w _ buffer) x y = PixelRGBF rr gg bb
  where index = y * w + x
        (Rgb rr gg bb) = buffer !! index

frameBufferToDynamicImage :: FrameBuffer Rgb -> DynamicImage
frameBufferToDynamicImage fBuffer@(FrameBuffer w h _) =
  ImageRGBF (generateImage (generatorFunc fBuffer) w h)  

saveImageAsPng :: FilePath -> FrameBuffer Rgb -> IO ()
saveImageAsPng path fBuffer = do
  putStrLn $ "Writing image " ++ path
  startTime <- getCurrentTime
  savePngImage path $ frameBufferToDynamicImage fBuffer
  endTime <- getCurrentTime
  putStr "[Saving Image]Seconds elapsed: "
  print $ nominalDiffTimeToSeconds (endTime `diffUTCTime` startTime)