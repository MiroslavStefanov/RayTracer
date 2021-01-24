module Export where

import Base
import Shading.FrameBuffer
import Shading.Color ( Rgb(Rgb) )
import Codec.Picture

generatorFunc :: FrameBuffer Rgb -> Int -> Int -> PixelRGBF
generatorFunc (FrameBuffer w _ buffer) x y = PixelRGBF rr gg bb
  where index = y * w + x
        (Rgb rr gg bb) = buffer !! index

frameBufferToDynamicImage :: FrameBuffer Rgb -> DynamicImage
frameBufferToDynamicImage fBuffer@(FrameBuffer w h _) =
  ImageRGBF (generateImage (generatorFunc fBuffer) w h)  

saveImageAsPng :: FilePath -> FrameBuffer Rgb -> IO ()
saveImageAsPng path fBuffer =
  savePngImage path $ frameBufferToDynamicImage fBuffer