module Rendering where

import qualified SDL
import qualified SDL.Video.Renderer as Renderer
import Data.Text
import Shading.FrameBuffer
import Shading.Color
import Data.Vector.Storable.Mutable
import Data.Vector.Storable
import Data.Word

openWindow :: Int -> Int -> IO (SDL.Window, SDL.Renderer)
openWindow width height = do
  SDL.initializeAll
  window <- SDL.createWindow (pack "RayTracer") SDL.defaultWindow
  SDL.windowSize window SDL.$= SDL.V2 (fromIntegral width) (fromIntegral height)
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
  return (window, renderer)

createFrameBufferSurface :: FrameBuffer Rgb -> IO SDL.Surface
createFrameBufferSurface (FrameBuffer w h rgbPixels) = 
  let unpackedPixels = fromList $ unpackRgb24 rgbPixels
  in
    do
      pixels <- thaw unpackedPixels
      SDL.createRGBSurfaceFrom pixels (fromIntegral <$> SDL.V2 w h) (3 * fromIntegral w) SDL.RGB24

renderFrameBuffer :: SDL.Renderer -> FrameBuffer Rgb -> IO ()
renderFrameBuffer renderer fBuffer = do
  SDL.clear renderer
  surface <- createFrameBufferSurface fBuffer
  texutre <- SDL.createTextureFromSurface renderer surface
  SDL.copy renderer texutre Nothing Nothing
  SDL.present renderer
  renderFrameBuffer renderer fBuffer