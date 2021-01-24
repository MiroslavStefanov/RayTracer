module Lib
    ( traceScene
    ) where

import Tracing.Scene
import PinholeCamera
import Shading.FrameBuffer
import Shading.Color

traceScene :: Int -> Int -> PinholeCamera -> Scene -> FrameBuffer Rgb
traceScene bufferWidth bufferHeight camera scene = undefined 
    