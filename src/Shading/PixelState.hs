module Shading.PixelState where

import Shading.Color
import Ray

import Tracing.Scene

data PixelState = 
  Ready Rgb |
  Tracing Ray |
  Composition [PixelState] [Float] |
  Shading Ray Hit

colorizePixel :: PixelState -> Rgb
colorizePixel (Ready color) = color
colorizePixel _ = black

canColorizePixel :: PixelState -> Bool
canColorizePixel (Ready _) = True
canColorizePixel _ = False