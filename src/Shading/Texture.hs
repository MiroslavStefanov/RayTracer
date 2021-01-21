module Shading.Texture where

import Shading.Color

data Texture = 
  ColorTexture {
    color :: Rgb
  }