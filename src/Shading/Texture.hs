module Shading.Texture where

import Shading.Color

data Texture = 
  InvalidTexture {
    error :: String
  } |
  ColorTexture {
    color :: Rgb
  } | 
  PhongTexture {
    specularMultiplier :: Float,
    specularExponent :: Float
  } 
  deriving (Show, Eq)