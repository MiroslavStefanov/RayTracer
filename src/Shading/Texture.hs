module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Texture = 
  InvalidTexture {
    error :: String
  } |
  ColorTexture {
    color :: Rgb
  } | 
  PhongTexture {
    diffuseSampler :: Sampler,
    specularMultiplier :: Float,
    specularExponent :: Float
  } 
  deriving (Show, Eq)