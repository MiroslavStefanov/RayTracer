module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Texture = 
  InvalidTexture {
    error :: String
  } |
  ColorTexture {
    colorSampler :: Sampler
  } | 
  PhongTexture {
    diffuseSampler :: Sampler,
    specularMultiplier :: Float,
    specularExponent :: Float
  } |
  FrenselTexture {
    eta :: Float
  } |
  TransparentTexture
  deriving (Show, Read, Eq)
