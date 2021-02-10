module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Material =
  PhongMaterial Float Float |
  ReflectiveMaterial Float |
  FresnelMaterial Float Float |
  NoMaterial
  deriving (Show, Read, Eq)

data Texture = Texture {
  diffuseSampler :: Sampler Rgb,
  alphaSampler :: Sampler Float,
  material :: Material
}

emptyTexture :: Texture
emptyTexture = Texture emptyDiffuse emptyAlpha NoMaterial where
  emptyDiffuse = Sampler undefined 
  emptyAlpha = Sampler undefined
