module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Material =
  PhongMaterial |
  ReflectiveMaterial Float |
  FresnelMaterial Float Float |
  NoMaterial
  deriving (Show, Read, Eq)

data Texture = Texture {
  diffuseSampler :: Sampler Rgb,
  alphaSampler :: Sampler Float,
  specularMultiplier :: Float,
  specularExponent :: Float,
  material :: Material
}

emptyTexture :: Texture
emptyTexture = Texture emptyDiffuse emptyAlpha 0 0 NoMaterial where
  emptyDiffuse = Sampler undefined 
  emptyAlpha = Sampler undefined
