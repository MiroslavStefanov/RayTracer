module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Texture = Texture {
  diffuseSampler :: Sampler Rgb,
  specularSampler :: Sampler (Float, Float)
}

solidColorTexture :: Rgb -> Texture
solidColorTexture color = Texture (constantSampler color) (constantSampler (1, 1))
