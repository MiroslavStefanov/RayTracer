module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Texture = Texture {
  diffuseSampler :: Sampler Rgb,
  specularSampler :: Sampler (Float, Float)
}

solidColorTexture :: Rgb -> Texture
solidColorTexture color = Texture (constantSampler color) (constantSampler (1, 1))

metalicColorTexture :: Rgb -> Texture
metalicColorTexture color = Texture (constantSampler color) (constantSampler (20, 3))

solidColorCheckerTexture :: Rgb -> Rgb -> Float -> Texture
solidColorCheckerTexture col1 col2 size = Texture (checkerSampler col1 col2 size) (constantSampler (1, 1))

shinyColorCheckerTexture :: Rgb -> Rgb -> Float -> Texture
shinyColorCheckerTexture col1 col2 size = Texture (checkerSampler col1 col2 size) (constantSampler (8, 1))