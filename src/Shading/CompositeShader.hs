module Shading.CompositeShader where

import Shading.Texture
import Shading.PixelState
import Shading.Shader
import Tracing.Scene
import qualified Shading.Color as Color

data CompositeShader = CompositeShader [Shader] [Float]

composeShaders :: (Shading s1, Shading s2) => Float -> s1 -> s2 -> CompositeShader
composeShaders alpha first second = CompositeShader [shading first, shading second] [alpha, 1.0 - alpha]

instance Shading CompositeShader where
  shading (CompositeShader shadings weights) scene incommingRay intersection = Composition states weights where
    states = map applyShading shadings
    applyShading shader = shader scene incommingRay intersection
