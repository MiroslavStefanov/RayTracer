module Shading.PhongShader where

import Shading.Texture
import Shading.PixelState
import Shading.Shader
import Tracing.Scene
import qualified Shading.Color as Color

newtype PhongShader = PhongShader Texture

instance Shading PhongShader where
  shading (PhongShader texture) scene incommingRay intersection = Ready $ Color.clamp $ foldl Color.add Color.black lights where
    lights = map applyLighting $ lightSources scene
    applyLighting lighting = lighting incommingRay intersection texture
