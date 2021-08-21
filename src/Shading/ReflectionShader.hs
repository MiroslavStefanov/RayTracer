module Shading.ReflectionShader where

import Shading.Shader
import Shading.Color
import qualified Vector
import Intersection
import Shading.PixelState

data ReflectionShader = ReflectionShader

instance Shading ReflectionShader where
  shading ReflectionShader scene (start, direction) intersection = Tracing reflectedRay where
      reflectedRay = (getPositiveBiasedIntersectionPosition intersection, reflectedDirection)
      reflectedDirection = Vector.reflect direction $ normal intersection


