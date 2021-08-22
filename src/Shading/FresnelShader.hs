module Shading.FresnelShader where

import Shading.Texture
import Shading.PixelState
import Shading.Shader
import Tracing.Scene
import qualified Shading.Color as Color
import qualified Vector
import Intersection

newtype FresnelShader = FresnelShader{ eta :: Float }

instance Shading FresnelShader where
  shading (FresnelShader eta) scene incommingRay@(rayOrigin, rayDirection) intersection = let
    reflectionRatio = min 1.0 (Vector.rSchlick2 (normal intersection) rayDirection 1 eta)
    reflectedShader = Tracing reflectedRay
    reflectedRay = (getPositiveBiasedIntersectionPosition intersection, reflectedDirection)
    reflectedDirection = Vector.reflect rayDirection $ normal intersection
    maybeTransmittedDirection = Vector.refract (normal intersection) rayDirection 1 eta
    in case maybeTransmittedDirection of
      Nothing -> reflectedShader
      Just direction -> let
          transmittedRay = (getNegativeBiasedIntersectionPostion intersection, direction)
          transmittedShader = Tracing transmittedRay
          in 
            Composition [reflectedShader, transmittedShader] [reflectionRatio, 1.0 - reflectionRatio]
