module Shading.PhongShader where

import Shading.Texture
import Shading.PixelState
import Shading.Shader
import Tracing.Scene
import qualified Shading.Color as Color
import Lighting.LightSource
import Intersection
import Vector (xx, yy, zz)
import Tracing.Mesh

newtype PhongShader = PhongShader Texture

instance Shading PhongShader where
  shading (PhongShader texture) scene incommingRay i = Ready totalColor where
    totalColor = Color.clamp $ foldl Color.add Color.black lightColors
    shadowChecks = map applyChecks $ lightSources scene
    applyChecks (_, occlusion) = occlusion i
    shadowFactors = map getShadowFactor shadowChecks
    getShadowFactor NoCheck = 1.0
    getShadowFactor (Ray shadowRay distanceSqr) = let
        shadowHit = traceRay scene shadowRay
        in case shadowHit of
            Nothing -> 1.0
            Just (Hit m int) -> if (distance int ** 2.0) < distanceSqr then 0.0 else 1.0
    lightColors = zipWith occludeLighting shadowFactors lights
    lights = map fst $ lightSources scene
    occludeLighting factor light = if factor < 0.01 then Color.black else factor `Color.scale` light incommingRay i texture

