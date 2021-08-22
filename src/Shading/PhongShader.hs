module Shading.PhongShader where

import Shading.Texture
import Shading.PixelState
import Shading.Shader
import Tracing.Scene
import qualified Shading.Color as Color
import Lighting.LightSource

newtype PhongShader = PhongShader Texture

instance Shading PhongShader where
  shading (PhongShader texture) scene incommingRay intersection = Ready $ Color.clamp $ foldl Color.add Color.black lightColors where
    shadowChecks = map applyChecks $ lightSources scene
    applyChecks (_, occlusion) = occlusion intersection
    shadowFactors = map getShadowFactor shadowChecks
    getShadowFactor NoCheck = 1.0
    getShadowFactor (Ray shadowRay) = let
        shadowHit = traceRay scene shadowRay
        in case shadowHit of
            Nothing -> 1.0
            Just _ -> 0.0

    lightColors = zipWith occludeLighting shadowFactors $ map fst $ lightSources scene
    occludeLighting factor light = if factor < 0.01 then Color.black else factor `Color.scale` light incommingRay intersection texture
