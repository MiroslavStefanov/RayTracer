module Shading.ShadingUtils where

import Shading.ShadingContext
import Shading.Color
import Intersection
import qualified LightSource as LS
import qualified Vector as Vec
import Numeric.Limits (epsilon)
import Shading.Texture
import Shading.Sampler

getShadowRayStartPoint :: Intersection -> Vec.Vector
getShadowRayStartPoint (Intersection start normal _ _ _) = 
  Vec.add start $ Vec.scale epsilon normal

getShadowMultiplier :: Maybe Intersection -> LS.LightSource -> Float
getShadowMultiplier Nothing _ = 1.0
getShadowMultiplier _ LS.AmbientLight {} = undefined
getShadowMultiplier (Just intersection) (LS.PointLight _ _ lightPosition) = 
  if distance intersection ^ 2 < distanceToLightSquared then 0.1 else 1.0 where
    distanceToLightSquared = Vec.lengthSqr $ Vec.subtract rayStart lightPosition
    rayStart = getShadowRayStartPoint intersection

getLightContribution :: ShadingContext -> LS.LightSource -> Rgb
getLightContribution 
  (ShadingContext (rayStart, rayDirection) intersection) 
  (LS.PointLight lightIntensity lightColor lightPosition) = let
    vectorToLight = Vec.subtract lightPosition $ position intersection
    directionToLight = Vec.normalize vectorToLight
    lambertCoefficient = Vec.dot directionToLight $ normal intersection
    lightFactor = lightIntensity / Vec.lengthSqr vectorToLight
    reflectedVector = Vec.reflect (Vec.invert directionToLight) $ normal intersection
    phongCoefficient = max 0.0 $ Vec.dot (Vec.invert rayDirection) reflectedVector ** specularExponent (texture intersection)

    lightFinalColor = scale (lambertCoefficient * lightFactor) lightColor
    specularColor = scale (phongCoefficient * lightFactor * specularMultiplier (texture intersection)) white
    diffuseColor = sample (diffuseSampler $ texture intersection) $ coordinates intersection
    in
      multiply diffuseColor $ add lightFinalColor specularColor

