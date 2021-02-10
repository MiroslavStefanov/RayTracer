module Shading.ShadingUtils where

import Shading.ShadingContext
import Shading.Color
import Intersection
import qualified LightSource as LS
import qualified Vector as Vec
import Shading.Texture
import Shading.Sampler

getShadowMultiplier :: Maybe Intersection -> LS.LightSource -> Float
getShadowMultiplier Nothing _ = 1.0
getShadowMultiplier _ LS.AmbientLight {} = undefined
getShadowMultiplier (Just intersection) (LS.PointLight _ _ lightPosition) = 
  if distance intersection ^ 2 < distanceToLightSquared then 1 - alpha else 1.0 where
    distanceToLightSquared = Vec.lengthSqr $ rayOrigin `Vec.subtract` lightPosition
    rayOrigin = getPositiveBiasedIntersectionPosition intersection
    alpha = sample (alphaSampler $ texture intersection) $ coordinates intersection

getPhongLightingColor :: ShadingContext -> Float -> Float -> LS.LightSource -> Rgb
getPhongLightingColor 
  (ShadingContext (rayOrigin, rayDirection) intersection) specularMultiplier specularExponent
  (LS.PointLight lightIntensity lightColor lightPosition) = let
    vectorToLight = lightPosition `Vec.subtract` position intersection
    directionToLight = Vec.normalize vectorToLight
    lambertCoefficient = Vec.dot directionToLight $ normal intersection
    lightFactor = lightIntensity / Vec.lengthSqr vectorToLight
    reflectedVector = Vec.invert directionToLight `Vec.reflect` normal intersection
    phongCoefficient = max 0.0 (Vec.invert rayDirection `Vec.dot` reflectedVector) ** specularExponent

    lightFinalColor = scale (lambertCoefficient * lightFactor) lightColor
    specularColor = scale (phongCoefficient * lightFactor * specularMultiplier) white
    diffuseColor = sample (diffuseSampler $ texture intersection) $ coordinates intersection
    in
      multiply diffuseColor lightFinalColor `add` specularColor
