module Lighting.PointLight where

import Shading.Color
import qualified Vector
import Lighting.LightSource
import Intersection
import Shading.Sampler
import Shading.Texture

data PointLight = PointLight Float Rgb Vector.Vector deriving (Show, Eq)

instance LightSource PointLight where
    lighting (PointLight intensity color point) (start, direction) intersection texture = finalColor where
        vectorToLight = point `Vector.subtract` position intersection
        directionToLight = Vector.normalize vectorToLight
        lambertCoefficient = directionToLight `Vector.dot` normal intersection
        lightFactor = intensity / Vector.lengthSqr vectorToLight
        reflectedVector = Vector.invert directionToLight `Vector.reflect` normal intersection
        lightFinalColor = scale (lambertCoefficient * lightFactor) color
        (specularExponent, specularMultiplier) = sample (specularSampler texture) $ coordinates intersection
        phongCoefficient = max 0.0 (Vector.invert direction `Vector.dot` reflectedVector) ** specularExponent
        specularColor = scale (phongCoefficient * lightFactor * specularMultiplier) white
        diffuseColor = sample (diffuseSampler texture) $ coordinates intersection
        finalColor = multiply diffuseColor lightFinalColor `add` specularColor
    occlusion (PointLight intensity color point) intersection = Ray (rayStart, rayDirection) where
        rayStart = getPositiveBiasedIntersectionPosition intersection
        rayDirection = Vector.normalize $ Vector.subtract point $ position intersection
        rayToLight = (getPositiveBiasedIntersectionPosition intersection, rayDirection)

