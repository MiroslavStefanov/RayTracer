module LightSource where

import Shading.Color
import Shading.ShadingContext
import Shading.Sampler
import Shading.Texture
import qualified Vector
import Intersection
import Tracing.Tracer

type Lighting = ShadingContext -> Float -> Float -> Rgb
type Occlusion = Tracer Float
type VisibilityCheck = Intersection -> Tracer ()

data Light = Light {
    lighting :: Lighting,
    visibility :: VisibilityCheck,
    occlusion :: Occlusion
}

class LightSource s where
    computeLighting :: s -> Lighting
    checkVisibility :: s -> VisibilityCheck
    getShadowMultiplier :: s -> Occlusion

getLighting :: LightSource s => s -> Light
getLighting s = Light (computeLighting s) (checkVisibility s) (getShadowMultiplier s)

data PointLight = PointLight Float Rgb Vector.Vector deriving (Show, Eq)

instance LightSource PointLight where
    computeLighting 
        (PointLight lIntensity lColor lPosition) 
        (ShadingContext (rayOrigin, rayDirection) intersection)
        specularMultiplier specularExponent = let
            vectorToLight = lPosition `Vector.subtract` position intersection
            directionToLight = Vector.normalize vectorToLight
            lambertCoefficient = directionToLight `Vector.dot` normal intersection
            lightFactor = lIntensity / Vector.lengthSqr vectorToLight
            reflectedVector = Vector.invert directionToLight `Vector.reflect` normal intersection
            phongCoefficient = max 0.0 (Vector.invert rayDirection `Vector.dot` reflectedVector) ** specularExponent

            lightFinalColor = scale (lambertCoefficient * lightFactor) lColor
            specularColor = scale (phongCoefficient * lightFactor * specularMultiplier) white
            diffuseColor = sample (diffuseSampler $ texture intersection) $ coordinates intersection
            in
                multiply diffuseColor lightFinalColor `add` specularColor
    
    checkVisibility 
        (PointLight _ _ lPosition) 
        intersection@(Intersection intPosition _ _ _ _) = shootRayTracer rayToLight where
            rayStart = getPositiveBiasedIntersectionPosition intersection
            rayDirection = Vector.normalize $ Vector.subtract lPosition intPosition
            --rayToLight = (Vector.add rayStart $ Vector.scale 0.001 rayDirection, rayDirection)
            rayToLight = (getPositiveBiasedIntersectionPosition intersection, rayDirection)
    
    getShadowMultiplier 
        (PointLight _ _ lPosition) = do
            shadowIntersection <- getIntersectionTracer
            case shadowIntersection of
                Nothing -> identityTracer 1.0
                Just intersection -> identityTracer $ 
                    if distance intersection ^ 2 < distanceToLightSquared then 1 - alpha else 1.0 where
                        distanceToLightSquared = Vector.lengthSqr $ rayOrigin `Vector.subtract` lPosition
                        rayOrigin = getPositiveBiasedIntersectionPosition intersection
                        alpha = sample (alphaSampler $ texture intersection) $ coordinates intersection
