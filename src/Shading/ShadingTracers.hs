module Shading.ShadingTracers where

import Shading.ShadingUtils
import qualified Shading.ShadingContext as SContext
import Shading.Texture
import Shading.Color
import Tracing.Tracer
import Tracing.Scene
import Intersection
import qualified Vector

import Control.Monad (replicateM)
import Data.Foldable (foldlM)
import Shading.Sampler

shootRaysFromIntersectionTracer :: Scene -> SContext.ShadingDensity -> Tracer SContext.ShadingDensity
shootRaysFromIntersectionTracer scene = foldlM tracer [] where
  tracer = \totalDensity d@(value, weight) -> do 
    case value of
      Left _ -> return $ d : totalDensity
      Right (SContext.ShadingContext (rayOrigin, rayDirection) intersection) -> let 
        t = texture intersection 
        diffuse = diffuseSampler t
        reflectedDirection = Vector.reflect rayDirection $ normal intersection
        reflectedRay = (getPositiveBiasedIntersectionPosition intersection, reflectedDirection)
        in case material t of
            PhongMaterial _ _ -> mapM_ (shootRayTowardsLightTracer intersection) (lightSources scene) >> identityTracer (d : totalDensity)
            FresnelMaterial eta strength -> let
              newIntersection = addTexture intersection emptyTexture
              ownColorDensity = (Left (sample diffuse (coordinates intersection)), (1 - strength) * weight) : totalDensity
              reflectionRatio = min 1.0 (Vector.rSchlick2 (normal intersection) rayDirection 1 eta)
              maybeTransmittedDirection = Vector.refract (normal intersection) rayDirection 1 eta
              reflectedDensity = (Right (SContext.ShadingContext reflectedRay newIntersection), reflectionRatio * strength * weight)
              in do
                shootRayTracer reflectedRay
                case maybeTransmittedDirection of
                  Just direction -> let 
                    transmittedRay = (getNegativeBiasedIntersectionPostion intersection, direction)
                    transmittedDensity = (Right (SContext.ShadingContext transmittedRay newIntersection), (1 - reflectionRatio) * strength * weight)
                    in shootRayTracer transmittedRay >> identityTracer (reflectedDensity : (transmittedDensity : ownColorDensity))
                  Nothing -> return (reflectedDensity : ownColorDensity)
              



shadeTracer :: Scene -> SContext.ShadingDensity -> Tracer SContext.ShadingDensity
shadeTracer scene = mapM tracer where
  tracer = \(value, weight) -> do
    case value of
      Left _ -> return (value, weight)
      Right context@(SContext.ShadingContext incidentRay previousIntersection) -> let
        t = texture previousIntersection
        in case material t of
          PhongMaterial specularMultiplier specularExponent -> do
            shadowIntersections <- Control.Monad.replicateM (getLightSourcesCount scene) getIntersectionTracer
            let 
              shadowMultipliers = zipWith getShadowMultiplier shadowIntersections $ lightSources scene
              colorSums = map (getPhongLightingColor context specularMultiplier specularExponent) $ lightSources scene
              shadedColors = zipWith Shading.Color.scale shadowMultipliers colorSums
              accumulatedLightContributions = foldl (Shading.Color.add . Shading.Color.clamp) (Rgb 0 0 0) shadedColors
              in
                return (Left accumulatedLightContributions, weight)
          NoMaterial -> do
            newIntersection <- getIntersectionTracer
            case newIntersection of
              Just intersection -> identityTracer (Right (SContext.ShadingContext incidentRay intersection), weight)
              Nothing -> identityTracer (Left (Rgb weight weight weight), weight)
