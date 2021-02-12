module Shading.ShadingTracers where

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
import LightSource
import Control.Monad.Zip (MonadZip(mzipWith))

shootRaysFromIntersectionTracer :: Scene -> SContext.ShadingDensity -> Tracer SContext.ShadingDensity
shootRaysFromIntersectionTracer scene = foldlM tracer [] where
  tracer = \totalDensity d@(value, weight) -> do 
    case value of
      Left _ -> return $ d : totalDensity
      Right (SContext.ShadingContext ray@(rayOrigin, rayDirection) intersection) -> let 
        t = texture intersection 
        diffuse = diffuseSampler t
        reflectedDirection = Vector.reflect rayDirection $ normal intersection
        reflectedRay = (getPositiveBiasedIntersectionPosition intersection, reflectedDirection)
        emptyIntersection = addTexture intersection emptyTexture
        reflectedContext = SContext.ShadingContext reflectedRay emptyIntersection
        diffuseContext = SContext.ShadingContext ray (addTexture intersection $ Texture diffuse (alphaSampler t) (specularMultiplier  t) (specularExponent t) PhongMaterial)
        in do
          mapM_ (`visibility` intersection) (lightSources scene)
          case material t of
            PhongMaterial ->  return (d : totalDensity)
            ReflectiveMaterial strength -> do
              shootRayTracer reflectedRay
              return $ (Right diffuseContext, (1 - strength) * weight) : (Right reflectedContext, strength * weight) : totalDensity 
            FresnelMaterial eta strength -> let
              reflectionRatio = min 1.0 (Vector.rSchlick2 (normal intersection) rayDirection 1 eta)
              reflectedDensity = (Right reflectedContext, reflectionRatio * strength * weight)
              maybeTransmittedDirection = Vector.refract (normal intersection) rayDirection 1 eta
              in do
                shootRayTracer reflectedRay
                case maybeTransmittedDirection of
                  Just direction -> let 
                    transmittedRay = (getNegativeBiasedIntersectionPostion intersection, direction)
                    transmittedDensity = (Right (SContext.ShadingContext transmittedRay emptyIntersection), (1 - reflectionRatio) * strength * weight)
                    in 
                      shootRayTracer transmittedRay >> identityTracer 
                      ((Right diffuseContext, (1 - strength) * weight) :
                      (reflectedDensity : 
                      (transmittedDensity : totalDensity)))
                  Nothing -> return (reflectedDensity : totalDensity)
              



shadeTracer :: Scene -> SContext.ShadingDensity -> Tracer SContext.ShadingDensity
shadeTracer scene = mapM tracer where
  tracer = \(value, weight) -> do
    case value of
      Left _ -> return (value, weight)
      Right context@(SContext.ShadingContext incidentRay previousIntersection) -> let
        t = texture previousIntersection
        in case material t of
          PhongMaterial -> do
            shadowMultipliers <- mapM (`occlusion` position previousIntersection) (lightSources scene)
            let 
              colorSums = map (`lighting` context) $ lightSources scene
              shadedColors = zipWith Shading.Color.scale shadowMultipliers colorSums
              accumulatedLightContributions = foldl (Shading.Color.add . Shading.Color.clamp) (Rgb 0 0 0) shadedColors
              in
                return (Left accumulatedLightContributions, weight)
          NoMaterial -> do
            newIntersection <- getIntersectionTracer
            case newIntersection of
              Just intersection -> identityTracer (Right (SContext.ShadingContext incidentRay intersection), weight)
              Nothing -> identityTracer (Left (Rgb weight weight weight), weight)
