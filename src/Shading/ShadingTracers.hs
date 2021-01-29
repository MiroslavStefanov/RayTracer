module Shading.ShadingTracers where

import Shading.ShadingUtils
import qualified Shading.ShadingContext as SContext
import Shading.Texture
import Shading.Color
import Tracing.Tracer
import Tracing.Scene
import Intersection

import Control.Monad (replicateM)
import Data.Foldable (foldlM)

shootRaysFromIntersectionTracer :: Scene -> SContext.ShadingDensity -> Tracer SContext.ShadingDensity
shootRaysFromIntersectionTracer scene = foldlM tracer [] where
  tracer = \currentDensity (value, weight) -> do 
    let appendedDensity = (value, weight) : currentDensity in
      case value of
        Left _ -> return appendedDensity
        Right (SContext.ShadingContext incidentRay intersection) ->
          case texture intersection of
              InvalidTexture errorMsg -> abortTracer errorMsg
              ColorTexture {} -> return appendedDensity
              PhongTexture {} -> mapM_ (shootRayTowardsLightTracer intersection) (lightSources scene) >> identityTracer appendedDensity
              TransparentTexture {} -> return appendedDensity --todo: log warning and continue
              FrenselTexture {} -> undefined -- TODO: shoot reflected and transmitted rays and add them to the density with respective weights
              -- shootReflectedRay
              -- shootTransimttedRay
              -- reflectRatio = r
              -- (ShadingContext reflectedRay intersection, r * weight) : currentDensity
              -- (ShadingContext transmittedRay intersection, (1-r) * weight) : currentDensity




shadeTracer :: Scene -> SContext.ShadingDensity -> Tracer SContext.ShadingDensity
shadeTracer scene = mapM tracer where
  tracer = \(value, weight) -> do
    case value of
      Left _ -> return (value, weight)
      Right context@(SContext.ShadingContext incidentRay previousIntersection) -> case texture previousIntersection of
        InvalidTexture msg -> abortTracer msg
        ColorTexture {} -> return (value, weight)
        PhongTexture {} -> do
          shadowIntersections <- Control.Monad.replicateM (getLightSourcesCount scene) getIntersectionTracer
          let 
            shadowMultipliers = zipWith getShadowMultiplier shadowIntersections $ lightSources scene
            colorSums = map (getLightContribution context) $ lightSources scene
            shadedColors = zipWith Shading.Color.scale shadowMultipliers colorSums
            accumulatedLightContributions = foldl Shading.Color.add (Rgb 0 0 0) shadedColors
            in
              return (Left $ clamp accumulatedLightContributions, weight)
        TransparentTexture {} -> do
          newIntersection <- getIntersectionTracer
          case newIntersection of
            Nothing -> return (Left white, weight)
            Just intersection -> return (Right $ SContext.ShadingContext incidentRay intersection, weight)
        FrenselTexture {} -> undefined

