module Shading.ShadingTracers where

import Shading.ShadingUtils
import Shading.ShadingContext
import Shading.Texture
import Shading.Color
import Tracing.Tracer
import Tracing.Scene
import Intersection

import Control.Monad (replicateM)

shadeTracer :: Scene -> ShadingValue -> Tracer ShadingValue
shadeTracer _ (Left rgb) = do
  identityTracer $ Left rgb

shadeTracer scene (Right context@(ShadingContext ray intersection)) = do
  case texture intersection of
    InvalidTexture msg -> abortTracer msg
    ColorTexture rgb -> return $ Left rgb
    PhongTexture _ specMult specExp -> do
      shadowIntersections <- Control.Monad.replicateM (getLightSourcesCount scene) getIntersectionTracer
      let 
        shadowMultipliers = zipWith getShadowMultiplier shadowIntersections $ lightSources scene
        colorSums = map (getLightContribution context) $ lightSources scene
        shadedColors = zipWith Shading.Color.scale shadowMultipliers colorSums
        accumulatedLightContributions = foldl Shading.Color.add (Rgb 0 0 0) shadedColors
        in
          return $ Left $ clamp accumulatedLightContributions