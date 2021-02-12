module Shading.ShadingContext where

import Ray
import Intersection
import Shading.Texture
import Shading.Color
import Tracing.Tracer

data ShadingContext = ShadingContext {
  incommingRay :: Ray,
  intersection :: Intersection
}

type ShadingValue = Either Rgb ShadingContext
type ShadingDensity = [(ShadingValue, Float)]

getShadingColor :: ShadingDensity -> Rgb
getShadingColor [] = Rgb 0 0 0
getShadingColor density = foldl addWeightedColor (Rgb 0 0 0) density where
  addWeightedColor = \totalColor (value, weight) ->
    case value of
      Left color -> clamp (totalColor `add` scale weight color)
      Right _ -> Rgb 0 0 0

initialDensityTracer :: Ray -> Tracer ShadingDensity
initialDensityTracer ray = identityTracer [(Right (ShadingContext ray emptyIntersection), 1)]