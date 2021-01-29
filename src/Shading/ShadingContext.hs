module Shading.ShadingContext where

import Ray
import Intersection
import Shading.Texture
import Shading.Color

data ShadingContext = ShadingContext {
  incommingRay :: Ray,
  intersection :: Intersection
} deriving (Show, Eq)

type ShadingValue = Either Rgb ShadingContext
type ShadingDensity = [(ShadingValue, Float)]