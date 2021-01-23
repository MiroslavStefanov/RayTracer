module Shading.ShadingContext where

import Ray
import Intersection
import Shading.Texture

data ShadingContext = ShadingContext {
  incommingRay :: Ray,
  intersection :: Maybe Intersection
} deriving (Show, Eq)