module Shading.ShadingContext where

import Ray
import Intersection

data ShadingContext = ShadingContext {
  incommingRay :: Ray,
  intersection :: Maybe Intersection
}