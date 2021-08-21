module Tracing.Mesh where

import Intersection
import Ray

data Mesh = Mesh {
  testIntersection :: Ray -> Maybe Intersection,
  shaderId :: Int
}

instance Intersectable Mesh where
  intersect r m = testIntersection m r