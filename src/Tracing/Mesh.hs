module Tracing.Mesh where

import Geometry
import Shading.Texture

data Mesh = Mesh {
  geometry :: Geometry,
  texture :: Texture
}

instance Intersectable Mesh where
  intersect ray (Mesh geometry texture) = intersect ray geometry