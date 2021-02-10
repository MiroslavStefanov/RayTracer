module Tracing.Mesh where

import Geometry
import Shading.Texture

data Mesh = Mesh {
  geometry :: Geometry,
  texture :: Texture
}