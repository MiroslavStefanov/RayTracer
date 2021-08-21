module Lighting.AmbientLight where

import Lighting.LightSource
import Shading.Color
import Intersection

data AmbientLight = AmbientLight Float Rgb deriving (Show, Eq)

instance LightSource AmbientLight where
  lighting (AmbientLight intensity color) (start, direction) intersection texture = scale intensity color