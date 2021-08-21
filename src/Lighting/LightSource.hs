module Lighting.LightSource where

import Ray
import Intersection
import Shading.Texture
import Shading.Color
type Lighting = Ray -> Intersection -> Texture -> Rgb

class LightSource s where
    lighting :: s -> Lighting