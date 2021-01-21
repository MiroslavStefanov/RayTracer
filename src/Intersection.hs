module Intersection where

import Vector
import Base
import Shading.Texture

data Intersection = Intersection {
    position :: Vector,
    normal :: Vector,
    texture :: Texture,
    distance :: Float,
    coordinates :: Texel
}