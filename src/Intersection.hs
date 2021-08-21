module Intersection where

import Vector
import Ray
import Base

data Intersection = Intersection {
  position :: Vector,
  normal :: Vector,
  distance :: Float,
  coordinates :: Texel
} deriving (Show, Eq)

class Intersectable a where
  intersect :: Ray -> a -> Maybe Intersection

biasEpsilon :: Float
biasEpsilon = 0.001

getPositiveBiasedIntersectionPosition :: Intersection  -> Vector 
getPositiveBiasedIntersectionPosition (Intersection position normal _ _) = position `add` scale biasEpsilon normal

getNegativeBiasedIntersectionPostion :: Intersection -> Vector 
getNegativeBiasedIntersectionPostion (Intersection position normal _ _) = position `Vector.subtract` scale biasEpsilon normal
