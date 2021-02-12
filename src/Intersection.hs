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
} deriving (Show, Eq)

emptyIntersection :: Intersection
emptyIntersection = Intersection (0,0,0) (0,0,0) emptyTexture 0 (0, 0) 

addTexture :: Intersection -> Texture -> Intersection
addTexture (Intersection pos normal _ distance coords) texture =
  Intersection pos normal texture distance coords

closerIntersection :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
closerIntersection Nothing Nothing = Nothing
closerIntersection Nothing ii = ii
closerIntersection ii Nothing = ii
closerIntersection i1@(Just (Intersection _ _ _ dist1 _))
                   i2@(Just (Intersection _ _ _ dist2 _))
  |dist1 < dist2 = i1
  |otherwise = i2    

biasEpsilon :: Float
biasEpsilon = 0.001

getPositiveBiasedIntersectionPosition :: Intersection  -> Vector 
getPositiveBiasedIntersectionPosition (Intersection position normal _ _ _) = position `add` scale biasEpsilon normal

getNegativeBiasedIntersectionPostion :: Intersection -> Vector 
getNegativeBiasedIntersectionPostion (Intersection position normal _ _ _) = position `Vector.subtract` scale biasEpsilon normal
