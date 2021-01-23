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

addTexture :: Maybe Intersection -> Texture -> Maybe Intersection
addTexture Nothing _ = Nothing
addTexture (Just (Intersection pos normal _ distance coords)) texture =
  Just (Intersection pos normal texture distance coords)

closerIntersection :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
closerIntersection Nothing Nothing = Nothing
closerIntersection Nothing ii = ii
closerIntersection ii Nothing = ii
closerIntersection i1@(Just (Intersection _ _ _ dist1 _))
                   i2@(Just (Intersection _ _ _ dist2 _))
  |dist1 < dist2 = i1
  |otherwise = i2                   