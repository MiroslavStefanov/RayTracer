module Intersection where

import Vector

type MeshId = Integer

data Intersection = Intersection {
    position :: Vector,
    normal :: Vector,
    meshId :: MeshId,
    distance :: Float,
    coordinates :: (Float, Float)
}