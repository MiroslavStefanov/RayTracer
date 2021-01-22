module Geometry where

import Ray
import Intersection
import Shading.Texture
import qualified Vector as Vec
import Numeric.Limits

data Geometry = 
  Plane {
    position :: Vec.Vector,
    normal :: Vec.Vector
  } |
  Sphere {
    position :: Vec.Vector,
    radius :: Float
  }

makePlane :: Vec.Vector -> Vec.Vector -> Geometry
makePlane position normal = 
  Plane position $ Vec.normalize normal

computePrimaryTexDir :: Vec.Vector -> Vec.Vector
computePrimaryTexDir normal = Vec.normalize maxDotVec
  where a = Vec.cross normal (1, 0, 0)
        b = Vec.cross normal (0, 1, 0)
        c = Vec.cross normal (0, 0, 1)
        maxDotVec = Vec.maxDot a b c

intersect :: Ray -> Geometry -> Maybe Intersection
intersect ray@(start, direction)
          (Plane position normal)
  |abs denom <= epsilon = Nothing
  |scalar < 0 = Nothing
  |otherwise = Just (Intersection newPosition
                                  normal
                                  (InvalidTexture "blank")
                                  scalar
                                  newCoords)
    where denom = Vec.dot normal direction
          scalar = Vec.dot (Vec.subtract position start) normal / denom
          newPosition = scaleTo scalar ray
          uuVec = computePrimaryTexDir normal
          vvVec = Vec.cross normal uuVec
          uu = Vec.dot uuVec newPosition
          vv = Vec.dot vvVec newPosition
          newCoords = (uu, vv)

intersect ray@(start, direction)
          (Sphere position radius)
  |d <= 0 = Nothing
  |xMin <= 0 = Nothing
  |otherwise = Just (Intersection newPosition
                                  newNormal
                                  (InvalidTexture "blank")
                                  xMin
                                  newCoords)
    where a = Vec.lengthSqr direction
          b = Vec.dot direction (Vec.subtract start position) * 2
          c = Vec.lengthSqr (Vec.subtract start position) - radius * radius
          d = b * b - 4 * a * c
          x1 = (-b + sqrt d) / (2 * a)
          x2 = (-b - sqrt d) / (2 * a)
          xMin = min x1 x2
          newPosition = scaleTo xMin ray
          newNormal = Vec.normalize (Vec.subtract newPosition position)
          uu = (atan2 (Vec.yy newNormal) (Vec.xx newNormal) + pi) / (2 * pi)
          vv = asin (Vec.zz newNormal / radius) * pi
          newCoords = (uu, vv)
