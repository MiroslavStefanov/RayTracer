module Geometry where

import Ray
import Intersection
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

intersect :: Ray -> Intersection -> Geometry -> Maybe Intersection
intersect ray@(start, direction, _)
          (Intersection interPos _ mId _ _) 
          (Plane geoPos geoNorm)
  |abs denom <= epsilon = Nothing
  |scalar < 0 = Nothing
  |otherwise = Just (Intersection newPosition
                                  geoNorm
                                  mId
                                  scalar
                                  newCoords)
    where denom = Vec.dot geoNorm direction
          scalar = Vec.dot (Vec.subtract geoPos start) geoNorm / denom
          newPosition = scaleTo scalar ray
          newCoords = (Vec.xx interPos, Vec.yy interPos)

intersect ray@(start, direction, _)
          (Intersection interPos interNorm mId _ _) 
          (Sphere geoPos radius)
  |d <= 0 = Nothing
  |xMin <= 0 = Nothing
  |otherwise = Just (Intersection newPosition
                                  newNormal
                                  mId
                                  xMin
                                  newCoords)
    where a = Vec.lengthSqr direction
          b = Vec.dot direction (Vec.subtract start geoPos) * 2
          c = Vec.lengthSqr (Vec.subtract start geoPos) - radius * radius
          d = b * b - 4 * a * c
          x1 = (-b + sqrt d) / (2 * a)
          x2 = (-b - sqrt d) / (2 * a)
          xMin = min x1 x2
          newPosition = scaleTo xMin ray
          newNormal = Vec.normalize (Vec.subtract interPos geoPos)
          uu = (atan2 (Vec.yy interNorm) (Vec.xx interNorm) + pi) / (2 * pi)
          vv = asin (Vec.zz interNorm / radius) * pi
          newCoords = (uu, vv)
