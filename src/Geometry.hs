module Geometry where

import Ray
import Intersection
import Shading.Texture
import qualified Vector as Vec
import Numeric.Limits
import Solver

data Geometry = 
  Plane {
    position :: Vec.Vector,
    normal :: Vec.Vector
  } |
  Sphere {
    position :: Vec.Vector,
    radius :: Float
  } |
  Triangle {
    aa :: Vec.Vector,
    bb :: Vec.Vector,
    cc :: Vec.Vector
  } |
  Parallelepiped {
    position :: Vec.Vector,
    xAxis :: Vec.Vector,
    yAxis :: Vec.Vector,
    zAxis :: Vec.Vector,
    halfLength :: Vec.Vector
  } |
  Torus {
    position :: Vec.Vector,
    sweptRadius :: Float,
    tubeRadius :: Float
  } |
  Cone {
    position :: Vec.Vector,
    radius :: Float,
    height :: Float,
    phiMax :: Float
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

intersect ray@(start, direction) (Triangle aa bb cc)
  |det < epsilon = Nothing
  |u < 0 || u > det = Nothing
  |v < 0 || u + v > det = Nothing
  |uu < 0 || uu > 1 = Nothing
  |vv < 0 || uu + vv > 1 = Nothing
  |otherwise = Just (Intersection newPosition
                                  normal
                                  (InvalidTexture "blank")
                                  distance
                                  newCoords)
    where e1 = Vec.subtract bb aa
          e2 = Vec.subtract cc aa
          pVec = Vec.cross direction e2
          normDirection = Vec.normalize direction
          det = Vec.dot pVec e1
          tVec = Vec.subtract start aa
          u = Vec.dot tVec pVec
          qVec = Vec.cross tVec e1
          v = Vec.dot normDirection qVec
          invDet = 1 / det
          uu = invDet * Vec.dot tVec pVec
          vv = invDet * Vec.dot qVec normDirection
          distance = invDet * Vec.dot e2 qVec
          newPosition = scaleTo distance ray
          normal = Vec.cross e1 e2
          newCoords = (uu, vv)

intersect ray@(start, direction)
          (Parallelepiped position aa bb cc _) = Nothing

intersect ray@(start, direction)
          (Cone position radius height phiMax) = Nothing          

intersect ray@(origStart, origDirection)
          torus@(Torus position sRadius tRadius)
  |null solution = Nothing
  |otherwise = Just (Intersection hitPoint
                                  localNormal
                                  (InvalidTexture "blank")
                                  minT
                                  newCoords)
    where
      localRay@(start, direction) = (Vec.subtract origStart position, origDirection)
      ox = Vec.xx start
      oy = Vec.yy start
      oz = Vec.zz start
      dx = Vec.xx direction
      dy = Vec.yy direction
      dz = Vec.zz direction
      sumDSqrd = dx^2 + dy^2 + dz^2
      e = ox^2 + oy^2 + oz^2 - sRadius^2 - tRadius^2
      f = ox * dx + oy * dy + oz * dz
      fourASqrd = 4 * sRadius^2
      coeffs = (e^2 - fourASqrd * (tRadius^2 - oy^2),
                4 * f * e + 2 * fourASqrd * oy * dy,
                2 * sumDSqrd * e + 4 * f^2 + fourASqrd * dy^2,
                4 * sumDSqrd * f,
                sumDSqrd^2)
      solution = filter (>kEps) (solve4 coeffs)
      minT = minimum solution               
      localHitPoint = scaleTo minT localRay
      localNormal = computeNormalAtPoint torus localHitPoint
      hitPoint = scaleTo minT ray
      newCoords = (0, 0)

computeNormalAtPoint :: Geometry -> Vec.Vector -> Vec.Vector
computeNormalAtPoint (Torus position sRadius tRadius) (xx, yy, zz) = Vec.normalize result
  where
    paramSquared = sRadius^2 + tRadius^2
    sumSquared = xx^2 + yy^2 + zz^2
    result = (4 * xx * (sumSquared - paramSquared),
              4 * yy * (sumSquared - paramSquared + 2 * sRadius^2),
              4 * zz * (sumSquared - paramSquared))
