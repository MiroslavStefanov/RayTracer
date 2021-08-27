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
    height :: Float
  } deriving (Show, Read, Eq)

instance Intersectable Geometry where
  intersect ray@(start, direction)
            (Plane position normal)
    |abs denom <= epsilon = Nothing
    |scalar < 0 = Nothing
    |otherwise = Just (Intersection newPosition normal scalar newCoords)
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
    |otherwise = Just (Intersection newPosition newNormal xMin newCoords)
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
    |otherwise = Just (Intersection newPosition normal distance newCoords)
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
            normal = Vec.normalize $ Vec.cross e1 e2
            newCoords = (uu, vv)

  intersect ray@(start, direction)
            paral@(Parallelepiped position aa bb cc (aLen, bLen, cLen))
    |abs f1 <= pEps && ((-e1) - aLen > 0 || (-e1) + aLen < 0) = Nothing
    |t1Min > t1Max || t1Max < 0 = Nothing
    |abs f2 <= pEps && ((-e2) - bLen > 0 || (-e2) + bLen < 0) = Nothing
    |t2Min > t2Max || t2Max < 0 = Nothing
    |abs f3 <= pEps && ((-e3) - cLen > 0 || (-e3) + cLen < 0) = Nothing
    |t3Min > t3Max || t3Max < 0 = Nothing
    |otherwise = Just (Intersection hitPoint localNormal t newCoords)
      where
        tmin = -99999.9
        tmax = 99999.9
        vcP = Vec.subtract position start
        --1st slab
        e1 = Vec.dot aa vcP
        f1 = Vec.dot aa direction
        t11 = (e1 + aLen) / f1
        t12 = (e1 - aLen) / f1
        t1Min = max tmin $ min t11 t12
        t1Max = min tmax $ max t11 t12
        --2nd slab
        e2 = Vec.dot bb vcP
        f2 = Vec.dot bb direction
        t21 = (e2 + bLen) / f2
        t22 = (e2 - bLen) / f2
        t2Min = max t1Min $ min t21 t22
        t2Max = min t1Max $ max t21 t22
        --3rd slab
        e3 = Vec.dot cc vcP
        f3 = Vec.dot cc direction
        t31 = (e3 + cLen) / f3
        t32 = (e3 - cLen) / f3
        t3Min = max t2Min $ min t31 t32
        t3Max = min t2Max $ max t31 t32
        t = if t3Min > 0 then t3Min else t3Max
        localNormal = computeNormalAtPoint paral hitPoint
        hitPoint = scaleTo t ray
        newCoords = (0, 0)

  intersect ray@(origStart, origDirection)
            cone@(Cone position radius height)
    |null solution = Nothing
    |t1 <= 0 = Nothing
    |r < Vec.zz position || r > Vec.zz position + height = Nothing
    |otherwise = Just (Intersection hitPoint localNormal tShapeHit newCoords)
      where
        localRay@(start, direction) = (Vec.subtract origStart position, origDirection)
        ox = Vec.xx start
        oy = Vec.yy start
        oz = Vec.zz start
        dx = Vec.xx direction
        dy = Vec.yy direction
        dz = Vec.zz direction
        k = (radius / height)^2
        aa = dx^2 + dy^2 - k * dz^2
        bb = 2 * (dx * ox + dy * oy - k * dz * (oz - height))
        cc = ox^2 + oy^2 - k * (oz - height)^2
        solution = solve2 (cc, bb, aa)
        t0 = minimum solution
        t1 = maximum solution
        tShapeHit = if t0 <= 0 then t1 else t0
        localHitPoint = scaleTo tShapeHit localRay
        localNormal = computeNormalAtPoint cone localHitPoint
        hitPoint = scaleTo tShapeHit ray
        r = Vec.zz hitPoint
        atanHit = atan2 (Vec.xx hitPoint) (Vec.yy hitPoint)
        phi = if atanHit < 0 then atanHit + 2 * pi else atanHit
        u = phi
        v = r / height
        newCoords = (u, v)

  intersect ray@(origStart, origDirection)
            torus@(Torus position sRadius tRadius)
    |null solution = Nothing
    |otherwise = Just (Intersection hitPoint localNormal minT newCoords)
      where
        localRay@(start, direction) = (Vec.subtract origStart position, origDirection)
        oz = Vec.zz start
        dz = Vec.zz direction
        sumDSqrd = Vec.lengthSqr direction
        e = Vec.lengthSqr start - sRadius^2 - tRadius^2
        f = Vec.dot start direction
        fourASqrd = 4.0 * sRadius^2
        coeffs = (e^2 - fourASqrd * (tRadius^2 - oz^2),
                  4 * f * e + 2 * fourASqrd * oz * dz,
                  2 * sumDSqrd * e + 4 * f^2 + fourASqrd * dz^2,
                  4 * sumDSqrd * f,
                  sumDSqrd^2)
        --solution = filter (>kEps) (solve4 coeffs)
        solution = solve4 coeffs
        minT = minimum solution
        localHitPoint = scaleTo minT localRay
        localNormal = computeNormalAtPoint torus localHitPoint
        hitPoint = scaleTo minT ray
        newCoords = (0, 0)

computeNormalAtPoint :: Geometry -> Vec.Vector -> Vec.Vector
computeNormalAtPoint (Torus position sRadius tRadius) point@(xx, yy, zz) = Vec.normalize result
  where
    paramSquared = sRadius^2 + tRadius^2
    sumSquared = Vec.lengthSqr point
    result = (4 * xx * (sumSquared - paramSquared),
              4 * yy * (sumSquared - paramSquared),
              4 * zz * (sumSquared - paramSquared + 2 * sRadius^2))
computeNormalAtPoint (Cone position radius height) (xx, yy, zz) = Vec.normalize (newX, newY, newZ)
  where
    newX = xx - Vec.xx position
    newY = yy - Vec.yy position
    r = sqrt $ newX^2 + newY^2
    newZ = if r == 0 then radius / height else r * (radius / height)
computeNormalAtPoint (Parallelepiped position aa bb cc (aLen, bLen, cLen)) point
  |s1Dot0 = Vec.normalize $ s1 `Vec.subtract` position
  |s2Dot0 = Vec.normalize $ s2 `Vec.subtract` position
  |s3Dot0 = Vec.normalize $ s3 `Vec.subtract` position
  |s4Dot0 = Vec.normalize $ s4 `Vec.subtract` position
  |s5Dot0 = Vec.normalize $ s5 `Vec.subtract` position
  |s6Dot0 = Vec.normalize $ s6 `Vec.subtract` position
    where
      s1 = Vec.add position $ Vec.scale aLen aa
      s2 = Vec.subtract position $ Vec.scale aLen aa
      s3 = Vec.add position $ Vec.scale bLen bb
      s4 = Vec.subtract position $ Vec.scale bLen bb
      s5 = Vec.add position $ Vec.scale cLen cc
      s6 = Vec.subtract position $ Vec.scale cLen cc
      vecsOnSides = map (Vec.subtract point) [s1,s2,s3,s4,s5,s6]
      s1Dot0 = ((<eps).abs.Vec.dot (vecsOnSides !! 0)) aa
      s2Dot0 = ((<eps).abs.Vec.dot (vecsOnSides !! 1)) aa
      s3Dot0 = ((<eps).abs.Vec.dot (vecsOnSides !! 2)) bb
      s4Dot0 = ((<eps).abs.Vec.dot (vecsOnSides !! 3)) bb
      s5Dot0 = ((<eps).abs.Vec.dot (vecsOnSides !! 4)) cc
      s6Dot0 = ((<eps).abs.Vec.dot (vecsOnSides !! 5)) cc
      eps = 10 ** (-5)

makePlane :: Vec.Vector -> Vec.Vector -> Geometry
makePlane position normal = 
  Plane position $ Vec.normalize normal

computePrimaryTexDir :: Vec.Vector -> Vec.Vector
computePrimaryTexDir normal = Vec.normalize maxDotVec
  where a = Vec.cross normal (1, 0, 0)
        b = Vec.cross normal (0, 1, 0)
        c = Vec.cross normal (0, 0, 1)
        maxDotVec = Vec.maxDot a b c