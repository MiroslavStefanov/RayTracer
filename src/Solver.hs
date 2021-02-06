module Solver where

type Coefficients3 = (Float, Float, Float)
type Coefficients4 = (Float, Float, Float, Float)
type Coefficients5 = (Float, Float, Float, Float, Float)

eps :: Float
eps = 10 ** (-9)

kEps :: Float
kEps = 10 ** (-4)

isZero :: Float -> Bool
isZero x = x > (-eps) && x < eps

solve2 :: Coefficients3 -> [Float]
solve2 (c1, c2, c3)
  |isZero discr = [-p]
  |discr < 0 = []
  |otherwise = [sqrtD - p, -sqrtD - p]
    where
      p = c2 / (2 * c3)
      q = c1 / c3
      discr = p^2 - q
      sqrtD = sqrt discr

solve3Calc :: Coefficients4 -> [Float]
solve3Calc (c1, c2, c3, c4)
  |isZero dd =
    if isZero q
    then [0]
    else [2 * u, -u]
  |dd < 0 = [t * cos phi,
            (-t) * cos (phi + pi / 3),
            (-t) * cos (phi - pi / 3)]

  |otherwise = [uu + vv]
    where
      aa = c3 / c4
      bb = c2 / c4
      cc = c1 / c4
      sqrA = aa^2
      p = 1 / 3 * ((-1) / 3 * sqrA + bb)
      q = 1 / 2 * (2 / 27 * aa * sqrA - 1 / 3 * aa * bb + cc)
      dd = q^2 - p^3
      u = (-q) ** (1/3)
      phi = 1 / 3 * acos ((-q) / sqrt (-p^3))
      t = 2 * sqrt (-p)
      sqrtD = sqrt dd
      uu = (sqrtD - q) ** (1/3)
      vv = -((sqrtD + q) ** (1/3))

solve3 :: Coefficients4 -> [Float]
solve3 coeffs@(_, _, c3, c4) = map (+ (-sub)) $ solve3Calc coeffs
  where
    aa = c3 / c4
    sub = 1 / 3 * aa

{-|
  Solves equation:
    c1 + c2*x + c3*x^2 + c4*x^3 + c5*x^4 = 0
-}
solve4Calc :: Coefficients5 -> [Float]
solve4Calc (c1 , c2, c3, c4, c5)
  |isZero r = solve3 (q, p, 0, 1) ++ [0]
  |not (isZero u) && u < 0 = []
  |not (isZero v) && v < 0 = []
  |otherwise = solve2 coeffs1 ++ solve2 coeffs2
    where
      aa = c4 / c5
      bb = c3 / c5
      cc = c2 / c5
      dd = c1 / c5
      sqrA = aa^2
      p = (-3) / 8 * sqrA + bb
      q = 1 / 8 * sqrA * aa - 1 / 2 * aa * bb + cc
      r = (-3) / 256 * aa^4 + 1 / 16 * sqrA * bb - 1 / 4 * aa * cc + dd
      coeffs = (1 / 2 * r * p - 1 / 8 * q^2, -r, (-1) / 2 * p, 1)
      ss = solve3 coeffs
      zz = head ss
      u = zz^2 - r
      v = 2 * zz - p
      uu = if isZero u then 0 else sqrt u
      vv = if isZero v then 0 else sqrt v
      vv1 = if q < 0 then (-vv) else vv
      vv2 = if q < 0 then vv else (-vv)
      coeffs1 = (zz - uu, vv1, 1)
      coeffs2 = (zz + uu, vv2, 1)

solve4 :: Coefficients5 -> [Float]
solve4 coeffs@(_, _, _, c4, c5) = map (+ (-sub)) $ solve4Calc coeffs
  where
    aa = c4 / c5
    sub = 1 / 4 * aa