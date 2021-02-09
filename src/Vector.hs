module Vector where

import Prelude hiding(length, subtract)

type Vector = (Float, Float, Float)

xx :: Vector -> Float
xx (x, _, _) = x

yy :: Vector -> Float
yy (_, y, _) = y

zz :: Vector -> Float
zz (_, _, z) = z

zeroVector :: Vector
zeroVector = (0, 0, 0)

isZero :: Vector -> Bool
isZero v = v == zeroVector

lengthSqr :: Vector -> Float
lengthSqr (x, y, z) = x*x + y*y + z*z

length :: Vector -> Float
length = sqrt . lengthSqr

scale :: Float -> Vector -> Vector
scale f (x, y, z) = (x * f, y * f, z * f)

normalize :: Vector -> Vector
normalize v@(x, y, z) =
  let len = length v
  in (x / len, y / len, z / len)

scaleTo :: Float -> Vector -> Vector
scaleTo f = scale f. normalize

add :: Vector -> Vector -> Vector
add (x, y, z) (a, b, c) = (x + a, y + b, z + c)

subtract :: Vector -> Vector -> Vector
subtract (x, y, z) (a, b, c) = (x - a, y - b, z - c)

invert :: Vector -> Vector
invert (x, y, z) = (-x, -y, -z)

cross :: Vector -> Vector -> Vector
cross (x, y, z) (xx, yy, zz) = 
  let a = y * zz - z * yy
      b = z * xx - x * zz
      c = x * yy - y * xx
  in (a, b, c)

dot :: Vector -> Vector -> Float
dot (x, y, z) (xx, yy, zz) = x * xx + y * yy + z * zz

reflect :: Vector -> Vector -> Vector
reflect v normal =
  subtract v $ scale (2 * dot normal v) normal

maxDot :: Vector -> Vector -> Vector -> Vector
maxDot a b c
  |maxDot == aDot = a
  |maxDot == bDot = b
  |maxDot == cDot = c
    where dots@[aDot, bDot, cDot] = map (\x -> dot x x) [a, b, c]
          maxDot = maximum dots

zeroDotNormalized :: Vector -> Vector -> Vector -> Vector -> Vector
zeroDotNormalized aa bb cc point
  |abs (aa `dot` point) < eps = normalize aa
  |abs (bb `dot` point) < eps = normalize bb
  |abs (cc `dot` point) < eps = normalize cc
    where eps = 10 ** (-5)

refract :: Vector -> Vector -> Float -> Float -> Maybe Vector
refract normal incident n1 n2
  |sinT2 > 1 = Nothing
  |otherwise = Just transmittedVector
    where n = n1 / n2
          cosI = negate $ normal `dot` incident
          sinT2 = n * n * (1 - cosI * cosI)
          cosT = sqrt $ 1 - sinT2
          transmittedVector = n `scale` incident `add` ((n * cosI - cosT) `scale` normal)

rSchlick2 :: Vector -> Vector -> Float -> Float -> Float
rSchlick2 normal incident n1 n2
  |n1 <= n2 = ratio1
  |sinT2 > 1 = 1
  |otherwise = ratio2
    where r0Root = (n1 - n2) / (n1 + n2)
          r0 = r0Root ^ 2
          cosI = negate $ normal `dot` incident
          x1 = 1 - cosI
          ratio1 = r0 + (1 - r0) * x1 ^ 5
          n = n1 / n2
          sinT2 = n * n * (1 - cosI * cosI)
          cosX = sqrt $ 1 - sinT2
          x2 = 1 - cosX
          ratio2 = r0 + (1 - r0) * x2 ^ 5