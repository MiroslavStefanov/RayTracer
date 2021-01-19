module Vector where

import Prelude hiding(length)

type Vector = (Float, Float, Float)

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