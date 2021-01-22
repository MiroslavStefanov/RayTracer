module Shading.Color where

data Rgb = Rgb {
    red :: Float,
    green :: Float,
    blue :: Float
} deriving (Show)

scale :: Float -> Rgb -> Rgb
scale scalar = multiply (Rgb scalar scalar scalar)

multiply :: Rgb -> Rgb -> Rgb
multiply (Rgb r1 g1 b1) (Rgb r2 g2 b2) = Rgb (r1*r2) (g1*g2) (b1*b2)

add :: Rgb -> Rgb -> Rgb
add (Rgb r1 g1 b1) (Rgb r2 g2 b2) = Rgb (r1+r2) (g1+g2) (b1+b2)

clamp :: Rgb -> Rgb
clamp (Rgb r g b) = let
    clampBetweenZeroAndOne = \value -> min (max 0 value) 1
    cR = clampBetweenZeroAndOne r
    cG = clampBetweenZeroAndOne g
    cB = clampBetweenZeroAndOne b
    in
        Rgb cR cG cB

white :: Rgb
white = Rgb 1 1 1
