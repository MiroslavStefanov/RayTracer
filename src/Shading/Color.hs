module Shading.Color where

import Data.Word

data Rgb = Rgb {
    red :: Float,
    green :: Float,
    blue :: Float
} deriving (Show, Read, Eq)

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

black :: Rgb
black = Rgb 0 0 0

toRgb24 :: Rgb -> (Word8, Word8, Word8)
toRgb24 (Rgb r g b) = (convert r, convert g, convert b) where
    convert = floor . (*255.0)

unpackRgb24 :: [Rgb] -> [Word8]
unpackRgb24 = foldr (unpacker . toRgb24) [] where
    unpacker = \(r, g, b) result -> r : (g : (b : result))