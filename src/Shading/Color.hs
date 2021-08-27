module Shading.Color where

import Data.Word
import Control.Monad.Random

data Rgb = Rgb {
    red :: Float,
    green :: Float,
    blue :: Float
} deriving (Show, Read)

instance Eq Rgb where
  (==) (Rgb r1 g1 b1) (Rgb r2 g2 b2) = let
      isEq lhs rhs = abs (lhs - rhs) < 0.1
      in isEq r1 r2 && isEq g1 g2 && isEq b1 b2

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

red' :: Rgb
red' = Rgb 1 0 0

lime :: Rgb
lime = Rgb 0 1 0

blue' :: Rgb
blue' = Rgb 0 0 1

green' :: Rgb
green' = Rgb 0 0.5 0

yellow :: Rgb
yellow = Rgb 1 1 0

cyan :: Rgb
cyan = Rgb 0 1 1

magenta :: Rgb
magenta = Rgb 1 0 1

purple :: Rgb
purple = Rgb 0.5 0 0.5

navy :: Rgb
navy = Rgb 0 0 0.5

silver :: Rgb
silver = Rgb 0.75 0.75 0.75

skyBlue :: Rgb
skyBlue = Rgb 0.53 0.8 0.92

sampleColors :: [Rgb]
sampleColors = [red', lime]
--sampleColors = [red', lime, blue', green', skyBlue, silver, navy, purple, magenta, cyan, yellow]

getRandomColor :: (MonadRandom m) => m Rgb
getRandomColor =
    let n = length sampleColors in do
    ii <- getRandomR (0, n-1)
    return (sampleColors !! ii)    

toRgb24 :: Rgb -> (Word8, Word8, Word8)
toRgb24 (Rgb r g b) = (convert r, convert g, convert b) where
    convert = floor . (*255.0)

unpackRgb24 :: [Rgb] -> [Word8]
unpackRgb24 = foldr (unpacker . toRgb24) [] where
    unpacker = \(r, g, b) result -> r : (g : (b : result))