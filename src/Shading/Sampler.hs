module Shading.Sampler where

import Base
import Shading.Color

data Sampler = 
  ConstantColorSampler {
    color :: Rgb
  } |
  CheckerSampler {
    color1 :: Rgb,
    color2 :: Rgb,
    size :: Float 
  } deriving (Show, Eq)

sample :: Sampler -> Texel -> Rgb
sample (ConstantColorSampler color) _ = color
sample (CheckerSampler c1 c2 size) (u, v) = let
  x = mod (floor $ u / size :: Int) 2
  y = mod (floor $ v / size :: Int) 2
  in
    if x == y 
      then c1
      else c2
