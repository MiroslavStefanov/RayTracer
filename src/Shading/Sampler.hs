module Shading.Sampler where

import Base
import Shading.Color

data Sampler = 
  ConstantColorSampler {
    color :: Rgb
  } |
  CheckerSampler {
    size :: Float 
  } deriving (Show, Eq)

sample :: Sampler -> Texel -> Rgb
sample (ConstantColorSampler color) _ = color
sample (CheckerSampler size) (u, v) = let
  x = mod (floor $ u / size :: Int) 2
  y = mod (floor $ v / size :: Int) 2
  in
    if x == y 
      then white
      else Rgb 0 0 0
