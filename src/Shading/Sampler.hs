module Shading.Sampler where

import Base
import Shading.Color

data Sampler = ConstantColorSampler {
  color :: Rgb
} deriving (Show, Eq)

sample :: Sampler -> Texel -> Rgb
sample (ConstantColorSampler color) _ = color