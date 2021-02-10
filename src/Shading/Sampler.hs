module Shading.Sampler where

import Base

newtype Sampler a = Sampler (Texel -> a)

sample :: Sampler a -> Texel -> a
sample (Sampler f) = f

constantSampler :: a -> Sampler a
constantSampler = Sampler . const

checkerSampler :: a -> a -> Float -> Sampler a
checkerSampler primary secondary size = Sampler $ \(u, v) -> let
  x = mod (floor $ u / size :: Int) 2
  y = mod (floor $ v / size :: Int) 2
  in
    if x == y 
      then primary
      else secondary
