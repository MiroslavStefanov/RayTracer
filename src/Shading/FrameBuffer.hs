module Shading.FrameBuffer where

import Base

data FrameBuffer a = FrameBuffer{
  width :: Int,
  height :: Int,
  buffer :: [a]
} deriving (Show, Eq)

createBuffer :: Int -> Int -> FrameBuffer Texel
createBuffer 0 _ = FrameBuffer 0 0 []
createBuffer _ 0 = FrameBuffer 0 0 []
createBuffer width height = FrameBuffer width height newBuffer where
  wStep = 1.0 / fromIntegral (width - 1) :: Float
  hStep = 1.0 / fromIntegral (height - 1) :: Float
  newBuffer = [(u, v) | v <- [0, wStep .. 1], u <- [0, hStep .. 1]]