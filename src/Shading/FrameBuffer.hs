module Shading.FrameBuffer where

import Base
import Tracing.Tracer

data FrameBuffer a = FrameBuffer{
  width :: Int,
  height :: Int,
  buffer :: [a]
} deriving (Show, Eq)

createEmptyBuffer :: FrameBuffer a
createEmptyBuffer = FrameBuffer 0 0 []

createBuffer :: Int -> Int -> FrameBuffer Texel
createBuffer 0 _ = createEmptyBuffer
createBuffer _ 0 = createEmptyBuffer
createBuffer width height = FrameBuffer width height newBuffer where
  wStep = 1.0 / fromIntegral (width - 1) :: Float
  hStep = 1.0 / fromIntegral (height - 1) :: Float
  newBuffer = [(u, v) | v <- [0, wStep .. 1], u <- [0, hStep .. 1]]

transformBufferTracer :: (a -> Tracer b) -> FrameBuffer a -> Tracer (FrameBuffer b)
transformBufferTracer func (FrameBuffer w h buff) = let
  newBuffer = mapM func buff in
    FrameBuffer w h <$> newBuffer