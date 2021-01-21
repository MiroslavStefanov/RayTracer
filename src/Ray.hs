module Ray where

import qualified Vector as Vec

type Start = Vec.Vector
type Direction = Vec.Vector
type Ray = (Start, Direction)

scaleTo :: Float -> Ray -> Vec.Vector
scaleTo factor (start, direction) =
  Vec.add start $ Vec.scale factor direction

translate :: Float -> Ray -> Ray
translate factor ray@(start, direction) =
  let newStart = scaleTo factor ray
  in (newStart, direction)