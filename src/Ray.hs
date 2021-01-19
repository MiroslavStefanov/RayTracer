module Ray where

import qualified Vector as Vec

type RecursionDepth = Integer
type Ray = (Vec.Vector, Vec.Vector, RecursionDepth)

scaleTo :: Float -> Ray -> Vec.Vector
scaleTo factor (start, direction, _) =
  Vec.add start $ Vec.scale factor direction

translate :: Float -> Ray -> Ray
translate factor ray@(start, direction, recDepth) =
  let newStart = scaleTo factor ray
  in (newStart, direction, recDepth)    