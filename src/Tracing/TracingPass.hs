module Tracing.TracingPass where

import Intersection ( Intersection )
import Ray ( Ray )
import Data.Maybe

data TracingPass = TracingPass {
  inputIntersections :: [Maybe Intersection],
  outputRays :: [Ray]
} deriving (Show, Eq)

emptyTracingPass :: TracingPass
emptyTracingPass = TracingPass [] []

addRay :: TracingPass -> Ray -> TracingPass
addRay (TracingPass i rays) ray = TracingPass i (ray : rays)