module Tracing.Scene where

import Tracing.TracingPass
import Ray
import Intersection

data Scene = Scene {
  objects :: [Mesh]
}


traceRay :: Ray -> Secen -> Maybe Intersection
traceRay ray scene = undefined


calculateNextTracingPass :: TracingPass -> Scene -> TracingPass
calculateNextTracingPass (TracingPass _ rays) = TracingPass (map traceRay rays) []