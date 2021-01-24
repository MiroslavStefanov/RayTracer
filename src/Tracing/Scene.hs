module Tracing.Scene where

import Tracing.TracingPass
import Tracing.Mesh
import Ray
import Intersection
import LightSource
import Geometry

data Scene = Scene {
  objects :: [Mesh],
  lightSources :: [LightSource]
}

addMesh :: Scene -> Mesh -> Scene
addMesh (Scene objects l) mesh = Scene (mesh : objects) l

addLightSrc :: Scene -> LightSource -> Scene
addLightSrc (Scene o lightSources) light = Scene o (light : lightSources)

getLightSourcesCount :: Scene -> Int
getLightSourcesCount = length . lightSources

traceRay :: Scene -> Ray -> Maybe Intersection
traceRay (Scene objects _) ray =
  foldl closerIntersection Nothing intersectionsWithTexture
  where geometries = map geometry objects
        textures = map Tracing.Mesh.texture objects
        intersections = map (intersect ray) geometries
        intersectionsWithTexture = zipWith addTexture intersections textures

calculateNextTracingPass :: TracingPass -> Scene -> TracingPass
calculateNextTracingPass (TracingPass _ rays) scene =
  TracingPass (map (traceRay scene) rays) []