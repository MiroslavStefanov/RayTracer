module Tracing.Scene where

import Tracing.TracingPass
import Tracing.Tracer
import Tracing.Mesh
import Ray
import Intersection
import LightSource
import Geometry

data Scene = Scene {
  objects :: [Mesh],
  lightSources :: [Light]
}

addMesh :: Scene -> Mesh -> Scene
addMesh (Scene objects l) mesh = Scene (mesh : objects) l

addLightSrc :: LightSource s => Scene -> s -> Scene
addLightSrc (Scene o lightSources) lightSrc = Scene o (getLighting lightSrc : lightSources)

getLightSourcesCount :: Scene -> Int
getLightSourcesCount = length . lightSources

traceRay :: Scene -> Ray -> Maybe Intersection
traceRay (Scene objects _) ray =
  foldl closerIntersection Nothing intersectionsWithTexture
  where textures = map (Just . Tracing.Mesh.texture) objects
        intersections = map (intersect ray) objects
        setTextures = map (fmap addTexture) intersections
        intersectionsWithTexture = zipWith (<*>) setTextures textures

calculateNextTracingPass :: TracingPass -> Scene -> TracingPass
calculateNextTracingPass (TracingPass _ rays) scene =
  TracingPass (map (traceRay scene) rays) []

generateIntersectionsTracer :: Scene -> Tracer (Int, Int)
generateIntersectionsTracer scene = Tracer $ \(TracingPass i rays) -> Right (calculateNextTracingPass (TracingPass [] $ reverse rays) scene, (Prelude.length i, Prelude.length rays))