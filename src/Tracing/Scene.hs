module Tracing.Scene where

import Tracing.Mesh
import Ray
import Intersection
import Lighting.LightSource

data SceneNode = SceneNode {
  meshes :: [Mesh],
  children :: [SceneNode]
}

data Scene = Scene {
  root :: SceneNode,
  lightSources :: [(Lighting, Occlusion)]
}

data Hit = Hit {
  mesh :: Mesh,
  intersection :: Intersection
}

emptyScene :: Scene
emptyScene = Scene (SceneNode [] []) []

closerHit :: Maybe Hit -> Maybe Hit -> Maybe Hit
closerHit Nothing Nothing = Nothing
closerHit Nothing ii = ii
closerHit ii Nothing = ii
closerHit i1@(Just (Hit _ leftIntersection))
                   i2@(Just ( Hit _ rightIntersection ))
  |distance leftIntersection < distance rightIntersection = i1
  |otherwise = i2

traceRay :: Scene -> Ray -> Maybe Hit
traceRay (Scene root lights) ray = let
  meshIntersections = map (intersect ray) $ meshes root
  makeHit = \maybeIntersection mesh -> fmap (Hit mesh) maybeIntersection
  hits = zipWith makeHit meshIntersections $ meshes root
    in foldl closerHit Nothing hits


makeMesh :: Intersectable i =>  i -> Int -> Mesh
makeMesh intersectable = Mesh (`intersect` intersectable)

addMesh :: Scene -> Mesh -> Scene
addMesh (Scene root lights) mesh = Scene newRoot lights where
  newRoot = SceneNode (mesh : meshes root) $ children root

addLight :: LightSource s => Scene -> s -> Scene
addLight (Scene root lights) lightSource = Scene root ((lighting lightSource, occlusion lightSource) : lights)
