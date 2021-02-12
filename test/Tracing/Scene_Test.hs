module Tracing.Scene_Test where

import Base
import Tracing.Scene
import Tracing.Tracer
import Tracing.TracingPass
import Tracing.Mesh
import Geometry ( Geometry(Sphere) )
import Intersection
import Shading.Texture ( emptyTexture )

import Test.Hspec

testGenerateIntersectionsTracer :: IO()
testGenerateIntersectionsTracer = hspec $ do
  describe "GenerateIntersectionsTracer should convert the rays in a tracing pass to intersections for the next pass" $ do
    let 
      sphere = Sphere (0,0,0) 3
      sphereTexture = emptyTexture
      meshSpere = Mesh sphere sphereTexture
      scene = Scene [meshSpere] []
    it "With empty pass it should produce empty pass" $ do
      trace (generateIntersectionsTracer scene) emptyTracingPass `shouldBe` Right (emptyTracingPass, (0,0))
    it "With 2 rays it should produce one intersection and one non-intersection" $ do
      let
        rayAtSphere = ((-5, 0, 0), (1, 0, 0))
        rayNotAtSphere = ((-5, 0, 0), (0, 1, 0))
        inputPass = TracingPass [] [rayAtSphere, rayNotAtSphere]
        expectedIntersection = Intersection (-3, 0, 0) (-1, 0, 0) sphereTexture 2 (1, 0)
        expectedPass = TracingPass [Nothing, Just expectedIntersection] []
        in
          trace (generateIntersectionsTracer scene) inputPass `shouldBe` Right (expectedPass, (0,2))

testSceneModule :: IO()
testSceneModule = hspec $ do
  describe "Scene module" $ do
    it "" testGenerateIntersectionsTracer