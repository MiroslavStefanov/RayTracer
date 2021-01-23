{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Tracing.Tracer_Test where

import ErrorHandling.ErrorMessages
import Tracing.Tracer
import Tracing.TracingPass
import Tracing.Scene
import Ray
import Intersection
import PinholeCamera
import qualified Vector as Vec
import Shading.Texture
import Shading.FrameBuffer
import Shading.Color
import Shading.ShadingContext
import qualified LightSource as LS

import Test.Hspec
import Linear.Epsilon

testIdentityTracer :: IO()
testIdentityTracer = hspec $ do
  describe "IdentityTracer with empty tracing pass" $ do
    it "Should return tracing result of 1" $ do
      trace (identityTracer 1) emptyTracingPass `shouldBe` Right (emptyTracingPass, 1)
            
testAbortTracer :: IO()
testAbortTracer = hspec $ do
  describe "AbortTacer" $ do
    it "Should return error" $ do
      trace testTracer emptyTracingPass `shouldBe` Left (GeneralError errorMessage)
      where 
        errorMessage = "Test error message"
        testTracer = abortTracer errorMessage :: Tracer ()

testShootRayTracer :: IO()
testShootRayTracer = hspec $ do
  describe "ShootRayTracer should add ray to a tracing pass" $ do 
    let 
      shootedRay = ((0,0,0), (1,0,0)) 
      expectedPass = TracingPass [] [shootedRay]
      in
        it "Shoot ray in an empty pass" $ do
          trace (shootRayTracer shootedRay) emptyTracingPass `shouldBe` Right (expectedPass, ())
    
testGetIntersectionTracer :: IO()
testGetIntersectionTracer = hspec $ do
  describe "GetIntersectionTracer should consume one intersection from a non-empty tracing pass" $ do
    it "Should get intersection" $ do
      let
        intersection = Intersection (0,0,0) (1,0,0) (InvalidTexture "No texture") 0 (0,0)
        initialPass = TracingPass [Just intersection] []
        expectedPass = emptyTracingPass
        in
          trace getIntersectionTracer initialPass `shouldBe` Right (expectedPass, Just intersection)
    it "Should get nothing" $ do
      let
        initialPass = TracingPass [Nothing] []
        expectedPass = emptyTracingPass
        in
          trace getIntersectionTracer initialPass `shouldBe` Right (expectedPass, Nothing)
    it "Should return error due to empty pass" $ do
      trace getIntersectionTracer emptyTracingPass `shouldBe` Left (GeneralError noIntersectionsErrorMessage)

testIndexTexelsTracer :: IO()
testIndexTexelsTracer = hspec $ do
  describe "IndexTexelsTracer should produce FrameBuffer that contains texel coordinates of each texel" $ do 
    let 
      bufferWidth = 3
      bufferHeight = 3
      expectedBuffer = FrameBuffer bufferWidth bufferHeight [
        (0, 0),   (0.5, 0),   (1, 0),
        (0, 0.5), (0.5, 0.5), (1, 0.5),
        (0, 1),   (0.5, 1),   (1, 1)
        ]
      in
        it "Create indexed 3x3 buffer" $ do
          trace (indexTexelsTracer bufferWidth bufferHeight) emptyTracingPass `shouldBe` Right (emptyTracingPass, expectedBuffer)

testShootCameraRayTracer :: IO()
testShootCameraRayTracer = hspec $ do
  describe "ShootCameraRayTracer should add ray projected from a camera to a texel to a pass and return the same ray" $ do
    let
      cameraPosition = (0,0,0)
      cameraDirection = (0,0,-1)
      camera = prepareCamera cameraPosition cameraDirection (0,1,0) (pi / 2.5) 1
      texel = (0.5, 0.5)
      expectedRay = (cameraPosition, cameraDirection)
      expectedPass = TracingPass [] [expectedRay]
      in
        it "Shoot ray in the center of the screen" $ do
          trace (shootCameraRayTracer camera texel) emptyTracingPass `shouldBe` Right (expectedPass, expectedRay)

testShootRayTowardsLightTracer :: IO()
testShootRayTowardsLightTracer = hspec $ do
  describe "ShootRayTowardsLightTracer should add ray from intersection point towards a light source" $ do
    let
      intersectionPosition = (0,0,0)
      intersection = Intersection intersectionPosition (1,0,0) (InvalidTexture "") 0 (0,0)
      lightPosition = (1,1,1)
      lightSource = LS.PointLight 0 (Rgb 0 0 0) lightPosition
      expectedRay = (intersectionPosition, Vec.normalize $ Vec.subtract lightPosition intersectionPosition)
      expectedPass = TracingPass [] [expectedRay]
      in
        it "Shoot ray in the center of the screen" $ do
          trace (shootRayTowardsLightTracer intersection lightSource) emptyTracingPass `shouldBe` Right (expectedPass, ())

testCameraRayIntersectionTracer :: IO()
testCameraRayIntersectionTracer = hspec $ do
  let
      lightPosition = (1,1,1)
      lightSource = LS.PointLight 0 (Rgb 0 0 0) lightPosition
      lightPosition2 = (0,5,2)
      lightSource2 = LS.PointLight 0 (Rgb 0 0 0) lightPosition2
      scene = Scene [] [lightSource, lightSource2]
      dummyRay = ((0,0,0), (1,0,0))
      in
        describe "CameraRayIntersectionTracer should add rays from intersection point towards all light sources in a scene or do nothing if no intersection happened" $ do
          it "Empty tracing pass" $ do
            trace (cameraRayIntersectionTracer dummyRay scene) (TracingPass [] []) `shouldBe` Left (GeneralError noIntersectionsErrorMessage)
          it "Intersection is not present" $ do
            trace (cameraRayIntersectionTracer dummyRay scene) (TracingPass [Nothing] []) `shouldBe` Right (emptyTracingPass, ShadingContext dummyRay Nothing)
          it "Intersection is present" $ do
            let
              intersectionPosition = (0,0,0)
              intersection = Intersection intersectionPosition (1,0,0) (InvalidTexture "") 0 (0,0)
              pass = TracingPass [Just intersection] []
              getRayDirection = Vec.subtract intersectionPosition
              expectedRays = map ((intersectionPosition, ) . Vec.invert . Vec.normalize . getRayDirection) [lightPosition2, lightPosition]
              expectedPass = TracingPass [] expectedRays
              snapToZero = \f -> if nearZero f then 0 else f
              normalizeZeroes = \((x,y,z), (nx, ny, nz)) -> ((snapToZero x, snapToZero y, snapToZero z), (snapToZero nx, snapToZero ny, snapToZero nz))
              snapResultZeroes = \(TracingPass i r, res) -> (TracingPass i (map normalizeZeroes r), res)
              in
                snapResultZeroes <$> trace (cameraRayIntersectionTracer dummyRay scene) pass `shouldBe` Right (expectedPass, ShadingContext dummyRay $ Just intersection)

testTransformBufferTracer :: IO()
testTransformBufferTracer = hspec  $ do
  describe "TransformBufferTracer should get a FrameBuffer, apply a tracer over all texels and produce FrameBuffer of the same size with results of the tracer" $ do
    it "Apply identity tracer to all texels of a buffer" $ do
      let buffer = FrameBuffer 2 2 [1, 2, 3, 4] in
        trace (transformBufferTracer identityTracer buffer) emptyTracingPass `shouldBe` Right (emptyTracingPass, buffer)
          
