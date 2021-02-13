module PinholeCamera_Test where

import Base
import PinholeCamera
import Tracing.Tracer
import Tracing.TracingPass

import Test.Hspec

testPrepareCamera :: IO()
testPrepareCamera = hspec $ do
  describe "Creating pinhole camera" $ do
    it "Position (0,0,0), target (0,0,-1), up (0,1,0), fov pi/2.5, aspect ratio 1" $ do
      prepareCamera pos tar u fieldOfView aspRatio `shouldBe` expectedCamera where
        pos = (0,0,0)
        tar = (0,0,-1)
        u = (0,1,0)
        fieldOfView = pi/2.5
        aspRatio = 1
        expectedCamera = PinholeCamera 
          (-0.7265426,0.7265426,-1.0)
          (-0.7265426,-0.7265426,-1.0)
          (0.7265426,0.7265426,-1.0)
          pos
          tar
          u
          fieldOfView
          aspRatio

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

testPinholeCameraModule :: IO()
testPinholeCameraModule = hspec $ do
  describe "PinholeCamera module" $ do
    it "" testPrepareCamera
    it "" testShootCameraRayTracer 