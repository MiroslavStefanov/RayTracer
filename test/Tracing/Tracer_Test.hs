module Tracing.Tracer_Test where

import Base
import ErrorHandling.ErrorMessages
import Tracing.Tracer
import Tracing.TracingPass
import Intersection

import Test.Hspec

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
        initialPass = TracingPass [Just emptyIntersection] []
        expectedPass = emptyTracingPass
        in
          trace getIntersectionTracer initialPass `shouldBe` Right (expectedPass, Just emptyIntersection)
    it "Should get nothing" $ do
      let
        initialPass = TracingPass [Nothing] []
        expectedPass = emptyTracingPass
        in
          trace getIntersectionTracer initialPass `shouldBe` Right (expectedPass, Nothing)
    it "Should return error due to empty pass" $ do
      trace getIntersectionTracer emptyTracingPass `shouldBe` Left (GeneralError noIntersectionsErrorMessage)

testAnyIntersectionsTracer :: IO()
testAnyIntersectionsTracer = hspec $ do
  describe "AnyIntersectionsTracer should return whether there are any intersections without modifying the pass" $ do
    it "Should return false for empty pass" $ do
      trace anyIntersectionsTracer emptyTracingPass `shouldBe` Right (emptyTracingPass, False)
    it "Should return true for non-empty pass" $ do
      trace anyIntersectionsTracer pass `shouldBe` Right (pass, True) where
        pass = TracingPass [Nothing] []

testTracerModule :: IO()
testTracerModule = hspec $ do
  describe "Tracer module" $ do
    it "" testIdentityTracer
    it "" testAbortTracer
    it "" testShootRayTracer
    it "" testGetIntersectionTracer
    it "" testAnyIntersectionsTracer

-- testShootCameraRayTracer :: IO()
-- testShootCameraRayTracer = hspec $ do
--   describe "ShootCameraRayTracer should add ray projected from a camera to a texel to a pass and return the same ray" $ do
--     let
--       cameraPosition = (0,0,0)
--       cameraDirection = (0,0,-1)
--       camera = prepareCamera cameraPosition cameraDirection (0,1,0) (pi / 2.5) 1
--       texel = (0.5, 0.5)
--       expectedRay = (cameraPosition, cameraDirection)
--       expectedPass = TracingPass [] [expectedRay]
--       in
--         it "Shoot ray in the center of the screen" $ do
--           trace (shootCameraRayTracer camera texel) emptyTracingPass `shouldBe` Right (expectedPass, expectedRay)

-- testShootRayTowardsLightTracer :: IO()
-- testShootRayTowardsLightTracer = hspec $ do
--   describe "ShootRayTowardsLightTracer should add ray from intersection point towards a light source" $ do
--     let
--       intersectionPosition = (0,0,0)
--       intersection = Intersection intersectionPosition (1,0,0) (InvalidTexture "") 0 (0,0)
--       lightPosition = (1,1,1)
--       lightSource = LS.PointLight 0 (Rgb 0 0 0) lightPosition
--       expectedRay = (intersectionPosition, Vec.normalize $ Vec.subtract lightPosition intersectionPosition)
--       expectedPass = TracingPass [] [expectedRay]
--       in
--         it "Shoot ray in the center of the screen" $ do
--           trace (shootRayTowardsLightTracer intersection lightSource) emptyTracingPass `shouldBe` Right (expectedPass, ())

-- testCameraRayIntersectionTracer :: IO()
-- testCameraRayIntersectionTracer = hspec $ do
--   let
--       lightPosition = (1,1,1)
--       lightSource = LS.PointLight 0 (Rgb 0 0 0) lightPosition
--       lightPosition2 = (0,5,2)
--       lightSource2 = LS.PointLight 0 (Rgb 0 0 0) lightPosition2
--       scene = Scene [] [lightSource, lightSource2]
--       dummyRay = ((0,0,0), (1,0,0))
--       in
--         describe "CameraRayIntersectionTracer should add rays from intersection point towards all light sources in a scene or do nothing if no intersection happened" $ do
--           it "Empty tracing pass" $ do
--             trace (cameraRayIntersectionTracer scene dummyRay) (TracingPass [] []) `shouldBe` Left (GeneralError noIntersectionsErrorMessage)
--           it "Intersection is not present" $ do
--             trace (cameraRayIntersectionTracer scene dummyRay) (TracingPass [Nothing] []) `shouldBe` Right (emptyTracingPass, Left white)
--           it "Intersection is present" $ do
--             let
--               intersectionPosition = (0,0,0)
--               intersection = Intersection intersectionPosition (1,0,0) (InvalidTexture "") 0 (0,0)
--               pass = TracingPass [Just intersection] []
--               getRayDirection = Vec.subtract intersectionPosition
--               expectedRays = map ((intersectionPosition, ) . Vec.invert . Vec.normalize . getRayDirection) [lightPosition2, lightPosition]
--               expectedPass = TracingPass [] expectedRays
--               snapToZero = \f -> if nearZero f then 0 else f
--               normalizeZeroes = \((x,y,z), (nx, ny, nz)) -> ((snapToZero x, snapToZero y, snapToZero z), (snapToZero nx, snapToZero ny, snapToZero nz))
--               snapResultZeroes = \(TracingPass i r, res) -> (TracingPass i (map normalizeZeroes r), res)
--               in
--                 snapResultZeroes <$> trace (cameraRayIntersectionTracer scene dummyRay) pass `shouldBe` Right (expectedPass, Right $ ShadingContext dummyRay intersection)

-- testShadeTracer :: IO()
-- testShadeTracer = hspec $ do
--     describe "ShadeTracer should produce shading value based on current shading value and input intersections" $ do
--       it "Shade already computed value" $ do
--         let colorValue = Left $ Rgb 1 1 1
--           in trace (shadeTracer (Scene [] []) colorValue) emptyTracingPass `shouldBe` Right (emptyTracingPass, colorValue)
--       it "Shade intersection with invalid texture" $ do
--         let 
--           textureError = "test invalid texture"
--           invalidTextureIntersection = Intersection (0,0,0) (1,0,0) (InvalidTexture textureError) 0 (0,0)
--           inputShadingValue = Right $ ShadingContext ((0,0,0), (1,0,0)) invalidTextureIntersection
--           in
--             trace (shadeTracer (Scene [] []) inputShadingValue) emptyTracingPass `shouldBe` Left (GeneralError textureError)
--       it "Shade intersection with color texture" $ do
--         let
--           textureColor = Rgb 1 1 1
--           intersection = Intersection (0,0,0) (1,0,0) (ColorTexture textureColor) 0 (0,0)
--           inputShadingValue = Right $ ShadingContext ((0,0,0), (1,0,0)) intersection
--           in
--             trace (shadeTracer (Scene [] []) inputShadingValue) emptyTracingPass `shouldBe` Right (emptyTracingPass, Left textureColor)
--       it "Shade intersection with phong texture" $ do
--         let
--           diffuseColor = Rgb 1 1 1
--           sampler = ConstantColorSampler diffuseColor
--           phongTexture = PhongTexture sampler 1 1
--           inputValue = Right $ ShadingContext ((0,0,0), (0,1,0)) $ Intersection (0,0,0) (1,0,0) phongTexture 0 (0.5,0.5)
--           light1 = LS.PointLight 1 (Rgb 1 0 0) (1,1,1)
--           light2 = LS.PointLight 1 (Rgb 0 1 0) (2,1,2) --distance to intersection is 3
--           intersectionTowardsLight1 = Nothing
--           intersectionTowardsLight2 = Just $ Intersection (0,0,0) (1,0,0) (InvalidTexture "whatever") 2 (0,0)
--           inputPass = TracingPass [intersectionTowardsLight1, intersectionTowardsLight2] []
--           c = sqrt 3.0
--           r = (60*c + 1)/270
--           g = (10*c + 1)/90
--           b = (30*c + 1)/270
--           round3 = \f -> (fromIntegral (truncate $ f * 1000))/1000
--           expectedColor = clamp $ Rgb (round3 r) (round3 g) (round3 b)
--           actualResult = trace (shadeTracer (Scene [] [light1, light2]) inputValue) inputPass
--           truncateColor = \(Rgb r g b) -> (Rgb (round3 r) (round3 g) (round3 b))
--           truncatedResult = (\case (pass, Left col) -> (pass, Left $ truncateColor col)) <$> actualResult
--           in
--             truncatedResult `shouldBe` (Right (emptyTracingPass, Left expectedColor) :: TraceResult ShadingValue)

-- testGenerateIntersectionsTracer :: IO()
-- testGenerateIntersectionsTracer = hspec $ do
--   describe "GenerateIntersectionsTracer should convert the rays in a tracing pass to intersections for the next pass" $ do
--     let 
--       sphere = Sphere (0,0,0) 3
--       sphereTexture = InvalidTexture ""
--       meshSpere = Mesh sphere sphereTexture
--       scene = Scene [meshSpere] []
--     it "With empty pass it should produce empty pass" $ do
--       trace (generateIntersectionsTracer scene) emptyTracingPass `shouldBe` Right (emptyTracingPass, ())
--     it "With 2 rays it should produce one intersection and one non-intersection" $ do
--       let
--         rayAtSphere = ((-5, 0, 0), (1, 0, 0))
--         rayNotAtSphere = ((-5, 0, 0), (0, 1, 0))
--         inputPass = TracingPass [] [rayAtSphere, rayNotAtSphere]
--         expectedIntersection = Intersection (-3, 0, 0) (-1, 0, 0) sphereTexture 2 (1, 0)
--         expectedPass = TracingPass [Just expectedIntersection, Nothing] []
--         in
--           trace (generateIntersectionsTracer scene) inputPass `shouldBe` Right (expectedPass, ())
          
