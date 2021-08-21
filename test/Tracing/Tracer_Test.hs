module Tracing.Tracer_Test where

--import Base
--import ErrorHandling.ErrorMessages
--import Tracing.Tracer
--import Tracing.TracingPass
--import Intersection
--
--import Test.Hspec
--
--testIdentityTracer :: IO()
--testIdentityTracer = hspec $ do
--  describe "IdentityTracer with empty tracing pass" $ do
--    it "Should return tracing result of 1" $ do
--      trace (identityTracer 1) emptyTracingPass `shouldBe` Right (emptyTracingPass, 1)
--            
--testAbortTracer :: IO()
--testAbortTracer = hspec $ do
--  describe "AbortTacer" $ do
--    it "Should return error" $ do
--      trace testTracer emptyTracingPass `shouldBe` Left (GeneralError errorMessage)
--      where 
--        errorMessage = "Test error message"
--        testTracer = abortTracer errorMessage :: Tracer ()
--
--testShootRayTracer :: IO()
--testShootRayTracer = hspec $ do
--  describe "ShootRayTracer should add ray to a tracing pass" $ do 
--    let 
--      shootedRay = ((0,0,0), (1,0,0)) 
--      expectedPass = TracingPass [] [shootedRay]
--      in
--        it "Shoot ray in an empty pass" $ do
--          trace (shootRayTracer shootedRay) emptyTracingPass `shouldBe` Right (expectedPass, ())
--    
--testGetIntersectionTracer :: IO()
--testGetIntersectionTracer = hspec $ do
--  describe "GetIntersectionTracer should consume one intersection from a non-empty tracing pass" $ do
--    it "Should get intersection" $ do
--      let
--        initialPass = TracingPass [Just emptyIntersection] []
--        expectedPass = emptyTracingPass
--        in
--          trace getIntersectionTracer initialPass `shouldBe` Right (expectedPass, Just emptyIntersection)
--    it "Should get nothing" $ do
--      let
--        initialPass = TracingPass [Nothing] []
--        expectedPass = emptyTracingPass
--        in
--          trace getIntersectionTracer initialPass `shouldBe` Right (expectedPass, Nothing)
--    it "Should return error due to empty pass" $ do
--      trace getIntersectionTracer emptyTracingPass `shouldBe` Left (GeneralError noIntersectionsErrorMessage)
--
--testAnyIntersectionsTracer :: IO()
--testAnyIntersectionsTracer = hspec $ do
--  describe "AnyIntersectionsTracer should return whether there are any intersections without modifying the pass" $ do
--    it "Should return false for empty pass" $ do
--      trace anyIntersectionsTracer emptyTracingPass `shouldBe` Right (emptyTracingPass, False)
--    it "Should return true for non-empty pass" $ do
--      trace anyIntersectionsTracer pass `shouldBe` Right (pass, True) where
--        pass = TracingPass [Nothing] []
--
--testTracerModule :: IO()
--testTracerModule = hspec $ do
--  describe "Tracer module" $ do
--    it "" testIdentityTracer
--    it "" testAbortTracer
--    it "" testShootRayTracer
--    it "" testGetIntersectionTracer
--    it "" testAnyIntersectionsTracer
--