module GeometryIntersect_Test where

import Shading.Texture
import Intersection
import Geometry
import Ray
import Test.Hspec

testPlane :: Geometry
testPlane = Plane (0, 0, 45) (0, 0, -1)

testSphere :: Geometry
testSphere = Sphere (0, 0, 15) 5

testTriangle :: Geometry
testTriangle = Triangle (0, -5, 15) (-5, 5, 15) (5, 5, 15)

testParallelepiped :: Geometry
testParallelepiped = Parallelepiped (0, 0, 15) (1, 0, 0) (0, 1, 0) (0, 0, 1) (5, 5, 5)

testRay1 :: Ray
testRay1 = ((0, 0, 0), (0, 0, 1))

testRay2 :: Ray
testRay2 = ((0, 0, 0), (1/3, 2/3, 2/3))

testRay3 :: Ray
testRay3 = ((0, 0, 0), (1, 0, 0))

testRay4 :: Ray
testRay4 = ((0, 4, 0), (0, 0, 1))

testRay5 :: Ray
testRay5 = ((5, 5, 0), (0, 0, 1))

testIntersectPlaneWithRay :: IO()
testIntersectPlaneWithRay = hspec $ do
  describe "Intersect testPlane with testRays" $ do
    it "Should return intersection at (0, 0, 45)" $ do
      intersect testRay1 testPlane `shouldBe` Just (Intersection (0, 0, 45)
                                                                 (0, 0, -1)
                                                                 (InvalidTexture "blank")
                                                                 45
                                                                 (0, 0))
    it "Should return intersection at (22.5, 45, 45)" $ do
      intersect testRay2 testPlane `shouldBe` Just (Intersection (22.5, 45, 45)
                                                                 (0, 0, -1)
                                                                 (InvalidTexture "blank")
                                                                 67.5
                                                                 (-45, -22.5))
    it "Should return Nothing" $ do
      intersect testRay3 testSphere `shouldBe` Nothing
      
testIntersectSphereWithRay :: IO()
testIntersectSphereWithRay = hspec $ do
  describe "Intersect testSphere with testRays" $ do
    it "Should return intersection at (0, 0, 10)" $ do
      intersect testRay1 testSphere `shouldBe` Just (Intersection (0, 0, 10)
                                                                  (0, 0, -1)
                                                                  (InvalidTexture "blank")
                                                                  10
                                                                  (0.5,-0.63258463))
    it "Should return intersection at (0, 4, 12)" $ do
      intersect testRay4 testSphere `shouldBe` Just (Intersection (0, 4, 12)
                                                                  (0, 0.8, -0.6)
                                                                  (InvalidTexture "blank")
                                                                  12
                                                                  (0.75, -0.37790182))
    it "Should return Nothing" $ do
      intersect testRay3 testSphere `shouldBe` Nothing

testIntersectTriangleWithRay :: IO()
testIntersectTriangleWithRay = hspec $ do
  describe "Intersect testTriangle with testRays" $ do
    it "Should return intersection at (0, 0, 15)" $ do
      intersect testRay1 testTriangle `shouldBe` Just (Intersection (0, 0, 15)
                                                                    (0, 0, -1)
                                                                    (InvalidTexture "blank")
                                                                    15
                                                                    (0.25, 0.25))
    it "Should return intersection at (5, 5, 15)" $ do
      intersect testRay5 testTriangle `shouldBe` Just (Intersection (5, 5, 15)
                                                                    (0, 0, -1)
                                                                    (InvalidTexture "blank")
                                                                    15
                                                                    (0, 1))
    it "Should return Nothing" $ do
      intersect testRay3 testTriangle `shouldBe` Nothing      

testIntersectParallelepipedWithRay :: IO()
testIntersectParallelepipedWithRay = hspec $ do
  describe "Intersect testParallelepiped with testRays" $ do
    it "Should return intersection at (0, 0, 10)" $ do
      intersect testRay1 testParallelepiped `shouldBe` Just (Intersection (0, 0, 10)
                                                                          (0, 0, -1)
                                                                          (InvalidTexture "blank")
                                                                          10
                                                                          (0, 0))
    it "Should return intersection at (5, 5, 10)" $ do
      intersect testRay5 testParallelepiped `shouldBe` Just (Intersection (5, 5, 10)
                                                                          (1, 0, 0)
                                                                          (InvalidTexture "blank")
                                                                          10
                                                                          (0, 0))
    it "Should return Nothing" $ do
      intersect testRay3 testParallelepiped `shouldBe` Nothing