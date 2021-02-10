--import Tracing.Tracer_Test
import GeometryIntersect_Test

import Test.Hspec

main :: IO ()
main = hspec $ do
  -- describe "Package Tracing" $ do
  --   context "Module Tracer" $ do
  --     it "" testIdentityTracer
  --     it "" testAbortTracer
  --     it "" testShootRayTracer
  --     it "" testGetIntersectionTracer
  --     it "" testAnyIntersectionsTracer
  --     it "" testIndexTexelsTracer
  --     it "" testShootCameraRayTracer
  --     it "" testCameraRayIntersectionTracer
  --     it "" testTransformBufferTracer
  --     it "" testShadeTracer
  --     it "" testGenerateIntersectionsTracer
  describe "Geometry intersections" $ do
    context "Intersection results" $ do
      it "" testIntersectPlaneWithRay
      it "" testIntersectSphereWithRay
      it "" testIntersectTriangleWithRay
      it "" testIntersectParallelepipedWithRay
      it "" testIntersectConeWithRay
      it "" testIntersectTorusWithRay