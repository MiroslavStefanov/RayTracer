import Tracing.Tracer_Test
import Shading.Color_Test
import GeometryIntersect_Test

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests for Ray Tracer" $ do
    it "" testTracerModule 
    it "" testColorModule 
    it "" testGeometryModule
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
  