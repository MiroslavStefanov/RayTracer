import Tracing.Tracer_Test

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Package Tracing" $ do
    context "Module Tracer" $ do
      it "" testIdentityTracer
      it "" testAbortTracer
      it "" testShootRayTracer
      it "" testGetIntersectionTracer
      it "" testIndexTexelsTracer
      it "" testShootCameraRayTracer
      it "" testCameraRayIntersectionTracer
      it "" testTransformBufferTracer
