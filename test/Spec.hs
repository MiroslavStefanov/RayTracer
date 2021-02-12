import Tracing.Tracer_Test
import Shading.Color_Test
import Shading.FrameBuffer_Test
import GeometryIntersect_Test

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests for Ray Tracer" $ do
    it "" testTracerModule 
    it "" testColorModule 
    it "" testGeometryModule
    it "" testGeometryModule
    it "" testFrameBufferModule