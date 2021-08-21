--import Tracing.Tracer_Test ( testTracerModule )
--import Tracing.Scene_Test ( testSceneModule )
import Shading.Color_Test ( testColorModule )
import Shading.FrameBuffer_Test ( testFrameBufferModule )
import Shading.Sampler_Test ( testSamplerModule )
import GeometryIntersect_Test ( testGeometryModule )
--import LightSource_Test ( testLightSourceModule )
import PinholeCamera_Test ( testPinholeCameraModule )

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests for Ray Tracer" $ do
    --it "" testTracerModule 
    it "" testColorModule 
    it "" testGeometryModule
    it "" testFrameBufferModule
    it "" testSamplerModule
    it "" testPinholeCameraModule
    --it "" testSceneModule