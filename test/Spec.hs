import Shading.Color_Test ( testColorModule )
import Shading.FrameBuffer_Test ( testFrameBufferModule )
import Shading.Sampler_Test ( testSamplerModule )
import GeometryIntersect_Test ( testGeometryModule )
import PinholeCamera_Test ( testPinholeCameraModule )
import Lighting.Lighting_Test ( testLightingModule )

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests for Ray Tracer" $ do
    it "" testLightingModule
    it "" testColorModule 
    it "" testGeometryModule
    it "" testFrameBufferModule
    it "" testSamplerModule
    it "" testPinholeCameraModule