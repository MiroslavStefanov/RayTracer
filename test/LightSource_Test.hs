module LightSource_Test where

import Base
import LightSource
import Shading.Color
import Shading.ShadingContext
import Shading.Texture
import Shading.Sampler
import Tracing.Tracer
import Tracing.TracingPass
import Intersection
import qualified Vector

import Test.Hspec

testAmbientLight :: IO()
testAmbientLight = hspec $ do
  describe "Ambient light" $ do
    let 
      light = getLighting $ AmbientLight intensity color
      intensity = 0.8
      color = Rgb 1 0.5 0
      in do
      context "Visibility check" $ do
        it "Do nothing for any intersection" $ do
          trace (visibility light emptyIntersection) emptyTracingPass `shouldBe` Right (emptyTracingPass, ())
      context "Occlusion" $ do
        it "No occlusion for any tracing pass" $ do
          trace (occlusion light (0,0,0)) emptyTracingPass `shouldBe` Right (emptyTracingPass, 1.0)
      context "Lighting" $ do
        it "Diffuse color of shading context mutiplied by light color scaled by light intensity" $ do
          let 
            diffuseColor = Rgb 0.2 0.8 1
            tex = Texture (constantSampler diffuseColor) (constantSampler 1) 0 0 NoMaterial
            i = Intersection (0,0,0) (0,0,0) tex 0 (0, 0)
            shadingContext = ShadingContext ((0,0,0), (0,0,0)) i
            expectedColor = scale intensity $ multiply diffuseColor color
            in do
              lighting light shadingContext `shouldBe` expectedColor

testPointLight :: IO()
testPointLight = hspec $ do
  describe "Point light" $ do
    let 
      light = getLighting $ PointLight intensity color pos
      intensity = 100
      color = Rgb 1 0.5 0
      pos = (0, 0, 10)
      in do
      context "Visibility check" $ do
        let
          inputIntersection = Intersection (3, 4, 0) (0, 0, 1) emptyTexture 0 (0, 0)
          rayOrigin = getPositiveBiasedIntersectionPosition inputIntersection
          rayDirection = Vector.normalize $ pos `Vector.subtract` position inputIntersection
          expectedPass = TracingPass [] [(rayOrigin, rayDirection)]
          in do
            it "Do nothing for any intersection" $ do
              trace (visibility light inputIntersection) emptyTracingPass `shouldBe` Right (expectedPass, ())
      context "Occlusion" $ do
        let
          shadowIntersection = \d -> Intersection (0,0,0) (0,0,0) tex d (0,0)
          tex = Texture (constantSampler white) (constantSampler alpha) 0 0 NoMaterial
          alpha = 0.8
          in do
            it "Intersection casting shadow" $ do
              trace (occlusion light (3, 4, 0)) (TracingPass [Just (shadowIntersection 5)] []) `shouldBe` Right (emptyTracingPass, 1 - alpha)
            it "Intersection no casting shadow" $ do
              trace (occlusion light (3, 4, 0)) (TracingPass [Just (shadowIntersection 25)] []) `shouldBe` Right (emptyTracingPass, 1)
            it "No intersection no casting shadow" $ do
              trace (occlusion light (3, 4, 0)) (TracingPass [Nothing] []) `shouldBe` Right (emptyTracingPass, 1)
      context "Lighting" $ do
        it "Phong illumination model" $ do
          let
            diffuseColor = Rgb 1 1 1
            alpha = 0.8
            tex = Texture (constantSampler diffuseColor) (constantSampler alpha) 1 1 NoMaterial
            i = Intersection (3,4,0) (0,0,1) tex 0 (0.5,0.5)
            inputContext = ShadingContext ((0, 0, 7), (1, 0, 0)) i
            result = lighting light inputContext
            round3 = \f -> fromIntegral (truncate $ f * 1000)/1000
            roundedColor = Rgb (round3 $ red result) (round3 $ green result) (round3 $ blue result)
            in do
               roundedColor `shouldBe` Rgb 0.715 0.357 0

testLightSourceModule :: IO()
testLightSourceModule = hspec $ do
  describe "LightSource module" $ do
    it "" testAmbientLight
    it "" testPointLight