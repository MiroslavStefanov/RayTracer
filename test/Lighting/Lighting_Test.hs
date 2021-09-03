module Lighting.Lighting_Test where

import Base
import Lighting.PointLight
import Lighting.AmbientLight
import Shading.Texture
import Shading.Color
import Intersection

import Test.Hspec
import Lighting.LightSource (LightSource(lighting, occlusion), ShadowCheck (NoCheck, Ray))
import Shading.Sampler
import Vector (normalize, lengthSqr, subtract)

testAmbientLight :: IO()
testAmbientLight = hspec $ do
 let 
     light = AmbientLight 0.5 white
     intensity = 0.8
     color = Rgb 1 0.5 0
     i = Intersection (0,0,0) (0,0,0) 0 (0, 0)
     in do
     context "Occlusion" $ do
       it "No occlusion for any intersection" $ do
         occlusion light i `shouldBe` NoCheck 
     context "Lighting" $ do
         it "" $ do
             let 
                diffuseColor = Rgb 0.2 0.8 1
                texture = solidColorTexture diffuseColor
                r = ((0,0,0), (0,1,0))
                expectedColor = Rgb 0.1 0.4 0.5
                in 
                    lighting light r i texture `shouldBe` expectedColor

testPointLight :: IO()
testPointLight = hspec $ do
  let 
     light = PointLight intensity color pos
     intensity = 100
     color = Rgb 1 0.5 0
     pos = (0, 10, 1)
     tex = Texture (constantSampler white) (constantSampler (0, 0))
     intersection = Intersection (0, 0, 0) (0, 1, 0) 2 (0, 0)
     ray = ((0, 5, 0), (0.72, -0.72, 0))
     distSqr = lengthSqr $ Vector.subtract pos (0, biasEpsilon, 0)
     in do
     context "Occlusion" $ do
        it "Shadow ray" $ do
          occlusion light intersection `shouldBe` Ray ((0, biasEpsilon, 0), normalize (0, 10, 1)) distSqr
     context "Lighting" $ do
       it "Phong illumination model" $ do
         lighting light ray intersection tex `shouldBe` color

testLightingModule :: IO()
testLightingModule = hspec $ do
 describe "Lighting module" $ do
   it "Ambient light" testAmbientLight
   it "Point light" testPointLight