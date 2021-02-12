module Shading.Color_Test where

import Base
import Shading.Color
import Data.Word

import Test.Hspec

testScale :: IO()
testScale = hspec $ do
  describe "Scaling colors with scalars" $ do
    it "Scale non-black color with scalar bigger than 0" $ do
      (0.5 `scale` Rgb 0.6 0.4 0.2) `shouldBe` Rgb 0.3 0.2 0.1
    it "Scale non-black color with scalar 0 should be black" $ do
      (0 `scale` Rgb 0.6 0.4 0.2) `shouldBe` black
    it "Scale black color with scalar bigger than 0" $ do
      (3 `scale` black) `shouldBe` black

testMultiply :: IO()
testMultiply = hspec $ do
  describe "Multiplying colors component-wise" $ do
    it "Multiply two non-black colors" $ do
      (Rgb 0.3 0.7 1 `multiply` Rgb 0.6 0.4 0.3) `shouldBe` Rgb 0.18 0.28 0.3
    it "Scale non-black color with black color" $ do
      (black `multiply` Rgb 0.6 0.4 0.2) `shouldBe` black
    it "Scale non-black color with white" $ do
      let c = Rgb 0.3 0.7 0.1 in
        (white `multiply` c) `shouldBe` c
            
testAdd :: IO()
testAdd = hspec $ do
  describe "Adding colors component-wise" $ do
    it "Adding two arbitrary colors" $ do
      (Rgb 0.3 0.5 0.2 `add` Rgb 0.9 0.22 0.7) `shouldBe` Rgb 1.2 0.72 0.9
    it "Adding arbitary color with black" $ do
      let c = Rgb 0.7 0.2 0.11 in
        (c `add` black) `shouldBe` c

testClamp :: IO()
testClamp = hspec $ do
  describe "Clamping color`s components in range [0,1]" $ do
    it "Clamping color already in bounds" $ do
      let c = Rgb 0.7 0.2 0.11 in
        clamp c `shouldBe` c
    it "Clamping color with negative red component" $ do
      clamp (Rgb (-2) 0.3 0.2) `shouldBe` Rgb 0 0.3 0.2
    it "Clamping color with all components above 1" $ do
      clamp (Rgb 5 10 11) `shouldBe` white

testToRgb24 :: IO()
testToRgb24 = hspec  $ do
  describe "Converting color's components to 255 bit numbers" $ do
    it "Converting black color" $ do
      toRgb24 black `shouldBe` (0, 0, 0)
    it "Converting white color" $ do
      toRgb24 white `shouldBe` (255, 255, 255)
    it "Converting grey color" $ do
      toRgb24 (Rgb 0.5 0.5 0.5) `shouldBe` (127, 127, 127)

testColorModule :: IO()
testColorModule = hspec $ do
  describe "Color module" $ do
    it "" testScale
    it "" testMultiply
    it "" testAdd
    it "" testClamp
    it "" testToRgb24