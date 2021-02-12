module Shading.Sampler_Test where

import Base
import Shading.Sampler
import Shading.Color

import Test.Hspec

testConstantSampler :: IO()
testConstantSampler = hspec $ do
  describe "Constant sampler that has the same value for each texel" $ do
    it "Sample at arbirary texel" $ do
      let c = 3 in
        sample (constantSampler c) (0.173, 0.461) `shouldBe` c

testCheckerSampler :: IO()
testCheckerSampler = hspec $ do
  describe "Checker sampler" $ do
    context "Black and white checker with size 1" $ do
      let s = checkerSampler black white 1 in do
        it "At texel (0, 0)" $ do
          sample s (0, 0) `shouldBe` black
        it "At texel (1.5, 0)" $ do
          sample s (1.5, 0) `shouldBe` white
        it "At texel (0, 1.5)" $ do
          sample s (0, 1.5) `shouldBe` white
        it "At texel (1.5, 1.5)" $ do
          sample s (1.5, 1.5) `shouldBe` black

testSamplerModule :: IO()
testSamplerModule = hspec $ do
  describe "Sampler module" $ do
    it "" testConstantSampler
    it "" testCheckerSampler