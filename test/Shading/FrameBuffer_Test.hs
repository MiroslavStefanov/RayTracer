module Shading.FrameBuffer_Test where

import Base
import Shading.FrameBuffer

import Test.Hspec

testCreateBuffer :: IO()
testCreateBuffer = hspec $ do
  describe "Creating empty buffer containing texels" $ do
    it "3x3 buffer" $ do
      createBuffer 3 3 `shouldBe` FrameBuffer 3 3 [
        (0, 0), (0.5, 0), (1, 0), (0, 0.5), (0.5, 0.5), (1, 0.5), (0, 1), (0.5, 1), (1, 1)
        ]
    it "Buffer with width 0" $ do
      createBuffer 0 5 `shouldBe` createEmptyBuffer
    it "Buffer with height 0" $ do
      createBuffer 3 0 `shouldBe` createEmptyBuffer

testFrameBufferModule :: IO()
testFrameBufferModule = hspec $ do
  describe "FrameBuffer module" $ do
    it "" testCreateBuffer