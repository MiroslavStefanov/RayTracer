module Shading.FrameBuffer_Test where

import Base
import Shading.FrameBuffer
import Tracing.Tracer
import Tracing.TracingPass

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

testTransformBufferTracer :: IO()
testTransformBufferTracer = hspec $ do
  describe "Applying the same tracer to all elements in a buffer" $ do
    it "Apply identityTracer to buffer" $ do
      let buffer = createBuffer 5 5 in
        trace (transformBufferTracer identityTracer buffer) emptyTracingPass `shouldBe` Right (emptyTracingPass, buffer)
    it "Apply identityTracer to empty byffer" $ do
      let buffer = createEmptyBuffer :: FrameBuffer Int in
        trace (transformBufferTracer identityTracer buffer) emptyTracingPass `shouldBe` Right (emptyTracingPass, buffer)

testFrameBufferModule :: IO()
testFrameBufferModule = hspec $ do
  describe "FrameBuffer module" $ do
    it "" testCreateBuffer
    it "" testTransformBufferTracer 