module Lib
    ( traceScene
    , saveImage
    , makePinholeCamera
    , makeMesh
    ) where

import Base
import Tracing.Scene
import Tracing.Tracer
import PinholeCamera
import Shading.FrameBuffer
import Shading.Color
import Shading.ShadingContext
import Shading.ShadingTracers
import Shading.Texture
import Tracing.Mesh

import ErrorHandling.ErrorMessages
import Tracing.TracingPass

import Export ( saveImageAsPng )
import qualified Vector as Vec
import Geometry

shadeFrameBufferTracer :: Scene -> FrameBuffer ShadingValue -> Int -> Tracer (FrameBuffer ShadingValue)
shadeFrameBufferTracer scene buffer remainingSteps
  | remainingSteps <= 0 = identityTracer buffer
  | otherwise = do
    newBuffer <- transformBufferTracer (shadeTracer scene) buffer
    generateIntersectionsTracer scene
    hasAnyIntersections <- anyIntersectionsTracer
    if hasAnyIntersections
      then do
        shadeFrameBufferTracer scene newBuffer $ remainingSteps - 1
      else do
        transformBufferTracer (shadeTracer scene) newBuffer -- one more step to process ShadingValues that need processing without intersections

nzTracer :: ShadingValue -> Tracer Rgb
nzTracer (Left color) = do return color
nzTracer (Right _) = abortTracer recursionDepthNotEnoughErrorMessage

testTracer :: Int -> Int -> PinholeCamera -> Scene -> Int -> Tracer (FrameBuffer Rgb)
testTracer bufferWidth bufferHeight camera scene maxRecursionDepth = do
  indexedBuffer <- indexTexelsTracer bufferWidth bufferHeight
  primaryRaysBuffer <- transformBufferTracer (shootCameraRayTracer camera) indexedBuffer
  generateIntersectionsTracer scene
  initialBuffer <- transformBufferTracer (cameraRayIntersectionTracer scene) primaryRaysBuffer
  generateIntersectionsTracer scene
  finalBuffer <- shadeFrameBufferTracer scene initialBuffer maxRecursionDepth
  transformBufferTracer nzTracer finalBuffer


traceScene :: Int -> Int -> PinholeCamera -> Scene -> Int -> Either TraceError (FrameBuffer Rgb)
traceScene bufferWidth bufferHeight camera scene recursionDepth =
  let tracingResult = trace (testTracer bufferWidth bufferHeight camera scene recursionDepth) emptyTracingPass 
  in case tracingResult of 
    Left error -> Left error
    Right (_, buffer) -> Right buffer

saveImage :: FilePath -> FrameBuffer Rgb -> IO ()
saveImage = saveImageAsPng

makePinholeCamera :: Vec.Vector -> Vec.Vector -> Vec.Vector -> Float -> Float -> PinholeCamera
makePinholeCamera = prepareCamera

makeMesh :: Geometry -> Texture -> Mesh
makeMesh = Mesh