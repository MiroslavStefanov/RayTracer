module Lib
    ( traceScene
    , saveImage
    , makePinholeCamera
    , makeMesh
    , exportImage
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

import Data.Foldable (foldlM)

shadeFrameBufferTracer :: Scene -> FrameBuffer ShadingDensity -> Int -> Tracer (FrameBuffer ShadingDensity)
shadeFrameBufferTracer scene buffer remainingSteps
  | remainingSteps <= 0 = identityTracer buffer
  | otherwise = do
    generateIntersectionsTracer scene
    hasAnyIntersections <- anyIntersectionsTracer
    if hasAnyIntersections
      then do
        shadedBuffer <- transformBufferTracer (shadeTracer scene) buffer
        requestedRaysBuffer <- transformBufferTracer (shootRaysFromIntersectionTracer scene) shadedBuffer
        shadeFrameBufferTracer scene requestedRaysBuffer $ remainingSteps - 1
      else
        return buffer

getColorTracer :: ShadingDensity -> Tracer Rgb
getColorTracer [] = abortTracer missingPixelValueErrorMessage
getColorTracer density = foldlM addWeightedColorTracer (Rgb 0 0 0) density where
  addWeightedColorTracer = \totalColor (value, weight) -> do
    case value of
      Left color -> return $ clamp (totalColor `add` scale weight color)
      Right _ -> abortTracer recursionDepthNotEnoughErrorMessage

renderSceneTracer :: Int -> Int -> PinholeCamera -> Scene -> Int -> Tracer (FrameBuffer Rgb)
renderSceneTracer bufferWidth bufferHeight camera scene maxRecursionDepth = do
  indexedBuffer <- indexTexelsTracer bufferWidth bufferHeight
  initialBuffer <- transformBufferTracer (shootCameraRayTracer camera 1) indexedBuffer
  shadedBuffer <- shadeFrameBufferTracer scene initialBuffer maxRecursionDepth
  transformBufferTracer getColorTracer shadedBuffer


traceScene :: Int -> Int -> PinholeCamera -> Scene -> Int -> Either TraceError (FrameBuffer Rgb)
traceScene bufferWidth bufferHeight camera scene recursionDepth =
  let tracingResult = trace (renderSceneTracer bufferWidth bufferHeight camera scene recursionDepth) emptyTracingPass 
  in case tracingResult of 
    Left error -> Left error
    Right (_, buffer) -> Right buffer

saveImage :: FilePath -> FrameBuffer Rgb -> IO ()
saveImage = saveImageAsPng

makePinholeCamera :: Vec.Vector -> Vec.Vector -> Vec.Vector -> Float -> Float -> PinholeCamera
makePinholeCamera = prepareCamera

makeMesh :: Geometry -> Texture -> Mesh
makeMesh = Mesh

exportImage :: Scene -> Perspective -> Int -> String -> IO ()
exportImage scene (Perspective camera width height) recDepth outputName = do
  putStrLn $ "Processing " ++ outputName ++ "..."
  case image of
    Right img -> saveImage outputName img
    Left (GeneralError msg) -> putStrLn $ "Error: " ++ msg
    where      
      image = traceScene width height camera scene recDepth