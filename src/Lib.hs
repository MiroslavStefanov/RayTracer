module Lib
    ( renderScene
    --, debugRenderScene
    ) where

import Base
import Tracing.Scene
import Shading.FrameBuffer
import Tracing.Mesh
-- import Shading.Color
-- import Tracing.Mesh
import Shading.PixelState
import Shading.Shader
import Rendering
import qualified SDL

import ErrorHandling.ErrorMessages

import Export ( saveImageAsPng )
import qualified Vector as Vec
import Geometry
import Data.List (any)

import PinholeCamera
import qualified Shading.Color as Color
import Shading.Color
import SceneBuilder


computePixel :: Scene -> [Shader] -> PixelState -> PixelState
computePixel scene shaders state = case state of
  (Shading incommingRay hit) -> let
            shadersLength = length shaders
            shader = shaders !! (shadersLength - index - 1)
            index = shaderId $ mesh hit 
            in shader scene incommingRay (intersection hit)
  (Ready color) -> Ready color
  (Tracing ray) -> let
    maybeHit = traceRay scene ray in
    case maybeHit of
      Nothing -> Ready black
      Just hit -> Shading ray hit
  (Composition states weights) -> let
    computedStates = map (computePixel scene shaders) states
    weightedStates = zip computedStates weights
    colorCombiner totalColor (pixel, weight) = Color.clamp $ Color.add totalColor $ Color.scale weight $ colorizePixel pixel
    readyColor = foldl colorCombiner black weightedStates
    (nextStates, nextWeights) = unzip $ filter (not . canColorizePixel . fst) weightedStates
    in case nextStates of
      [] -> Ready readyColor
      _ -> Composition (Ready readyColor : nextStates) (1.0 : nextWeights)



computeImage :: Int -> Scene -> [Shader] -> FrameBuffer PixelState -> FrameBuffer Rgb
computeImage depth scene shaders oldBuffer = image where
  newBuffer = fmap (computePixel scene shaders) oldBuffer
  isReady = all canColorizePixel (buffer newBuffer)
  image = if isReady ||  depth <= 0 then
    fmap colorizePixel newBuffer else
      computeImage (depth - 1) scene shaders newBuffer


traceImage :: Int -> Scene -> [Shader] -> Perspective -> FrameBuffer Rgb
traceImage depth scene shaders p = computeImage depth scene shaders initialBuffer where
  initialBuffer = fmap Tracing raysBuffer
  raysBuffer = fmap (\t -> getRay t $ camera p) texelBuffer
  texelBuffer = createBuffer (PinholeCamera.width p) (PinholeCamera.height p)



-- shadingStepTracer :: Scene -> FrameBuffer ShadingDensity -> Tracer (FrameBuffer ShadingDensity)
-- shadingStepTracer scene buffer = do
--     generateIntersectionsTracer scene
--     hasAnyIntersections <- anyIntersectionsTracer
--     if hasAnyIntersections
--       then do
--         shadedBuffer <- transformBufferTracer (shadeTracer scene) buffer
--         transformBufferTracer (shootRaysFromIntersectionTracer scene) shadedBuffer
--       else
--         return buffer

-- shadeFrameBufferTracer :: Scene -> FrameBuffer ShadingDensity -> Int -> Tracer (FrameBuffer ShadingDensity)
-- shadeFrameBufferTracer scene buffer remainingSteps
--   | remainingSteps <= 0 = identityTracer buffer
--   | otherwise = do
--     nextBuffer <- shadingStepTracer scene buffer
--     shadeFrameBufferTracer scene nextBuffer (remainingSteps - 1)

-- renderSceneTracer :: Int -> Int -> PinholeCamera -> Scene -> Int -> Tracer (FrameBuffer Rgb)
-- renderSceneTracer bufferWidth bufferHeight camera scene maxRecursionDepth = 
--   let indexedBuffer = createBuffer bufferWidth bufferHeight
--   in do
--     cameraRaysBuffer <- transformBufferTracer (shootCameraRayTracer camera) indexedBuffer
--     initialBuffer <- transformBufferTracer initialDensityTracer cameraRaysBuffer
--     shadedBuffer <- shadeFrameBufferTracer scene initialBuffer maxRecursionDepth
--     transformBufferTracer (identityTracer . getShadingColor) shadedBuffer

-- traceScene :: Int -> Int -> PinholeCamera -> Scene -> Int -> Either TraceError (FrameBuffer Rgb)
-- traceScene bufferWidth bufferHeight camera scene recursionDepth =
--   let tracingResult = trace (renderSceneTracer bufferWidth bufferHeight camera scene recursionDepth) emptyTracingPass 
--   in case tracingResult of 
--     Left error -> Left error
--     Right (_, buffer) -> Right buffer

-- preformTracingPass :: SDL.Renderer -> Scene -> FrameBuffer ShadingDensity -> TracingPass -> IO (TracingPass, FrameBuffer ShadingDensity)
-- preformTracingPass renderer scene buffer pass = let
--   stepTracer = do
--     nextBuffer <- shadingStepTracer scene buffer
--     colorizedBuffer <- transformBufferTracer (identityTracer . getShadingColor) nextBuffer
--     return (nextBuffer, colorizedBuffer)
--   result = trace stepTracer pass
--   in case result of
--       Left (GeneralError msg) -> do
--         putStrLn $ "Error: " ++ msg
--         return (emptyTracingPass, createEmptyBuffer)
--       Right (nextPass, (densityBuffer, colorBuffer)) -> do
--         renderFrameBuffer renderer colorBuffer
--         return (nextPass, densityBuffer)

-- exportImage :: Scene -> Perspective -> Int -> String -> IO ()
-- exportImage scene (Perspective camera width height) recDepth outputName = do
--   startTime <- getCurrentTime
--   case image of
--     Right img -> do
--       putStr "[Tracing]Seconds elapsed: "
--       endTime <- getCurrentTime
--       print $ nominalDiffTimeToSeconds (endTime `diffUTCTime` startTime)
--       saveImageAsPng outputName img
--     Left (GeneralError msg) -> putStrLn $ "Error: " ++ msg
--     where
--       image = traceScene width height camera scene recDepth

renderLoop :: SDL.Renderer -> FrameBuffer Rgb -> IO Bool
renderLoop renderer image = do
  events <- SDL.pollEvents
  if any (isKeyPress SDL.KeycodeQ) events
  then return True
  else if any (isKeyPress SDL.KeycodeE) events
  then return False
  else do
    renderFrameBuffer renderer image
    renderLoop renderer image

renderScene :: ShadingContext -> Perspective -> Int -> IO()
renderScene ctx@(ShadingContext scene shaders) perspective@(Perspective camera width height) recDepth = do
  (window, renderer) <- openWindow width height
  shouldQuit <- renderLoop renderer image
  if shouldQuit
  then return ()
  else do
    renderScene ctx perspective recDepth where
      image = traceImage recDepth scene shaders perspective

-- debugRenderLoop :: SDL.Renderer -> Scene -> FrameBuffer ShadingDensity -> TracingPass -> IO Int
-- debugRenderLoop renderer scene buffer@(FrameBuffer w h densities) pass = do
--   events <- SDL.pollEvents
--   if any (isKeyPress SDL.KeycodeT) events
--     then do
--       putStrLn "Starting next tracing pass:"
--       putStrLn $ show (length (outputRays pass)) ++ "rays"
--       startTime <- getCurrentTime
--       (nextPass, nextBuffer) <- preformTracingPass renderer scene buffer pass
--       endTime <- getCurrentTime
--       putStr "[Tracing]Seconds elapsed: "
--       print $ nominalDiffTimeToSeconds (endTime `diffUTCTime` startTime)
--       debugRenderLoop renderer scene nextBuffer nextPass
--     else if any (isKeyPress SDL.KeycodeQ) events
--       then return 0
--     else if any (isKeyPress SDL.KeycodeLeft) events
--       then return (-1)
--     else if any (isKeyPress SDL.KeycodeRight) events
--       then return 1
--     -- else if any (isKeyPress SDL.KeycodeDown) events
--     --   then return 2
--     else
--         let colorBuffer = FrameBuffer w h $ map getShadingColor densities
--         in do
--           renderFrameBuffer renderer colorBuffer
--           debugRenderLoop renderer scene buffer pass   

-- debugRenderScene :: Scene -> Perspective -> IO Int
-- debugRenderScene scene (Perspective camera width height) = 
--   let 
--     indexedBuffer = createBuffer width height
--     initializeTracer = transformBufferTracer (shootCameraRayTracer camera) indexedBuffer >>= transformBufferTracer initialDensityTracer
--     Right (initialPass, initialBuffer) = trace initializeTracer emptyTracingPass
--     in do
--       (window, renderer) <- openWindow width height
--       (initialPass, initialBuffer) <- preformTracingPass renderer scene initialBuffer initialPass
--       debugRenderLoop renderer scene initialBuffer initialPass
