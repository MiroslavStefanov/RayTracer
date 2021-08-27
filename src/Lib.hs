module Lib
    ( renderScene
    --, debugRenderScene
    ) where

import Base
import Tracing.Scene
import Shading.FrameBuffer
import Tracing.Mesh
import Shading.PixelState
import Shading.Shader
import Rendering
import qualified SDL

import Export ( saveImageAsPng )
import qualified Vector as Vec
import Geometry
import Data.List (any)

import PinholeCamera
import Shading.Color
import SceneBuilder

computePixel :: ShadingContext -> PixelState -> PixelState
computePixel ctx@(ShadingContext scene shaders) state = case state of
  (Ready color) -> Ready color
  (Tracing ray) -> let
    maybeHit = traceRay scene ray in
    case maybeHit of
      Nothing -> Ready black
      Just hit -> Shading ray hit
  (Composition states weights) -> let
    computedStates = map (computePixel ctx) states
    weightedStates = zip computedStates weights
    colorCombiner totalColor (pixel, weight) = clamp $ add totalColor $ scale weight $ colorizePixel pixel
    readyColor = foldl colorCombiner black weightedStates
    (nextStates, nextWeights) = unzip $ filter (not . canColorizePixel . fst) weightedStates
    in case nextStates of
      [] -> Ready readyColor
      _ -> Composition (Ready readyColor : nextStates) (1.0 : nextWeights)
  (Shading incommingRay hit) -> let
            shadersLength = length shaders
            shader = shaders !! (shadersLength - index - 1)
            index = shaderId $ mesh hit 
            in shader scene incommingRay (intersection hit)

computeImage :: Int -> ShadingContext -> FrameBuffer PixelState -> FrameBuffer Rgb
computeImage depth shadingContext oldBuffer = image where
  newBuffer = fmap (computePixel shadingContext) oldBuffer
  isReady = all canColorizePixel (buffer newBuffer)
  image = if isReady ||  depth <= 0 then
    fmap colorizePixel newBuffer else
      computeImage (depth - 1) shadingContext newBuffer

traceImage :: Int -> ShadingContext -> Perspective -> FrameBuffer Rgb
traceImage depth shadingContext p = computeImage depth shadingContext initialBuffer where
  initialBuffer = fmap Tracing raysBuffer
  raysBuffer = fmap (\t -> getRay t $ camera p) texelBuffer
  texelBuffer = createBuffer (PinholeCamera.width p) (PinholeCamera.height p)


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
renderScene shadingContext perspective@(Perspective camera width height) recDepth = do
  (window, renderer) <- openWindow width height
  shouldQuit <- renderLoop renderer image
  if shouldQuit
  then return ()
  else do
    renderScene shadingContext perspective recDepth where
      image = traceImage recDepth shadingContext perspective

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