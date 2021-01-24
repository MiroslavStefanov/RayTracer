{-# LANGUAGE TupleSections #-}
module Tracing.Tracer where

import Base
import ErrorHandling.ErrorMessages
import Tracing.TracingPass
import Tracing.Scene
import Vector
import Ray
import Intersection
import PinholeCamera hiding (position)
import qualified LightSource as LS
import Shading.FrameBuffer
import Shading.ShadingContext
import Shading.Color
import Shading.ShadingUtils
import Control.Applicative (Alternative (empty, many, (<|>)))

import Data.Bifunctor (Bifunctor (second))
import Prelude hiding (subtract)

import Shading.Texture
import Control.Monad (replicateM)

newtype TraceError
  = GeneralError String
  deriving (Show, Eq)

abort :: String -> Either TraceError b
abort = Left . GeneralError

type TraceResult a = Either TraceError (TracingPass, a)

newtype Tracer a = Tracer
  { trace :: TracingPass -> TraceResult a
  }

identityTracer :: a -> Tracer a
identityTracer a = Tracer $ Right . (, a)

abortTracer :: String -> Tracer a
abortTracer error = Tracer $ \_ -> abort error

instance Functor Tracer where
  fmap mapper tracer =
    Tracer $ fmap (second mapper) . trace tracer

instance Applicative Tracer where
  pure a = Tracer $ Right . (,a)
  Tracer ft <*> Tracer vt = Tracer $ \pass -> do
    (pass', f) <- ft pass
    (pass'', v) <- vt pass'
    return (pass'', f v)

instance Monad Tracer where
  Tracer t >>= func = Tracer $ \pass -> do
    (pass', a) <- t pass
    trace (func a) pass'
  
instance Alternative Tracer where
  empty = abortTracer ""
  Tracer ta <|> Tracer tb = Tracer $ \pass ->
    case ta pass of
      Left _ -> tb pass
      result -> result

shootRayTracer :: Ray -> Tracer ()
shootRayTracer ray = Tracer $ \(TracingPass intersections rays) -> let
  newPass = TracingPass intersections (ray : rays)
  in
    Right (newPass, ())

getIntersectionTracer :: Tracer (Maybe Intersection)
getIntersectionTracer = Tracer $ \(TracingPass intersections r) ->
  case intersections of
    (h : t) -> Right (TracingPass t r, h)
    _ -> Left $ GeneralError noIntersectionsErrorMessage

anyIntersectionsTracer :: Tracer Bool 
anyIntersectionsTracer = Tracer $ \pass@(TracingPass intersections rays) -> Right (pass, not (null intersections))

indexTexelsTracer :: Int -> Int -> Tracer (FrameBuffer Texel)
indexTexelsTracer w h = Tracer $ \pass -> Right (pass, createBuffer w h)

shootCameraRayTracer :: PinholeCamera -> Texel -> Tracer Ray
shootCameraRayTracer camera texel = let
  cameraRay = getRay texel camera
  in do
    shootRayTracer cameraRay
    return cameraRay

shootRayTowardsLightTracer :: Intersection -> LS.LightSource -> Tracer ()
shootRayTowardsLightTracer 
  intresection@(Intersection intPosition intNormal _ _ _)
  (LS.PointLight _ _ lightPosition) = 
    shootRayTracer rayToLight where
      rayStart = getShadowRayStartPoint intresection
      rayDirection = normalize $ subtract lightPosition intPosition
      rayToLight = (rayStart, rayDirection)

shootRayTowardsLightTracer _ _ = identityTracer ()

cameraRayIntersectionTracer :: Scene -> Ray -> Tracer ShadingValue
cameraRayIntersectionTracer scene ray = do
  maybeIntersection <- getIntersectionTracer
  case maybeIntersection of
    Nothing -> return $ Left white
    Just i -> do
      mapM_ (shootRayTowardsLightTracer i) (lightSources scene)
      return $ Right $ ShadingContext ray i

shadeTracer :: Scene -> ShadingValue -> Tracer ShadingValue
shadeTracer _ (Left rgb) = do
  identityTracer $ Left rgb

shadeTracer scene (Right context@(ShadingContext ray intersection)) = do
  case texture intersection of
    InvalidTexture msg -> abortTracer msg
    ColorTexture rgb -> return $ Left rgb
    PhongTexture _ specMult specExp -> do
      shadowIntersections <- Control.Monad.replicateM (getLightSourcesCount scene) getIntersectionTracer
      let 
        shadowMultipliers = zipWith getShadowMultiplier shadowIntersections $ lightSources scene
        colorSums = map (getLightContribution context) $ lightSources scene
        shadedColors = zipWith Shading.Color.scale shadowMultipliers colorSums
        accumulatedLightContributions = foldl Shading.Color.add (Rgb 0 0 0) shadedColors
        in
          return $ Left $ clamp accumulatedLightContributions


transformBufferTracer :: (a -> Tracer b) -> FrameBuffer a -> Tracer (FrameBuffer b)
transformBufferTracer func (FrameBuffer w h buff) = let
  newBuffer = mapM func buff in
    FrameBuffer w h <$> newBuffer

generateIntersectionsTracer :: Scene -> Tracer ()
generateIntersectionsTracer scene = Tracer $ \pass -> Right (calculateNextTracingPass pass scene, ())

shadeFrameBufferTracer :: Scene ->  FrameBuffer ShadingValue -> Int -> Tracer (FrameBuffer ShadingValue)
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


-- testTracer :: Int -> Int -> PinholeCamera -> Scene -> Int -> Tracer (FrameBuffer Rgb)
-- testTracer bufferWidth bufferHeight camera scene maxRecursionDepth = do
--   indexedBuffer <- indexTexelsTracer bufferWidth bufferHeight
--   primaryRaysBuffer <- transformBufferTracer (shootCameraRayTracer camera) indexedBuffer
--   initialShadedBuffer <- transformBufferTracer (cameraRayIntersectionTracer scene) primaryRaysBuffer


-- testShootCameraRayTracer = let
--   w = 2
--   h = 2
--   camera = prepareCamera (0,0,10) (0,1,10) (0,0,1) (pi / 2.5) (fromIntegral w / fromIntegral h)
--   initialPass = TracingPass [] []
--   testTracer = do
--     texelsBuffer <- indexTexelsTracer w h
--     transformBufferTracer (shootCameraRayTracer camera) texelsBuffer
--   in
--     print $ trace testTracer initialPass

-- testFunc = trace testTracer $ TracingPass testIntersections [] where
--   testTracer = do
--     indexedBuffer <- indexTexelsTracer 2 2
--     newBuffer <- transformBufferTracer intersectionPositionTracer indexedBuffer
--     return newBuffer
--   testIntersection = Just $ Intersection (0, 1, 0) (1, 0, 0) (InvalidTexture "") 0 (0, 0)
--   testIntersections = [testIntersection, testIntersection, testIntersection, testIntersection]

-- testTracer :: Tracer Ray
-- testTracer = do
--   i <- getIntersectionShader
--   shootRayShader r
--   return r
--   where r = ((0, 0, 0), (1, 0, 0))

-- testFunc = let
--   emptyIntersection = Intersection (0, 0, 0) (0, 0, 0) (InvalidTexture "") 0.0 (0.0, 0.0)
--   initialPass = TracingPass [Just emptyIntersection] []
--   in
--     print $ trace testTracer initialPass