{-# LANGUAGE TupleSections #-}
module Shading.Shader where

import Base
import Tracing.TracingPass
import Tracing.Scene
import Vector
import Ray
import Intersection
import PinholeCamera hiding (position)
import LightSource
import Shading.FrameBuffer
import Shading.ShadingContext
import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Bifunctor (Bifunctor (second))
import Numeric.Limits

import Shading.Texture

newtype TraceError
  = GeneralError String
  deriving (Show)

abort :: String -> Either TraceError b
abort = Left . GeneralError

type TraceResult a = Either TraceError (TracingPass, a)

newtype Tracer a = Tracer
  { trace :: TracingPass -> TraceResult a
  }

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
  empty = Tracer $ \_ -> abort ""
  Tracer ta <|> Tracer tb = Tracer $ \pass ->
    case ta pass of
      Left _ -> tb pass
      result -> result

identityTracer :: a -> Tracer a
identityTracer = Tracer . (pass,)

abortTracer :: String -> Tracer a
abortTracer error = Tracer $ \_ -> abort error

shootRayTracer :: Ray -> Tracer ()
shootRayTracer ray = Tracer $ \(TracingPass intersections rays) -> let
  newPass = TracingPass intersections (ray : rays)
  in
    Right (newPass, ())

getIntersectionTracer :: Tracer (Maybe Intersection)
getIntersectionTracer = Tracer $ \(TracingPass intersections r) ->
  case intersections of
    (h : t) -> Right (TracingPass t r, h)
    _ -> Left $ GeneralError "No intersections left"

indexTexelsTracer :: Int -> Int -> Tracer (FrameBuffer Texel)
indexTexelsTracer w h = Tracer $ \pass -> Right (pass, createBuffer w h)

intersectionPositionTracer :: Texel -> Tracer Vector
intersectionPositionTracer _ = do
  maybeIntersection <- getIntersectionTracer
  case maybeIntersection of
    Just i -> return $ position i
    Nothing -> abortTracer "Expected intersection"

transformBufferTracer :: (a -> Tracer b) -> FrameBuffer a -> Tracer (FrameBuffer b)
transformBufferTracer func (FrameBuffer w h buff) = let
  newBuffer = mapM func buff in
    FrameBuffer w h <$> newBuffer

shootCameraRayTracer :: PinholeCamera -> Texel -> Tracer Ray
shootCameraRayTracer camera texel = let
  cameraRay = getRay texel camera
  in do
    shootRayTracer cameraRay
    return cameraRay

shootRayTowardsLightTracer :: LightSource -> Intersection -> Tracer ()
shootRayTowardsLightTracer 
  (PointLight _ _ lightPosition) 
  (Intersection intPosition intNormal _ _ _) = 
    shootRayTracer rayToLight where
      rayStart = add intPosition $ scale epsilon intNormal
      rayDirection = normalize subtract lightPosition intPosition
      rayToLight = (rayStart, rayDirection)

shootRayTowardsLightTracer _ _ = identityTracer ()

-- processIntersectionTracer :: Intersection -> Scene -> Tracer ShadingContext
-- processIntersectionTracer intesection scene = let
--   tex = texture intersection
--   in do
--     case tex of
--       PhongTexture specMult specExp -> 

cameraRayIntersectionTracer :: Ray -> Scene -> Tracer ShadingContext
cameraRayIntersectionTracer ray scene = do
  maybeIntersection <- getIntersectionTracer
  case maybeIntersection of
    Just i -> return ShadingContext ray maybeIntersection
    Nothing -> return ShadingContext ray maybeIntersection

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