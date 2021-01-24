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
import Numeric.Limits
import Shading.Texture

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
      rayToLight = (Vector.add rayStart $ Vector.scale 0.001 rayDirection, rayDirection)

shootRayTowardsLightTracer _ _ = identityTracer ()

cameraRayIntersectionTracer :: Scene -> Ray -> Tracer ShadingValue
cameraRayIntersectionTracer scene ray = do
  maybeIntersection <- getIntersectionTracer
  case maybeIntersection of
    Nothing -> return $ Left white
    Just i -> do
      mapM_ (shootRayTowardsLightTracer i) (lightSources scene)
      return $ Right $ ShadingContext ray i

transformBufferTracer :: (a -> Tracer b) -> FrameBuffer a -> Tracer (FrameBuffer b)
transformBufferTracer func (FrameBuffer w h buff) = let
  newBuffer = mapM func buff in
    FrameBuffer w h <$> newBuffer

generateIntersectionsTracer :: Scene -> Tracer (Int, Int)
generateIntersectionsTracer scene = Tracer $ \(TracingPass i rays) -> Right (calculateNextTracingPass (TracingPass [] $ reverse rays) scene, (Prelude.length i, Prelude.length rays))