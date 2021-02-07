{-# LANGUAGE TupleSections #-}
module Tracing.Tracer where

import Base
import ErrorHandling.ErrorMessages
import Tracing.TracingPass
import Tracing.Scene
import Vector
import Ray
import Intersection
import qualified PinholeCamera as Camera
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

shootCameraRayTracer :: Camera.PinholeCamera  -> Texel -> Tracer ShadingDensity
shootCameraRayTracer camera texel = let
  cameraRay = Camera.getRay texel camera
  cameraIntersection = Intersection (Camera.position camera) (normalize $ Camera.target camera `subtract` Camera.position camera) TransparentTexture 0 (0,0)
  in do
    shootRayTracer cameraRay
    return [(Right $ ShadingContext cameraRay cameraIntersection, 1)]

shootRayTowardsLightTracer :: Intersection -> LS.LightSource -> Tracer ()
shootRayTowardsLightTracer 
  intresection@(Intersection intPosition intNormal _ _ _)
  (LS.PointLight _ _ lightPosition) = 
    shootRayTracer rayToLight where
      rayStart = getShadowRayStartPoint intresection
      rayDirection = normalize $ subtract lightPosition intPosition
      rayToLight = (Vector.add rayStart $ Vector.scale 0.001 rayDirection, rayDirection)

shootRayTowardsLightTracer _ _ = identityTracer ()

shootReflectedRayTracer :: Intersection -> Ray -> Tracer Ray
shootReflectedRayTracer (Intersection pos norm _ _ _) (rayStart, rayDirection) =
  shootRayTracer reflectedRay >> identityTracer reflectedRay where
    reflectDirection = Vector.normalize $ Vector.reflect rayDirection norm
    reflectedRay = (pos, reflectDirection)

transformBufferTracer :: (a -> Tracer b) -> FrameBuffer a -> Tracer (FrameBuffer b)
transformBufferTracer func (FrameBuffer w h buff) = let
  newBuffer = mapM func buff in
    FrameBuffer w h <$> newBuffer

generateIntersectionsTracer :: Scene -> Tracer (Int, Int)
generateIntersectionsTracer scene = Tracer $ \(TracingPass i rays) -> Right (calculateNextTracingPass (TracingPass [] $ reverse rays) scene, (Prelude.length i, Prelude.length rays))
