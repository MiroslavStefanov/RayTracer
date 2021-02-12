{-# LANGUAGE TupleSections #-}
module Tracing.Tracer where

import Base
import ErrorHandling.ErrorMessages
import Tracing.TracingPass
import Vector
import Ray
import Intersection
import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Bifunctor (Bifunctor (second))
import Prelude hiding (subtract)
import Numeric.Limits

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
