{-# LANGUAGE TupleSections #-}
module Shading.Shader where

import Base
import Tracing.TracingPass
import Ray
import PinholeCamera hiding (position)
import Shading.FrameBuffer
import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Bifunctor (Bifunctor (second))

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




-- identityShader :: a -> Shader a
-- identityShader value pass = (pass, value)


-- indexTexelsShader :: Int -> Int -> Shader FrameBuffer Texel
-- indexTexelsShader width height pass = (pass, createBuffer width height)


-- shootCameraRayShader :: PinholeCamera -> FrameBuffer Texel -> Shader FrameBuffer Ray
-- shootCameraRayShader camera (FrameBuffer w h texels) = do


-- sampleTextureShader :: Ray -> Shader Texture
-- sampleTextureShader incommingRay pass = let
--   intersection = head inputIntersections pass
--   in do 
--     case texture intersection of
--       ColorTexture c -> return ColorTexture c
--       anyT -> return InvalidTexture "Usupported texture type: " ++ show anyT


-- newtype Shader a = Shader{
--   shade :: Texel -> ShadingPass -> (ShadingPass, a)
-- }
