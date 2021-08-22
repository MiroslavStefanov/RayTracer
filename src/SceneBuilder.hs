{-# LANGUAGE TupleSections #-}
module SceneBuilder where

import Tracing.Scene
import Shading.Shader
import Data.Bifunctor
import Tracing.Mesh
import Lighting.LightSource
import Intersection

data ShadingContext = ShadingContext {
    scene :: Scene,
    shaders :: [Shader]
}

newtype SceneBuilder a = SceneBuilder
  { build :: ShadingContext -> (ShadingContext, a)
  }

instance Functor SceneBuilder where
  fmap mapper builder = SceneBuilder newBuild where
    newBuild context = let
        (newContext, result) = build builder context
        mappedResult = mapper result
        in (newContext, mappedResult)

instance Applicative SceneBuilder where
  pure a = SceneBuilder (,a)
  SceneBuilder fb <*> SceneBuilder vb = SceneBuilder $ \context -> let
    (context', f) = fb context
    (context'', v) = vb context'
    in (context'', f v)

instance Monad SceneBuilder where
  SceneBuilder b >>= func = SceneBuilder $ \context -> 
      let (context', a) = b context in 
          build (func a) context'

getShadingContext :: SceneBuilder a -> ShadingContext 
getShadingContext builder = readyContext where
    (readyContext, _) = build builder emptyContext
    emptyContext = ShadingContext emptyScene []


addShaderBuilder :: Shading s => s -> SceneBuilder Int 
addShaderBuilder shader = SceneBuilder builder where
    builder (ShadingContext scene shaders) = 
        let
            shaderId = length shaders
            newContext = ShadingContext scene (shading shader : shaders)
            in (newContext, shaderId)

addMeshBuilder :: Mesh -> SceneBuilder ()
addMeshBuilder m = SceneBuilder builder where
    builder (ShadingContext scene shaders) = (ShadingContext (addMesh scene m) shaders, ())

addMeshesBuilder :: Intersectable i => [i] -> (Int -> Int) -> SceneBuilder ()
addMeshesBuilder intersectables shaderIdMapper = mapM_ addMeshBuilder $ zipWith makeMesh intersectables [ shaderIdMapper x | x <- [0 .. length intersectables]]


addLightBuilder :: LightSource s => s -> SceneBuilder ()
addLightBuilder source = SceneBuilder builder where
    builder (ShadingContext scene shaders) = (ShadingContext (addLight scene source) shaders, ())

-- getShadingContextBuilder :: SceneBuilder ShadingContext
-- getShadingContextBuilder = SceneBuilder $ \context -> (context, context)
