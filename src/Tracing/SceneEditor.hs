module Tracing.SceneEditor where

import qualified Tracing.Scene as Scene
import Tracing.Mesh
import Geometry
import Shading.Texture
import LightSource

addMesh :: Scene.Scene -> IO Scene.Scene
addMesh scene = do
  putStrLn "Enter geometry"
  geometry <- (readLn :: IO Geometry)
  putStrLn "Enter texture"
  texture <- (readLn :: IO Texture)
  return $ Scene.addMesh scene $ Mesh geometry texture

addLight :: Scene.Scene -> IO Scene.Scene
addLight scene = do
  putStrLn "Enter light"
  light <- (readLn :: IO LightSource)
  return $ Scene.addLightSrc scene light