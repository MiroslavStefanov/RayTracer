module Main where

import Lib
import Geometry
import Tracing.Scene
import qualified LightSource as LS
import Shading.Color
import Shading.Texture
import Shading.Sampler
import Base

main :: IO ()
main = do
  putStrLn "Processing traceOutput.png ..."
  case image of
    Right img -> saveImage "traceOutput.png" img
    Left (GeneralError msg) -> putStrLn msg
    where
      camera = makePinholeCamera (3,3,10) (0,0,-1) (0,1,0) (pi/2.5) 1
      sphere = Sphere (4,4,-5) 2
      triangle = Triangle (2, 2, -3) (8, 2, -6) (4, 7, -6)
      plane = Plane (0, 0, -6) (0,0,1)
      sphereTexture = PhongTexture (ConstantColorSampler (Rgb 1 0 0)) 1 50
      triangleTexture = PhongTexture (ConstantColorSampler (Rgb 0 0 1)) 1 1
      planeTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 0)) 1 1
      meshSpere = makeMesh sphere sphereTexture
      meshTriangle = makeMesh triangle triangleTexture
      meshPlane = makeMesh plane planeTexture
      light = LS.PointLight 100 (Rgb 1 1 1) (10, 8, 0)
      scene = Scene [meshSpere, meshPlane, meshTriangle] [light]
      image = traceScene 300 300 camera scene 1

