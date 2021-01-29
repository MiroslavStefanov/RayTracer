module Main where

import Lib
import Geometry
import Tracing.Scene
import qualified LightSource as LS
import Shading.Color
import Shading.Texture
import Shading.Sampler
import Base
import PinholeCamera

scene1 :: Scene
scene1 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    sphere = Sphere (5, 30, 6) 6
    bottomPlaneTexture = PhongTexture (CheckerSampler white (Rgb 0 0 0) 3.0) 0 8
    topPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 0)) 0 8
    leftPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 1 0 0)) 0 8
    rightPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 0 1)) 0 8
    sphereTexture = PhongTexture (CheckerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) 6 10
    meshBottomPlane = makeMesh bottomPlane bottomPlaneTexture
    meshTopPlane = makeMesh topPlane topPlaneTexture
    meshLeftPlane = makeMesh leftPlane leftPlaneTexture
    meshRightPlane = makeMesh rightPlane rightPlaneTexture
    meshSphere = makeMesh sphere sphereTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    light2 = LS.PointLight 650 (Rgb 1 0 1) (5, 20, 30)
    scene = Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshSphere] [light, light2]
  
scene2 :: Scene
scene2 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    sphere = Sphere (5, 30, 6) 6
    triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 33, 27)
    bottomPlaneTexture = PhongTexture (CheckerSampler white (Rgb 0 0 0) 3.0) 1 1
    topPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 0)) 0 8
    leftPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 1 0 0)) 0 8
    rightPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 0 1)) 0 8
    sphereTexture = PhongTexture (CheckerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) 6 10
    triangleTexture = FrenselTexture 1.33
    meshBottomPlane = makeMesh bottomPlane bottomPlaneTexture
    meshTopPlane = makeMesh topPlane topPlaneTexture
    meshLeftPlane = makeMesh leftPlane leftPlaneTexture
    meshRightPlane = makeMesh rightPlane rightPlaneTexture
    meshSphere = makeMesh sphere sphereTexture
    meshTriangle = makeMesh triangle triangleTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    scene = Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshSphere, meshTriangle] [light]

perspective :: Perspective
perspective = createPerspective (0, 0, 10) (0, 1, 10) (0, 0, 1) (pi/2.5) 400 400

main :: IO ()
main = do
  exportImage scene1 perspective 3 "traceOutput.png"
  exportImage scene2 perspective 5 "traceOutput2.png"