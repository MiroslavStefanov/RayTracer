module Main where

import Lib
import Geometry
import Tracing.Scene ( Scene(Scene) )
import Tracing.Mesh ( Mesh(Mesh) )
import qualified LightSource as LS
import Shading.Color
import Shading.Texture
import Shading.Sampler
import PinholeCamera ( Perspective, createPerspective )


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
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshSphere = Mesh sphere sphereTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    light2 = LS.PointLight 650 (Rgb 1 0 1) (5, 20, 30)
    scene = Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshSphere] [light]
  
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
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshSphere = Mesh sphere sphereTexture
    meshTriangle = Mesh triangle triangleTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    scene = Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshSphere, meshTriangle] [light]

scene3 :: Scene
scene3 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    torus = Torus (5, 60, 6) 3 14
    --triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 33, 27)
    bottomPlaneTexture = PhongTexture (CheckerSampler white (Rgb 0 0 0) 3.0) 0 8
    topPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 0)) 0 8
    leftPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 1 0 0)) 0 8
    rightPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 0 1)) 0 8
    torusTexture = ColorTexture (Rgb 1 1 0)
    --torusTexture = PhongTexture (ConstantColorSampler (Rgb 1 0 1)) 0 8
    --triangleTexture = FrenselTexture 1.33
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshTorus = Mesh torus torusTexture
    --meshTriangle = makeMesh triangle triangleTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    scene = Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshTorus] [light]    

scene4 :: Scene
scene4 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    obb = Parallelepiped (5, 45, 10) (1,0,0) (0,1,0) (0,0,1) (2,2,2)
    cone = Cone (5, 25, 6) 5 15
    --triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 33, 27)
    bottomPlaneTexture = PhongTexture (CheckerSampler white (Rgb 0 0 0) 3.0) 0 8
    topPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 0)) 0 8
    leftPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 1 0 0)) 0 8
    rightPlaneTexture = PhongTexture (ConstantColorSampler (Rgb 0 0 1)) 0 8
    obbTexture = PhongTexture (ConstantColorSampler (Rgb 1 1 0)) 0 8
    --obbTexture = NormalTexture
    coneTexture = PhongTexture (CheckerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) 6 10
    --coneTexture = PhongTexture (ConstantColorSampler (Rgb 1 1 0)) 0 8
    --coneTexture = ColorTexture (Rgb 1 1 0)
    --triangleTexture = FrenselTexture 1.33
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshObb = Mesh obb obbTexture
    --meshTriangle = makeMesh triangle triangleTexture
    meshCone = Mesh cone coneTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    light2 = LS.PointLight 650 (Rgb 1 0 1) (5, 20, 30)
    scene = Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshCone] [light]    

perspective :: Perspective
perspective = createPerspective (0, 0, 10) (0, 1, 10) (0, 0, 1) (pi/2.5) 400 400

main :: IO ()
main = do
  renderScene scene4 perspective 5
  -- exportImage scene1 perspective 3 "traceOutput.png"
  -- exportImage scene2 perspective 5 "traceOutput2.png"