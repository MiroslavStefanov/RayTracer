module Main where

import Lib
import Geometry
import Tracing.Scene (addLightSrc,  Scene(Scene) )
import Tracing.Mesh ( Mesh(Mesh) )
import qualified LightSource as LS
import Shading.Color
import Shading.Texture
import Shading.Sampler
import PinholeCamera ( Perspective, createPerspective )
import Control.Monad.Zip (MonadZip(mzipWith))


scene1 :: Scene
scene1 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    sphere = Sphere (5, 30, 6) 6
    bottomPlaneTexture = Texture (checkerSampler white (Rgb 0 0 0) 3) (constantSampler 1) $ PhongMaterial 0 8
    topPlaneTexture =  Texture (constantSampler $ Rgb 0 1 0) (constantSampler 1) $ PhongMaterial 0 8
    leftPlaneTexture = Texture (constantSampler $ Rgb 1 0 0) (constantSampler 1) $ PhongMaterial 0 8
    rightPlaneTexture = Texture (constantSampler $ Rgb 0 0 1) (constantSampler 1) $ PhongMaterial 0 8
    sphereTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) $ PhongMaterial 6 10
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshSphere = Mesh sphere sphereTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    light2 = LS.PointLight 650 (Rgb 1 0 1) (5, 20, 30)
    scene = addLightSrc (Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshSphere] []) light
  
scene2 :: Scene
scene2 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    sphere = Sphere (5, 30, 6) 6
    triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 50, 27)
    bottomPlaneTexture = Texture (checkerSampler white (Rgb 0 0 0) 3) (constantSampler 1) $ PhongMaterial 1 1
    topPlaneTexture =  Texture (constantSampler $ Rgb 0 1 0) (constantSampler 1) $ PhongMaterial 0 8
    leftPlaneTexture = Texture (constantSampler $ Rgb 1 0 0) (constantSampler 1) $ PhongMaterial 0 8
    rightPlaneTexture = Texture (constantSampler $ Rgb 0 0 1) (constantSampler 1) $ PhongMaterial 0 8
    sphereTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) $ PhongMaterial 3 100
    triangleTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) $ PhongMaterial 3 100
    waterTexture = Texture (constantSampler $ Rgb 0 0.4 0.6) (constantSampler 0) $ FresnelMaterial 1.5 0.8
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshSphere = Mesh sphere waterTexture
    meshTriangle = Mesh triangle triangleTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    scene = addLightSrc (Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshSphere, meshTriangle] []) light

scene3 :: Scene
scene3 = scene
  where 
    bottomPlane = Plane (0, 0, 0) (0, 0, 1)
    topPlane = Plane (0, 0, 45) (0, 0, -1)
    leftPlane = Plane (-20, 0, 0) (1, 0, 0)
    rightPlane = Plane (20, 0, 0) (-1, 0, 0)
    torus = Torus (5, 25, 6) 3 2
    bottomPlaneTexture = Texture (checkerSampler white (Rgb 0 0 0) 3.0) (constantSampler 1)  $ PhongMaterial  0 8
    topPlaneTexture = Texture (constantSampler (Rgb 0 1 0)) (constantSampler 1) $ PhongMaterial 0 8
    leftPlaneTexture = Texture (constantSampler (Rgb 1 0 0)) (constantSampler 1) $ PhongMaterial 0 8
    rightPlaneTexture = Texture (constantSampler (Rgb 0 0 1)) (constantSampler 1) $ PhongMaterial 0 8
    torusTexture = Texture (constantSampler (Rgb 1 1 0)) (constantSampler 1) $ PhongMaterial 0 8
    --torusTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 1)) 0 8
    --triangleTexture = FrenselTexture 1.33
    meshBottomPlane = Mesh bottomPlane bottomPlaneTexture
    meshTopPlane = Mesh topPlane topPlaneTexture
    meshLeftPlane = Mesh leftPlane leftPlaneTexture
    meshRightPlane = Mesh rightPlane rightPlaneTexture
    meshTorus = Mesh torus torusTexture
    --meshTriangle = makeMesh triangle triangleTexture
    light = LS.PointLight 900 (Rgb 1 1 1) (10, 20, 30)
    scene = addLightSrc (Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshTorus] []) light    

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
    bottomPlaneTexture = Texture (checkerSampler white (Rgb 0 0 0) 3.0) (constantSampler 1)  $ PhongMaterial  0 8
    topPlaneTexture = Texture (constantSampler (Rgb 0 1 0)) (constantSampler 1) $ PhongMaterial 0 8
    leftPlaneTexture = Texture (constantSampler (Rgb 1 0 0)) (constantSampler 1) $ PhongMaterial 0 8
    rightPlaneTexture = Texture (constantSampler (Rgb 0 0 1)) (constantSampler 1) $ PhongMaterial 0 8
    obbTexture = Texture (constantSampler (Rgb 1 1 0)) (constantSampler 1) $ PhongMaterial 0 8
    --obbTexture = NormalTexture
    coneTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) $ PhongMaterial 6 10
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
    scene = addLightSrc (Scene [meshBottomPlane, meshTopPlane, meshLeftPlane, meshRightPlane, meshCone] []) light

-- scene6 :: Scene
-- scene6 = scene
--   where
--     torus = Torus (5, 25, 6) 6 4
--     torusTexture = ColorTexture $ ConstantColorSampler (Rgb 1 1 0)
--     --torusTexture = PhongTexture (ConstantColorSampler (Rgb 0 1 1)) 0 8
--     meshTorus = Mesh torus torusTexture
--     light = LS.PointLight 100 (Rgb 1 1 1) (10, 20, 30)
--     scene = Scene [meshTorus] [light]     

perspective :: Int -> Int -> Perspective
perspective = createPerspective (0, 0, 10) (0, 1, 10) (0, 0, 1) (pi/2.5)

scenes :: [Scene]
scenes = [scene1, scene2, scene3, scene4]

main :: IO ()
main = do
  putStrLn "Enter image width"
  imageWidth <- (readLn :: IO Int)
  putStrLn "Enter image height"
  imageHeight <- (readLn :: IO Int)
  mapM_ (`debugRenderScene` perspective imageWidth imageHeight) scenes
