module Main where

import Lib
import Geometry
import Tracing.Scene
import Tracing.Mesh ( Mesh(Mesh) )
import qualified LightSource as LS
import Shading.Color
import Shading.Texture
import Shading.Sampler
import PinholeCamera ( Perspective, createPerspective )
import Control.Monad.Zip (MonadZip(mzipWith))
import Control.Monad.Loops (whileM_)
import Control.Monad (unless)

wallTexture :: Sampler Rgb -> Texture
wallTexture colorSampler = Texture colorSampler (constantSampler 1) 0 8 PhongMaterial

walls :: [Geometry]
walls = [
  Plane (0, 0, 0) (0, 0, 1), --bottom
  Plane (0, 0, 45) (0, 0, -1), --top
  Plane (-20, 0, 0) (1, 0, 0), --left
  Plane (20, 0, 0) (-1, 0, 0), --right
  Plane (0, 80, 0) (0, -1, 0) --front
  ]

wallMeshes :: [Mesh]
wallMeshes = zipWith Mesh walls $ map wallTexture wallSamplers where
  wallSamplers = [
    checkerSampler white (Rgb 0 0 0) 3,
    constantSampler $ Rgb 0 1 0,
    constantSampler $ Rgb 1 0 0,
    constantSampler $ Rgb 0 0 1,
    constantSampler $ Rgb 0.46 0.36 0.66
    ]

scene1 :: Scene
scene1 = scene
  where 
    sphere = Sphere (5, 30, 6) 6
    sphereTexture = Texture (constantSampler (Rgb 1 1 1)) (constantSampler 1) 6 10 $ ReflectiveMaterial 0.75
    meshSphere = Mesh sphere sphereTexture
    light = LS.PointLight 300 (Rgb 1 0 0) (10, 4, 18)
    light2 = LS.AmbientLight 0.32 (Rgb 1 1 1)
    s1 = Scene [] []
    s2 = foldl addMesh s1 wallMeshes
    s3 = addMesh s2 meshSphere
    s4 = addLightSrc s3 light
    scene = addLightSrc s4 light2
  
scene2 :: Scene
scene2 = scene
  where 
    sphere = Sphere (5, 30, 6) 6
    triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 50, 27)
    triangleTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) 0 12 PhongMaterial
    sphereTexture = Texture (constantSampler $ Rgb 0.37 0.42 0.44) (constantSampler 0.3) 1 10 $ FresnelMaterial 1.5 0.9
    meshSphere = Mesh sphere sphereTexture
    meshTriangle = Mesh triangle triangleTexture
    light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
    light2 = LS.AmbientLight 0.32 (Rgb 1 1 1)
    s1 = Scene [] []
    s2 = foldl addMesh s1 wallMeshes
    s3 = addMesh s2 meshSphere
    s4 = addLightSrc s3 light
    s5 = addLightSrc s4 light2
    scene = addMesh s5 meshTriangle

scene3 :: Scene
scene3 = scene
  where 
    torus = Torus (5, 25, 6) 3 2
    torusTexture = Texture (constantSampler (Rgb 1 1 0)) (constantSampler 1) 0 8 PhongMaterial
    meshTorus = Mesh torus torusTexture
    light = LS.PointLight 900 (Rgb 1 1 1) (10, 20, 30)
    s1 = Scene [] []
    s2 = foldl addMesh s1 wallMeshes
    s3 = addMesh s2 meshTorus
    scene = addLightSrc s3 light

scene4 :: Scene
scene4 = scene
  where 
    obb = Parallelepiped (4, 13, 12) (1,0,0) (0,1,0) (0,0,1) (1.8,2,2.5)
    cone = Cone (-5, 35, 2) 5 15
    --triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 33, 27)
    obbTexture = Texture (constantSampler (Rgb 0.9 0.8 0.6)) (constantSampler 1) 0 8 PhongMaterial
    coneTexture = Texture (checkerSampler (Rgb 0 1 0) (Rgb 0.4 0.2 1) 0.03) (constantSampler 1) 0 8 $ ReflectiveMaterial 0.2
    --coneTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) 6 10 PhongMaterial
    --triangleTexture = FrenselTexture 1.33
    meshObb = Mesh obb obbTexture
    --meshTriangle = makeMesh triangle triangleTexture
    meshCone = Mesh cone coneTexture
    light = LS.PointLight 630 (Rgb 1 1 1) (-2, 40, 30)
    light2 = LS.PointLight 550 (Rgb 1 0.7 1) (0, 0, 30)
    s1 = Scene [] []
    s2 = foldl addMesh s1 wallMeshes
    s3 = addMesh s2 meshObb
    s4 = addLightSrc s3 light
    s5 = addMesh s4 meshCone
    scene = addLightSrc s5 light2

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

sampleScenes :: [Scene]
sampleScenes = [scene1, scene2, scene3, scene4]

switchScenes :: [Scene] -> Perspective -> Int -> Int -> IO Bool
switchScenes scenes p index offset = let
  len = length scenes
  i = (index + offset + len) `mod` len
  in do
    cmd <- debugRenderScene (scenes !! i) p
    if cmd == 0 then return False
    else switchScenes scenes p i cmd

main :: IO ()
main = do
  putStrLn "Enter image width"
  imageWidth <- (readLn :: IO Int)
  putStrLn "Enter image height"
  imageHeight <- (readLn :: IO Int)

  let p = perspective imageWidth imageHeight in
    whileM_ (switchScenes sampleScenes p 0 0) $ return()
