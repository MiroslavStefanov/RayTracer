module Main where

import Lib
import Geometry
import Tracing.Scene
import Tracing.Mesh ( Mesh(Mesh) )
import Shading.Color
import Shading.Texture
import Shading.Sampler
import PinholeCamera ( Perspective, createPerspective )
import Control.Monad.Zip (MonadZip(mzipWith))
import Control.Monad.Loops (whileM_)
import Control.Monad ( unless, replicateM, replicateM_ )
import Shading.Shader
import Shading.ColorShader
import Shading.ReflectionShader
import Shading.PhongShader
import Shading.CompositeShader
import Shading.FresnelShader
import SceneBuilder

import Lighting.PointLight
import Lighting.AmbientLight

import System.Random
import Data.Vector.Unboxed ()
import qualified Vector

-- wallTexture :: Sampler Rgb -> Texture
-- wallTexture colorSampler = Texture colorSampler (constantSampler 1) 0 8 PhongMaterial

-- shaders :: [Shader]
-- shaders = [whiteShader, reflectiveShader] where
--   whiteShader = shading $ ColorShader white
--   reflectiveShader = shading ReflectionShader

addShader :: Shading s => s -> [Shader] -> ([Shader], Int)
addShader shader shaders = (newShaders, index) where
  index = length shaders
  newShaders = shading shader : shaders


walls :: [Geometry]
walls = [
  Plane (0, 0, 0) (0, 0, 1), --bottom
  Plane (0, 0, 45) (0, 0, -1), --top
  Plane (-20, 0, 0) (1, 0, 0), --left
  Plane (20, 0, 0) (-1, 0, 0), --right
  Plane (0, 80, 0) (0, -1, 0) --front
  ]

-- wallMeshes :: ([Mesh], [Shader])
-- wallMeshes = (meshes, shaders) where
--   meshes = zipWith Mesh walls shaderIds

--   map wallShaders wallSamplers
--   wallShaders color = addShader
--   wallSamplers = [
--     white,
--     Rgb 0 1 0,
--     Rgb 1 0 0,
--     Rgb 0 0 1,
--     Rgb 0.46 0.36 0.66
--     ]

-- scene2 :: Scene
-- scene2 = scene
--   where 
--     sphere1 = Sphere (7, 30, 6) 6
--     sphere1Texture = Texture (constantSampler $ Rgb 0.37 0.42 0.44) (constantSampler 0.3) 1 10 $ FresnelMaterial 1.5 0.9
--     sphere2 = Sphere (-6, 20, 6) 6
--     sphere2Texture = Texture (constantSampler (Rgb 1 1 1)) (constantSampler 1) 6 10 $ ReflectiveMaterial 0.75
--     triangle = Triangle (-15, 40, 0) (15, 60, 0) (0, 50, 27)
--     triangleTexture = Texture (checkerSampler (Rgb 1 0 0) (Rgb 0 0 1) 0.03) (constantSampler 1) 0 12 PhongMaterial
--     meshSphere1 = Mesh sphere1 sphere1Texture
--     meshSphere2 = Mesh sphere2 sphere2Texture
--     meshTriangle = Mesh triangle triangleTexture
--     -- light = LS.PointLight 650 (Rgb 1 1 1) (10, 20, 30)
--     -- light2 = LS.AmbientLight 0.32 (Rgb 1 1 1)
--     s1 = Scene [] []
--     s2 = foldl addMesh s1 wallMeshes
--     s3 = addMesh s2 meshSphere1
--     s4 = addLightSrc s3 light
--     s5 = addLightSrc s4 light2
--     s6 = addMesh s5 meshTriangle
--     scene = addMesh s6 meshSphere2

-- scene3 :: Scene
-- scene3 = scene
--   where 
--     torus = Torus (5, 25, 6) 3 2
--     torusTexture = Texture (constantSampler (Rgb 1 1 0)) (constantSampler 1) 0 8 PhongMaterial
--     meshTorus = Mesh torus torusTexture
--     -- light = LS.PointLight 900 (Rgb 1 1 1) (10, 20, 30)
--     s1 = Scene [] []
--     s2 = foldl addMesh s1 wallMeshes
--     s3 = addMesh s2 meshTorus
--     scene = addLightSrc s3 light

-- scene4 :: Scene
-- scene4 = scene
--   where 
--     obb = Parallelepiped (4, 13, 12) (1,0,0) (0,1,0) (0,0,1) (1.8,2,2.5)
--     cone = Cone (-5, 35, 2) 5 15
--     obbTexture = Texture (constantSampler (Rgb 0.9 0.8 0.6)) (constantSampler 1) 0 8 PhongMaterial
--     coneTexture = Texture (checkerSampler (Rgb 0 1 0) (Rgb 0.4 0.2 1) 0.03) (constantSampler 1) 0 8 $ ReflectiveMaterial 0.2
--     meshObb = Mesh obb obbTexture
--     meshCone = Mesh cone coneTexture
--     -- light = LS.PointLight 630 (Rgb 1 1 1) (-2, 40, 30)
--     -- light2 = LS.PointLight 550 (Rgb 1 0.7 1) (0, 0, 30)
--     s1 = Scene [] []
--     s2 = foldl addMesh s1 wallMeshes
--     s3 = addMesh s2 meshObb
--     s4 = addLightSrc s3 light
--     s5 = addMesh s4 meshCone
--     scene = addLightSrc s5 light2

walls2 :: [Geometry]
walls2 = [
  Plane (0, 0, 0) (0, 0, 1), --bottom
  Plane (0, 0, 45) (0, 0, -1), --top
  Plane (-20, 0, 0) (1, 0, 0), --left
  Plane (20, 0, 0) (-1, 0, 0), --right
  Plane (0, 80, 0) (0, -1, 0) --front
  ]

-- wallMeshes2 :: [Mesh]
-- wallMeshes2 = zipWith Mesh walls2 $ map wallTexture wallSamplers where
--   wallSamplers = [
--     constantSampler $ Rgb 1 0.894 1,
--     constantSampler $ Rgb 1 0.894 1,
--     constantSampler $ Rgb 0.941 0.5 0.5,
--     constantSampler $ Rgb 0.392 0.584 0.929,
--     constantSampler $ Rgb 1 0.894 1
--     ]

-- scene5 :: Scene
-- scene5 = scene
--   where
--     s1 = Scene [] []
--     s2 = foldl addMesh s1 wallMeshes2
--     -- light = LS.PointLight 650 white (10, 20, 30)
--     -- light2 = LS.AmbientLight 0.32 white
--     s3 = addLightSrc (addLightSrc s2 light) light2
--     triangle = Triangle (0, 52, 44) ((-4) * sqrt 3, 64, 44) (4 * sqrt 3, 64, 44)
--     triangleTexture = Texture (checkerSampler (Rgb 0 1 0) (Rgb 0.4 0.2 1) 0.03) (constantSampler 1) 0 8 $ ReflectiveMaterial 0.2
--     meshTriangle = Mesh triangle triangleTexture
--     sphere1 = Sphere (5, 20, 5) 5
--     sphere1Texture = Texture (constantSampler $ Rgb 0.37 0.42 0.44) (constantSampler 0.3) 1 10 $ FresnelMaterial 1.5 0.9
--     meshSphere1 = Mesh sphere1 sphere1Texture
--     sphere2 = Sphere (-4, 40, 5) 5
--     sphere2Texture = Texture (constantSampler $ Rgb 0.37 0.42 0.44) (constantSampler 0.3) 1 10 $ FresnelMaterial 1.5 0.9
--     meshSphere2 = Mesh sphere2 sphere2Texture
--     s4 = addMesh s3 meshTriangle
--     s5 = addMesh s4 meshSphere1
--     scene = addMesh s5 meshSphere2

smallBalls :: [Geometry]
smallBalls = [
  Sphere (0, 20, 1) 1,
  Sphere (3, 21, 1) 1,
  Sphere (6, 23, 1) 1,
  Sphere (9, 21, 1) 1,
  Sphere (12, 23, 1) 1,
  Sphere (0, 30, 1) 1,
  Sphere (3, 31, 1) 1,
  Sphere (6, 33, 1) 1,
  Sphere (9, 31, 1) 1,
  Sphere (12, 33, 1) 1,
  Sphere (-3, 30, 1) 1,
  Sphere (-6, 33, 1) 1,
  Sphere (-9, 36, 1) 1,
  Sphere (-12, 40, 1) 1,
  Sphere (9, 40, 1) 1,
  Sphere (-3, 20, 1) 1,
  Sphere (-6, 23, 1) 1,
  Sphere (-9, 26, 1) 1,
  Sphere (-12, 20, 1) 1,
  Sphere (9, 20, 1) 1,
  Sphere (0, 0, 1) 1,
  Sphere (3, 3, 1) 1,
  Sphere (6, 6, 1) 1,
  Sphere (9, 9, 1) 1,
  Sphere (12, 12, 1) 1,
  Sphere (0, 15, 1) 1,
  Sphere (3, 18, 1) 1,
  Sphere (6, 0, 1) 1,
  Sphere (9, 3, 1) 1,
  Sphere (12, 6, 1) 1,
  Sphere (-3, 9, 1) 1,
  Sphere (-6, 12, 1) 1,
  Sphere (-9, 15, 1) 1,
  Sphere (-12, 18, 1) 1,
  Sphere (9, 21, 1) 1,
  Sphere (-3, 3, 1) 1,
  Sphere (-6, 6, 1) 1,
  Sphere (-9, 9, 1) 1,
  Sphere (-12, 12, 1) 1,
  Sphere (9, 15, 1) 1,
  Sphere (15, 20, 1) 1,
  Sphere (18, 21, 1) 1,
  Sphere (21, 23, 1) 1,
  Sphere (24, 21, 1) 1,
  Sphere (27, 23, 1) 1,
  Sphere (15, 30, 1) 1,
  Sphere (18, 31, 1) 1,
  Sphere (21, 33, 1) 1,
  Sphere (24, 31, 1) 1,
  Sphere (27, 33, 1) 1,
  Sphere (-15, 30, 1) 1,
  Sphere (-18, 33, 1) 1,
  Sphere (-21, 36, 1) 1,
  Sphere (-24, 40, 1) 1,
  Sphere (-27, 40, 1) 1,
  Sphere (-15, 20, 1) 1,
  Sphere (-18, 23, 1) 1,
  Sphere (-21, 26, 1) 1,
  Sphere (-24, 20, 1) 1,
  Sphere (-27, 20, 1) 1
  ]

bigBalls :: [Geometry]
bigBalls = [
  Sphere (-9, 45, 7) 7,
  Sphere (0, 35, 7) 7,
  Sphere (9, 25, 7) 7
  ]

-- phongTexture :: Sampler Rgb -> Texture
-- phongTexture colorSampler = Texture colorSampler (constantSampler 1) 0 8 PhongMaterial

-- fresnelTexture :: Float -> Float -> Sampler Rgb -> Texture
-- fresnelTexture eta p colorSampler = Texture colorSampler (constantSampler 0.3) 1 10 $ FresnelMaterial eta p

-- reflectiveTexture :: Float -> Sampler Rgb -> Texture
-- reflectiveTexture p colorSampler = Texture colorSampler (constantSampler 1) 1 10 $ ReflectiveMaterial p

-- smallBallMeshes :: [Mesh]
-- smallBallMeshes = zipWith Mesh smallBalls $ map phongTexture smallBallSamplers where
--   smallBallSamplers = [
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue',
--     constantSampler yellow,
--     constantSampler cyan,
--     constantSampler magenta,
--     constantSampler silver,
--     constantSampler purple,
--     constantSampler green',
--     constantSampler navy,
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue',
--     constantSampler yellow,
--     constantSampler cyan,
--     constantSampler magenta,
--     constantSampler silver,
--     constantSampler purple,
--     constantSampler green',
--     constantSampler navy,
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue',
--     constantSampler yellow,
--     constantSampler cyan,
--     constantSampler magenta,
--     constantSampler silver,
--     constantSampler purple,
--     constantSampler green',
--     constantSampler navy,
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue',
--     constantSampler yellow,
--     constantSampler cyan,
--     constantSampler magenta,
--     constantSampler silver,
--     constantSampler purple,
--     constantSampler green',
--     constantSampler navy,
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue',
--     constantSampler yellow,
--     constantSampler cyan,
--     constantSampler magenta,
--     constantSampler silver,
--     constantSampler purple,
--     constantSampler green',
--     constantSampler navy,
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue',
--     constantSampler yellow,
--     constantSampler cyan,
--     constantSampler magenta,
--     constantSampler silver,
--     constantSampler purple,
--     constantSampler green',
--     constantSampler navy
--     ]    

-- bigBallMeshes :: [Mesh]
-- bigBallMeshes = zipWith Mesh bigBalls $ map (reflectiveTexture 0.7) bigBallSamplers where
--   bigBallSamplers = [
--     constantSampler red',
--     constantSampler lime,
--     constantSampler blue'
--     ]

walls3 :: [Geometry]
walls3 = [
  Plane (0, 0, 0) (0, 0, 1), --bottom
  Plane (0, 70, 0) (0, -1, 0) --front
  ]

-- wall3Meshes :: [Mesh]
-- wall3Meshes = zipWith Mesh walls3 $ map wallTexture wallSamplers where
--   wallSamplers = [
--     checkerSampler lime cyan 1,
--     constantSampler skyBlue
--     ]  

-- scene1 :: Scene
-- scene1 = scene
--   where
--     s1 = Scene [] []
--     s2 = foldl addMesh s1 wall3Meshes
--     -- light = LS.PointLight 630 (Rgb 1 1 1) (-2, 40, 30)
--     -- light2 = LS.PointLight 550 (Rgb 1 0.7 1) (0, 0, 30)
--     -- s3 = addLightSrc (addLightSrc s2 light) light2
--     s4 = foldl addMesh s2 smallBallMeshes
--     -- s5 = addLightSrc s4 $ LS.AmbientLight 0.3 white
--     scene = foldl addMesh s4 bigBallMeshes

-- scene1 :: Scene
-- scene1 = scene where
--   emptyScene = Scene (SceneNode [][]) []
--   geometry = walls3 ++ smallBalls ++ bigBalls
--   meshes = zipWith makeMesh geometry [0 .. length geometry]
--   scene = foldl addMesh emptyScene meshes

spheres :: [Geometry]
spheres = [sphere1, sphere2, sphere3, sphere4]
  where
    sphere1 = Sphere (-3, 0, -16) 2
    sphere2 = Sphere (-1, -1.5, -12) 2
    sphere3 = Sphere (1.5, -0.5, -18) 3
    sphere4 = Sphere (7, 5, -18) 4

pointLights :: [PointLight]
pointLights = [light1, light2, light3]
  where
    light1 = PointLight 600 white (-20, 20, 20)
    light2 = PointLight 800 white (30, 50, -25)
    light3 = PointLight 700 white (30, 20, 30)

pointLights2 :: [PointLight]
pointLights2 = [light1]
  where
    light1 = PointLight 650 white (10, 20, 30)
    --light2 = PointLight 800 white (30, 50, -25)

checkerPlane :: Geometry
checkerPlane = Plane (0, -4, 0) (0, 1, 0)

checkerTriangle :: Geometry
checkerTriangle = Triangle (0, -4, -60) (-6, -4, -10) (6, -4, -10)

checkerPlaneShader :: PhongShader
checkerPlaneShader = PhongShader $ solidColorCheckerTexture white purple 1

checkerTriangleShader :: PhongShader
checkerTriangleShader = PhongShader $ solidColorCheckerTexture white magenta 0.05

newPerspective :: Int -> Int -> Perspective
newPerspective = createPerspective (0, 0, 0) (0, 0, -1) (0, 1, 0) (pi/3)

ballsScene :: IO (SceneBuilder ())
ballsScene = do
  ballColors <- replicateM 20 getRandomColor
  return $ do
    addMeshesBuilder smallBalls $ \i -> mod i 20
    mapM_ (addShaderBuilder . PhongShader . metalicColorTexture) ballColors
    shader1 <- addShaderBuilder $ composeShaders 0.9 (FresnelShader 1.5) $ PhongShader $ Texture (constantSampler $ Rgb 0.37 0.42 0.44) (constantSampler (10, 1))
    shader2 <- addShaderBuilder $ composeShaders 0.25 ReflectionShader $ PhongShader $ shinyColorCheckerTexture skyBlue yellow 0.06
    addMeshesBuilder bigBalls $ \i -> 20 + mod (i+1) 2


newScene :: IO (SceneBuilder ())
newScene = let
  shaderGenerator index = mod index $ length sampleColors in do
    return $ do
      addMeshesBuilder spheres (+1)
      addMeshBuilder $ makeMesh checkerTriangle 0
      addShaderBuilder checkerTriangleShader
      addShaderBuilder $ PhongShader $ metalicColorTexture silver
      addShaderBuilder $ composeShaders 0.8 (FresnelShader 1.8) $ PhongShader $ metalicColorTexture white
      addShaderBuilder $ PhongShader $ solidColorTexture red'
      addShaderBuilder $ composeShaders 0.8 ReflectionShader $ PhongShader $ metalicColorTexture black
      addLightsBuilder pointLights
      addLightBuilder $ AmbientLight 0.18 white
      return ()

newScene2 :: IO (SceneBuilder ())
newScene2 = let
  shaderGenerator index = mod index $ length sampleColors
  obb = Parallelepiped (3, 3, -13) (1,0,0) (0,1,0) (0,0,1) (1.8,2,2.5)
  cone = Cone (-3, 3.2, -14) 2 9
  sphere = Sphere (-0.5, -0.5, -10) 2 in do
    return $ do
      addMeshBuilder $ makeMesh checkerPlane 0
      addShaderBuilder checkerPlaneShader
      addMeshBuilder $ makeMesh obb 1
      addShaderBuilder $ PhongShader $ metalicColorTexture green'
      addMeshBuilder $ makeMesh cone 2
      addShaderBuilder $ composeShaders 0.2 ReflectionShader $ PhongShader $ metalicColorTexture $ Rgb 0.3 0.4 0.7
      addMeshBuilder $ makeMesh sphere 3
      addShaderBuilder $ composeShaders 0.8 (FresnelShader 1.8) $ PhongShader $ metalicColorTexture white
      addLightsBuilder pointLights
      addLightBuilder $ AmbientLight 0.18 white
      return ()

newPerspective2 :: Int -> Int -> Perspective
newPerspective2 = createPerspective (0, 0, 10) (0, 1, 10) (0, 0, 1) (pi/2.5)

newScene3 :: IO (SceneBuilder ())
newScene3 = do
  ballsBuilder <- ballsScene
  return $ do
    ballsBuilder
    addLightBuilder $ PointLight 630 (Rgb 1 1 1) (-2, 40, 30)
    addLightBuilder $ PointLight 700 (Rgb 1 0.7 1) (0, -20, 30)
    floorShader <- addShaderBuilder $ composeShaders 0.4 ReflectionShader $ PhongShader $ shinyColorCheckerTexture black white 1
    addMeshBuilder $ makeMesh (Plane (0, 0, 0) (0, 0, 1)) floorShader

newPerspective3 :: Int -> Int -> Perspective 
newPerspective3 = createPerspective (-10, -22, 12) (15, 40, 3.5) (0, 0, 1) (pi/3)

-- scene1 :: SceneBuilder ()
-- scene1 = let shaderGenerator index = mod index $ length sampleColors in do
--   colorShaders <- mapM (addShaderBuilder . PhongShader . solidColorTexture) sampleColors
--   --reflectiveShaders <- mapM ((addShaderBuilder . composeShaders 0.25 ReflectionShader) . (PhongShader . solidColorTexture)) sampleColors
--   addMeshesBuilder walls3 shaderGenerator
--   addMeshesBuilder smallBalls shaderGenerator
--   --addMeshesBuilder bigBalls $ \index -> mod index (length reflectiveShaders) + length colorShaders
--   addLightBuilder $ PointLight 630 (Rgb 1 1 1) (-2, 40, 30)
--   addLightBuilder $ PointLight 550 (Rgb 1 0.7 1) (0, 0, 30)
--   addLightBuilder $ AmbientLight 0.3 white
--   return ()

-- scene2 :: IO (SceneBuilder ())
-- scene2 = let
--   shaderGenerator index = mod index $ length sampleColors
--   obb = Parallelepiped (4, 13, 12) (1,0,0) (0,1,0) (0,0,1) (1.8,2,2.5)
--   cone = Cone (-5, 35, 2) 5 15 in do
--     randomColors <- replicateM 10 getRandomColor
--     wallColors <- replicateM 5 getRandomColor
--     return $ do
--       colorShaders <- mapM (addShaderBuilder . PhongShader . solidColorTexture) randomColors
--       reflectiveShaders <- mapM ((addShaderBuilder . composeShaders 0.25 (shading ReflectionShader)) . (shading . PhongShader . solidColorTexture)) randomColors
--       addMeshesBuilder walls $ \index -> mod index $ length wallColors
--       addMeshesBuilder [obb, cone] $ \index -> mod index (length reflectiveShaders) + length colorShaders
--       addLightBuilder $ PointLight 630 (Rgb 1 1 1) (-2, 40, 30)
--       addLightBuilder $ PointLight 700 (Rgb 1 0.7 1) (0, -20, 30)
--       return ()

perspective1 :: Int -> Int -> Perspective
perspective1 = createPerspective (0, 0, 10) (0, 1, 10) (0, 0, 1) (pi/2.5)

perspective2 :: Int -> Int -> Perspective
perspective2 = createPerspective (5, 25, 45) (5, 25, 6) (0, 1, 0) (pi/2.5)

sampleScenes :: [IO ShadingContext]
sampleScenes = [fmap getShadingContext newScene3]

-- switchScenes :: [Scene] -> [Perspective] -> (Int, Int) -> (Int, Int) -> IO Bool
-- switchScenes scenes p (x, y) (dx, dy) = let
--   sCount = length scenes
--   pCount = length p
--   xx = (x + dx + sCount) `mod` sCount
--   yy = (y + dy + pCount) `mod` pCount
--   in do
--     cmd <- debugRenderScene (scenes !! xx) (p !! yy)
--     let
--       newDx = if cmd > 1 then 0 else cmd
--       newDy = if cmd == 2 then 1 else 0
--       in
--         if cmd == 0 then return False
--         else switchScenes scenes p (x, y) (newDx, newDy) 

-- mapMultiple :: [a -> b] -> a -> [b]
-- mapMultiple fs x = map ($ x) fs

switchScenes :: [IO ShadingContext] -> Perspective -> Int -> Int -> IO Bool
switchScenes contexts p index offset = let
  len = length contexts
  i = (index + offset + len) `mod` len
  in do
    ctx <- contexts !! i
    renderScene ctx p 50
    return False
    -- if cmd == 0 then return False
    -- else switchScenes scenes shaders p i cmd

main :: IO ()
main = do
  putStrLn "Enter image width"
  imageWidth <- (readLn :: IO Int)
  putStrLn "Enter image height"
  imageHeight <- (readLn :: IO Int)

  let
    p = newPerspective3 imageWidth imageHeight
    --count = meshesCount scene1 - 1
    in
      whileM_ (switchScenes sampleScenes p 0 0) $ return()
      -- reds <- replicateM count (randomIO :: IO Float)
      -- greens <- replicateM count (randomIO :: IO Float)
      -- blues <- replicateM count (randomIO :: IO Float)
      -- let
      --   --shaders = shading ReflectionShader : map (shading . ColorShader) (zipWith3 Rgb reds greens blues) 
      --   in


  -- let perspectives =  mapMultiple (mapMultiple [perspective1, perspective2] imageWidth) imageHeight in
  --   whileM_ (switchScenes sampleScenes perspectives (0, 0) (0, 0)) $ return()
