module Shading.DebugShader where

import Shading.Texture
import Shading.PixelState
import Shading.Shader
import Tracing.Scene
import qualified Shading.Color as Color
import Lighting.LightSource
import Intersection
import Vector (xx, yy, zz)
import Tracing.Mesh

data DebugShader = DebugShader{}

instance Shading DebugShader where
  shading DebugShader scene incommingRay i = Ready totalColor where
    --totalColor = Color.clamp $ foldl Color.add Color.black lightColors
    --totalColor = Color.Rgb x y z
    --totalColor = Color.scale 0.5 $ Color.add Color.white $ Color.Rgb x y z
    --totalColor = Color.clamp $ Color.add (Color.scale 10000 Color.white) $ Color.Rgb x y z
    totalColor = Color.clamp $ Color.scale (1.0/30.0) $ Color.Rgb (zz $ position i) (zz $ position i) (zz $ position i)
    --totalColor = case head shadowChecks of
      --NoCheck -> Color.black
      --Ray (o, (dx, dy, dz)) -> Color.scale 0.5 $ Color.add Color.white $ Color.Rgb dx dy dz
    -- (x, y, z) = normal i
    -- shadowChecks = map applyChecks $ lightSources scene
    -- applyChecks (_, occlusion) = occlusion i
    -- shadowFactors = map getShadowFactor shadowChecks
    -- getShadowFactor NoCheck = 1.0
    -- getShadowFactor (Ray shadowRay) = let
    --     shadowHit = traceRay scene shadowRay
    --     in case shadowHit of
    --         Nothing -> 1.0
    --         -- Just hit -> distance $ intersection hit
    --         --Just (Hit m int) -> fromIntegral $ shaderId m
    --         Just _ -> 0.0

    -- getDebugColor NoCheck = Color.black
    -- getDebugColor (Ray (v, d)) = Color.scale 1.0 $ Color.Rgb (xx d + 0.0) (yy d + 0.0) (zz d + 0.0)
    --getDebugColor (Ray (v, d)) = case traceRay scene (v, d) of
      --Nothing -> Color.black
      --Just hit -> Color.Rgb (distance (intersection hit) / 1000) (distance (intersection hit) / 1000) (distance (intersection hit) / 1000)
      --Just hit -> Color.scale 0.1 $ Color.Rgb (xx $ position $ intersection hit) (yy $ position $ intersection hit) (zz $ position $ intersection hit)
      -- Just hit -> 
    --totalColor = Color.clamp $ foldl Color.add Color.black $ map getDebugColor shadowChecks
    -- getDebugShading NoCheck = Ready Color.black
    -- getDebugShading (Ray shadowRay) = case traceRay scene shadowRay of
    --   Nothing -> Ready Color.black
    --   Just hit -> Shading shadowRay hit
    -- debugShadings = map getDebugShading shadowChecks
    -- getDebugWeight Shading {} = 1.0
    -- getDebugWeight _ = 0.0
    
    

    
