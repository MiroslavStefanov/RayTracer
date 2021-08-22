module Lighting.AmbientLight where

import Lighting.LightSource
import Shading.Color
import Intersection
import Shading.Sampler
import Shading.Texture

data AmbientLight = AmbientLight Float Rgb deriving (Show, Eq)

instance LightSource AmbientLight where
  lighting (AmbientLight intensity color) (start, direction) intersection texture = 
      clamp $ multiply diffuseColor lightColor where
        lightColor = scale intensity color
        diffuseColor = sample (diffuseSampler texture) $ coordinates intersection
  occlusion AmbientLight {} intersection = NoCheck