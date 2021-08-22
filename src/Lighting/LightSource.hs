module Lighting.LightSource where

import Ray
import Intersection
import Shading.Texture
import Shading.Color


data ShadowCheck = NoCheck | Ray Ray

type Occlusion = Intersection -> ShadowCheck
type Lighting = Ray -> Intersection -> Texture -> Rgb

class LightSource s where
    lighting :: s -> Lighting
    occlusion :: s -> Occlusion