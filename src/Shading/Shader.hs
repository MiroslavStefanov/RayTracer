module Shading.Shader where

--import Tracing.Scene
import Shading.Color
import Ray;
import Tracing.Scene
import Shading.PixelState
import Intersection

type Shader = Scene -> Ray -> Intersection -> PixelState

class Shading s where
    shading :: s -> Shader

