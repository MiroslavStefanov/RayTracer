module LightSource where

import Shading.Color
import Vector

data LightSource = 
    PointLight {
        intensity :: Float,
        color :: Rgb,
        position :: Vector
    } |
    AmbientLight {
        intensity :: Float,
        color :: Rgb,
        position :: Vector
    } deriving (Show, Read, Eq)