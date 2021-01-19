module LightSource where

import Color
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
    }