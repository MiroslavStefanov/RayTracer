module Shading.ColorShader where

import Shading.Color
import Shading.Shader
import Shading.PixelState

newtype ColorShader = ColorShader Rgb

instance Shading ColorShader where
    shading (ColorShader color) _ _ _ = Ready color