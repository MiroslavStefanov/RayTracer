module Shading.Texture where

import Shading.Color

data Texture = 
  InvalidTexture {
    error :: String
  } |
  ColorTexture {
    color :: Rgb
  }
  deriving (Show)