module Shading.Texture where

import Shading.Color
import Shading.Sampler

data Material =
  PhongMaterial |
  ReflectiveMaterial Float |
  FresnelMaterial Float Float |
  NoMaterial
  deriving (Show, Read, Eq)

data Texture = Texture {
  diffuseSampler :: Sampler Rgb,
  alphaSampler :: Sampler Float,
  specularMultiplier :: Float,
  specularExponent :: Float,
  material :: Material
}

instance Show Texture where
  show (Texture _ _ mult exp mat) = "Texture{Specular strength = " ++ show mult ++ ", Shininess = " ++ show exp ++ "Material = " ++ show mat ++ "}"

instance Eq Texture where
  (Texture _ _ m1 e1 mat1) == (Texture _ _ m2 e2 mat2) = m1 == m2 && e1 == e2 && mat1 == mat2

emptyTexture :: Texture
emptyTexture = Texture emptyDiffuse emptyAlpha 0 0 NoMaterial where
  emptyDiffuse = Sampler undefined 
  emptyAlpha = Sampler undefined
