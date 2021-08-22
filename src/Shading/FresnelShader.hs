module Shading.FresnelShader where

-- import Shading.Texture
-- import Shading.PixelState
-- import Shading.Shader
-- import Tracing.Scene
-- import qualified Shading.Color as Color

-- newtype FresnelShader = FresnelShader{ eta :: Float }

-- composeShaders :: Float -> Shader -> Shader -> CompositeShader
-- composeShaders alpha first second = CompositeShader [first, second] [alpha, 1.0 - alpha]

-- instance Shading CompositeShader where
--   shading (CompositeShader shadings weights) scene incommingRay intersection = Composition states weights where
--     states = map applyShading shadings
--     applyShading shader = shader scene incommingRay intersection
