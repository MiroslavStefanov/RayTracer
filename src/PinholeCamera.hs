module PinholeCamera where

import Ray
import Base
import qualified Vector as Vec
import Tracing.Tracer

data PinholeCamera = PinholeCamera {
  topLeft :: Vec.Vector,
  bottomLeft :: Vec.Vector,
  topRight :: Vec.Vector,
  position :: Vec.Vector,
  target :: Vec.Vector,
  up :: Vec.Vector,
  fov :: Float,
  aspectRatio :: Float
}

data Perspective = Perspective {
  camera :: PinholeCamera,
  width :: Int,
  height :: Int
}

createPerspective :: Vec.Vector -> Vec.Vector -> Vec.Vector -> Float -> Int -> Int -> Perspective
createPerspective pos target up fov width height =
  Perspective (prepareCamera pos target up fov (fromIntegral width / fromIntegral height)) width height

prepareCamera :: Vec.Vector -> Vec.Vector -> Vec.Vector -> Float -> Float -> PinholeCamera
prepareCamera pos target up fov aspRatio =
  PinholeCamera topLeft bottomLeft topRight pos target normUp fov aspRatio
    where normUp = Vec.normalize up
          forward = Vec.normalize (Vec.subtract target pos)
          left = Vec.normalize (Vec.cross normUp forward)
          leftFactor = tan (fov / 2)
          upFactor = leftFactor / aspRatio
          middleLeft = Vec.scale leftFactor left
          middleRight = Vec.scale (-leftFactor) left
          middleUp = Vec.scale upFactor normUp
          middleDown = Vec.scale (-upFactor) normUp
          topLeft = Vec.add (Vec.add middleLeft middleUp) forward
          topRight = Vec.add (Vec.add middleRight middleUp) forward
          bottomLeft = Vec.add (Vec.add middleLeft middleDown) forward


getRay :: Texel -> PinholeCamera -> Ray
getRay (xInterp, yInterp) (PinholeCamera topLeft
                                         bottomLeft
                                         topRight
                                         pos
                                         _
                                         _
                                         _
                                         _ ) = (start, direction)
  where start = pos
        xx = Vec.scale xInterp (Vec.subtract topRight topLeft)
        yy = Vec.scale yInterp (Vec.subtract bottomLeft topLeft)
        direction = Vec.normalize (Vec.add (Vec.add topLeft xx) yy)

shootCameraRayTracer :: PinholeCamera  -> Texel -> Tracer Ray
shootCameraRayTracer camera texel = let
  cameraRay = getRay texel camera
  in do
    shootRayTracer cameraRay
    return cameraRay