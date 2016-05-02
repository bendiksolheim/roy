module Camera
       ( getRay
       ) where

import Math

camera, start, horizontal, vertical :: Point3D
camera = Vec3D 0.0 0.0 0.0
start = Vec3D (-2.0) (-1.0) (-1.0)
horizontal = Vec3D 4.0 0.0 0.0
vertical = Vec3D 0.0 2.0 0.0

getRay :: (Double, Double) -> Ray
getRay = Ray camera . getPoint
  where
    getPoint (y, x) = pointAt start (horiz x) (verti y)
    pointAt (Vec3D xs ys zs) (Vec3D xh _ _) (Vec3D _ yw _) = Vec3D (xs +xh) (ys + yw) zs
    horiz x = vmap (* x) horizontal
    verti y = vmap (* y) vertical
