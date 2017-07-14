{-# LANGUAGE ScopedTypeVariables #-}
module Camera
       ( Camera
       , mkCamera
       , getRay
       ) where

import Math (Vec3D(Vec3D), Point3D, zeroVector, Ray(Ray), vmap, mkNormVect, normalize, (<*>))

data Camera = Camera
              { origin :: Vec3D
              , lowerLeftCorner :: Vec3D
              , horizontal :: Vec3D
              , vertical :: Vec3D
              }

instance Show Camera where
  show (Camera o llc h v) = "{ origin : " ++ show o ++
                            "\n, lowerLeftCorner : " ++ show llc ++
                            "\n, horizontal : " ++ show h ++
                            "\n, vertical : " ++ show v ++
                            "\n}\n"

mkCamera :: Vec3D -> Point3D -> Vec3D -> Double -> Double -> Camera
mkCamera lookfrom lookat vup vfov aspect = Camera o llc horizontal vertical where
  theta = vfov * pi / 180.0
  halfHeight = tan (theta / 2.0)
  halfWidth = aspect * halfHeight
  o = lookfrom
  w = normalize $ lookfrom - lookat
  u = normalize $ vup Math.<*> w
  v = w Math.<*> u
  llc = o - vmap (* halfWidth) u - vmap (* halfHeight) v - w
  horizontal = vmap (* (2 * halfWidth)) u
  vertical = vmap (* (2 * halfHeight)) v
  -- llc = Vec3D (-halfWidth) (-halfHeight) (-1.0)
  -- h = Vec3D (2.0 * halfWidth) 0.0 0.0
  -- v = Vec3D 0.0 (2.0 * halfHeight) 0.0

getRay :: Camera -> (Double, Double) -> Ray
getRay camera (y, x) = Ray (origin camera) direction where
  mappedHorizontal = vmap (* x) (horizontal camera)
  mappedVertical = vmap (* y) (vertical camera)
  direction = lowerLeftCorner camera + mappedHorizontal + mappedVertical - origin camera

-- camera, start, horizontal, vertical :: Point3D
-- camera = Vec3D 0.0 0.0 0.0
-- start = Vec3D (-2.0) (-1.0) (-1.0)
-- horizontal = Vec3D 4.0 0.0 0.0
-- vertical = Vec3D 0.0 2.0 0.0

-- getRay :: (Double, Double) -> Ray
-- getRay = Ray camera . getPoint
--   where
--     getPoint (y, x) = pointAt start (horiz x) (verti y)
--     pointAt (Vec3D xs ys zs) (Vec3D xh _ _) (Vec3D _ yw _) = Vec3D (xs + xh) (ys + yw) zs
--     horiz x = vmap (* x) horizontal
--     verti y = vmap (* y) vertical
