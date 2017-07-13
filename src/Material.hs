module Material where

import Math

data Material = Lambertian Vec3D
              | Metal Vec3D Double
              | Dielectric Double
              deriving (Show)

data Scattered = Scattered
                 { scattered :: Ray
                 , attenuation :: Vec3D
                 }
