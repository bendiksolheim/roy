module Material where

import Math

data Material = Lambertian Vec3D
              | Metal Vec3D
data Scattered = Scattered
                 { scattered :: Ray
                 , attenuation :: Vec3D
                 }
