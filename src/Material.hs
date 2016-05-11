module Material where

import Math
import Surface
import Random

data Material = Lamberian Vec3D
data Scattered = Scattered
                 { scattered :: Ray
                 , attenuation :: Vec3D
                 }

scatter :: Ray -> Intersection -> Material -> Rand (Maybe Scattered)
scatter _ int (Lamberian albedo) = do
  randomVector <- randomInUnitSphere
  let target = intPoint int + intGetNormal (intPoint int) int + randomVector
  let s = Ray (intPoint int) (target - intPoint int)
  return $ Just (Scattered s albedo)
