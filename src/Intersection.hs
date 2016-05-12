module Intersection where

import Math
import Entity
import Surface

data Intersection = Intersection
                    { intT :: Double
                    , intPoint :: Point3D
                    , intEntity :: Entity
                    }

instance Eq Intersection where
  (Intersection t1 _ _) == (Intersection t2  _ _) = t1 == t2

instance Ord Intersection where
  compare (Intersection t1 _ _) (Intersection t2 _ _) = compare t1 t2

intGetNormal :: Point3D -> Intersection -> Vec3D
intGetNormal p (Intersection _ _ (Entity (AnyObject o) _)) = getNormal p o

intersectionPoint :: Ray -> Scalar -> Point3D
intersectionPoint (Ray orig dir) t = orig + vmap (* t) dir

fstPos :: [Scalar] -> Scalar
fstPos [] = 0.0
fstPos (x:xs) = if x > epsilon then x else fstPos xs

intersects :: Ray -> Entity -> Maybe Intersection
intersects ray ent@(Entity (AnyObject object) _) =
  if t > epsilon
  then Just (Intersection t (intersectionPoint ray t) ent)
  else Nothing
  where
    t = fstPos $ intersect ray object
