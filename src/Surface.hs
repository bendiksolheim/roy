{-# LANGUAGE ExistentialQuantification #-}

module Surface where

import Math

class Object o where
  -- get closest intersection point, if any
  intersect :: Ray -> o -> [Scalar]
  -- compute unit normal at point
  getNormal :: Point3D -> o -> Vec3D

data AnyObject = forall o. Object o => AnyObject o

data Sphere = Sphere
              { center :: Point3D
              , radius :: Scalar
              } deriving (Eq, Show)

instance Object Sphere where
  intersect (Ray start dir) (Sphere cen rad) = solveQ $ Vec3D a b c
    where
      a = dir <.> dir
      d = start - cen
      b = 2 * (dir <.> d)
      c = (d <.> d) - rad * rad

  getNormal p (Sphere c _) = mkNormVect p c

data Intersection = Intersection
                    { intT :: Double
                    , intPoint :: Point3D
                    , intAnyObject :: AnyObject
                    }

instance Eq Intersection where
  (Intersection t1 _ _) == (Intersection t2  _ _) = t1 == t2

instance Ord Intersection where
  compare (Intersection t1 _ _) (Intersection t2 _ _) = compare t1 t2

intGetNormal :: Point3D -> Intersection -> Vec3D
intGetNormal p (Intersection _ _ (AnyObject o)) = getNormal p o

intersectionPoint :: Ray -> Scalar -> Point3D
intersectionPoint (Ray orig dir) t = orig + vmap (* t) dir

fstPos :: [Scalar] -> Scalar
fstPos [] = 0.0
fstPos (x:xs) = if x > epsilon then x else fstPos xs

intersects :: Ray -> AnyObject -> Maybe Intersection
intersects ray obj@(AnyObject object) =
  if t > epsilon
  then Just (Intersection t (intersectionPoint ray t) obj)
  else Nothing
  where
    t = fstPos $ intersect ray object
