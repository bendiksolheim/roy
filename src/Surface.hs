{-# LANGUAGE ExistentialQuantification #-}

module Surface where

import Math

class Object o where
  -- get closest intersection point, if any
  intersect :: Ray -> o -> [Scalar]
  -- compute unit normal at point
  getNormal :: Point3D -> o -> Vec3D

data ObjectW = forall o. Object o => OW o

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
