module Math where

-- import Types

-- epsilon :: Scalar
-- epsilon = 10 ** (-9)

-- (<+>) :: Vector -> Vector -> Vector
-- (x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- (<->) :: Vector -> Vector -> Vector
-- (x1, y1, z1) <-> (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- (<*>) :: Vector -> Vector -> Vector
-- (x1, y1, z1) <*> (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

-- (*>>) :: Vector -> Scalar -> Vector
-- (x, y, z) *>> f = (x * f, y * f, z * f)

-- maxF :: Double -> Vector -> Vector
-- maxF f (x, y, z) = (max x f, max y f, max z f)

-- minF :: Double -> Vector -> Vector
-- minF f (x, y, z) = (min x f, min y f, min z f)

-- (*.) :: Vector -> Vector -> Double
-- (x1, y1, z1) *. (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- len :: Vector -> Double
-- len v = sqrt (v *. v)

-- normalize :: Vector -> Vector
-- normalize v
--   | len v < epsilon = (0.0, 0.0, 0.0)
--   | otherwise = v *>> (1 / len v)

-- mkNormalizedVect :: Point3D -> Point3D -> Vector
-- mkNormalizedVect u v = normalize $ u <-> v

-- distance :: Point3D -> Point3D -> Double
-- distance u v = sqrt $ (v <-> u) *. (u <-> v)

-- clip :: Vector -> Vector
-- clip = maxF 0.0 . minF 1.0
