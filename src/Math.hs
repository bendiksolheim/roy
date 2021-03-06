module Math where

import Random

type Scalar  = Double
data Vec3D = Vec3D Scalar Scalar Scalar deriving (Eq)
type Point2D = (Int, Int)
type Point3D = Vec3D
type Normal = Vec3D
type Axis = Vec3D
type Angle = Scalar
type Color = Vec3D

data Ray = Ray
           { origin :: Point3D
           , direction :: Vec3D }
           deriving (Eq, Show)

type Resolution = (Int, Int)
type Dimension  = (Int, Int)

epsilon :: Scalar
epsilon = 10 ** (-9)

zeroVector :: Vec3D
zeroVector = Vec3D 0.0 0.0 0.0

instance Show Vec3D where
  show (Vec3D x y z) = "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]"

instance Num Vec3D where
  (Vec3D x1 y1 z1) + (Vec3D x2 y2 z2) = Vec3D (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3D x1 y1 z1) - (Vec3D x2 y2 z2) = Vec3D (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3D x1 y1 z1) * (Vec3D x2 y2 z2) = Vec3D (x1 * x2) (y1 * y2) (z1 * z2)
  negate (Vec3D x y z) = Vec3D (-x) (-y) (-z)
  abs (Vec3D x y z) = Vec3D (abs x) (abs y) (abs z)
  signum (Vec3D x y z) = Vec3D (signum x) (signum y) (signum z)
  fromInteger n = let x = fromInteger n in Vec3D x x x

vmap :: (Scalar -> Scalar) -> Vec3D -> Vec3D
vmap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

vmapM :: Monad m => (Scalar -> Scalar) -> Vec3D -> m Vec3D
vmapM f v = return $ vmap f v

-- Dot product
(<.>) :: Vec3D -> Vec3D -> Scalar
(Vec3D x1 y1 z1) <.> (Vec3D x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Cross product
(<*>) :: Vec3D -> Vec3D -> Vec3D
(Vec3D x1 y1 z1) <*> (Vec3D x2 y2 z2) = Vec3D (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

len :: Vec3D -> Scalar
len v = sqrt $ v <.> v

normalize :: Vec3D -> Vec3D
normalize v
    | len v < epsilon = zeroVector
    | otherwise = vmap (/ len v) v

mkNormVect :: Point3D -> Point3D -> Vec3D
mkNormVect u v = normalize $ u - v

dist :: Vec3D -> Vec3D -> Scalar
dist u v = len $ u - v

clip :: Vec3D -> Vec3D
clip = vmap (max 0.0 . min 1.0)

solveQ :: Vec3D -> [Scalar]
solveQ (Vec3D a b c)
    | d < 0 = []
    | d > 0 = [(-b - sqrt d) / (2.0 * a), (-b + sqrt d) / (2.0 * a)]
    | otherwise = [-b / (2.0 * a)]
  where
    d = b * b - 4.0 * a * c

mkRay :: Point3D -> Point3D -> Ray
mkRay p1 p2 = Ray p1 (mkNormVect p1 p2)

reflect :: Vec3D -> Vec3D -> Vec3D
reflect v n = v - vmap (* (2 * (v <.> n))) n

refract :: Vec3D -> Vec3D -> Scalar -> Maybe Vec3D
refract v n r =
  if discriminant > 0.0
  then Just $ vmap (* r) (uv - vmap (* dt) n) - vmap (* sqrt discriminant) n
  else Nothing
  where
    uv = normalize v
    dt = uv <.> n
    discriminant = 1.0 - r * r * (1.0 - dt * dt)

schlick :: Double -> Double -> Double
schlick cosine idx = r0 + (1.0 - r0) * ((1.0 - cosine) ** 5)
  where
    r = (1.0 - idx) / (1.0 + idx)
    r0 = r * r

randomInUnitSphere :: Rand Vec3D
randomInUnitSphere = do
    x <- getDouble
    y <- getDouble
    z <- getDouble
    let p = vmap (* 2.0) (Vec3D x y z) - Vec3D 1.0 1.0 1.0
    if p <.> p >= 10
    then randomInUnitSphere
    else return p
