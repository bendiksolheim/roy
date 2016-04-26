module Types where

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

data Object = Sphere Scalar Point3D
            | Plane (Scalar, Scalar, Scalar, Scalar)

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
mkNormVect u v = normalize $ v - u

dist :: Vec3D -> Vec3D -> Scalar
dist u v = len $ u - v

clip :: Vec3D -> Vec3D
clip = vmap (max 0.0 . min 1.0)

solveQ :: Vec3D -> [Scalar]
solveQ (Vec3D a b c)
    | d < 0 = []
    | d > 0 = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
    | otherwise = [-b / (2 * a)]
  where
    d = b * b - 4 * a * c

mkRay :: Point3D -> Point3D -> Ray
mkRay p1 p2 = Ray p1 (mkNormVect p1 p2)

intersect :: Ray -> Object -> [Scalar]
intersect (Ray start dir) (Sphere rad center) = solveQ $ Vec3D a b c
  where
    a = dir <.> dir
    d = start - center
    b = 2 * (dir <.> d)
    c = (d <.> d) - rad * rad

intersect (Ray start dir) (Plane (a, b, c, d)) =
    if abs part < epsilon
    then []
    else [- (d + (Vec3D a b c <.> start)) / part]
  where
    part = Vec3D a b c <.> dir

normal :: Point3D -> Object -> Normal
normal p (Sphere rad center) = normalize $ vmap (/ rad) $ p - center
normal _ (Plane (a, b, c, _)) = normalize $ Vec3D a b c

reflect :: Vec3D -> Vec3D -> Vec3D
reflect i n = i - vmap (* (2 * (n <.> i))) n

refract :: Vec3D -> Vec3D -> Scalar -> Vec3D
refract i n r
    | v < 0 = zeroVector
    | otherwise = normalize $ vmap (* r_c) i + vmap (* (r_c * abs c - sqrt v)) n
  where
    c = n <.> negate i
    r_c = if c < 0 then r else 1 / r
    v = 1 + (r_c * r_c) * (c * c - 1)

mapToWindow :: Resolution -> Dimension -> Point2D -> Point3D
mapToWindow (rx, ry) (w, h) (px, py) = Vec3D (x / rxD) (y /ryD) 0.0
  where
    (rxD, ryD) = (fromIntegral rx, fromIntegral ry)
    (pxD, pyD) = (fromIntegral px, fromIntegral py)
    (wD, hD)   = (fromIntegral w, fromIntegral h)
    (x, y)     = ((pxD - rxD / 2) * wD, (pyD - ryD / 2) * hD)
