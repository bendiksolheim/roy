module Lib
    ( someFunc
    ) where

import           Camera
import           Math
import           Ppm
import           Random
import           Surface

import           Data.Maybe                    (isNothing)
import           System.Random.Mersenne.Pure64

data Intersection = Intersection Double Ray ObjectW

intersectionPoint :: Ray -> Scalar -> Point3D
intersectionPoint (Ray orig dir) t = orig + vmap (* t) dir

intDist :: Maybe Intersection -> Scalar
intDist Nothing = 0.0
intDist (Just (Intersection t _ _)) = t

fstPos :: [Scalar] -> Scalar
fstPos [] = 0.0
fstPos (x:xs) = if x > epsilon then x else fstPos xs

intersects :: Ray -> Maybe Intersection -> ObjectW -> Maybe Intersection
intersects ray int o@(OW obj) =
  if t > epsilon && (isNothing int || t < intDist int)
  then Just (Intersection t ray o)
  else int
  where
    t = fstPos $ intersect ray obj

intersectObjects :: Ray -> [ObjectW] -> Maybe Intersection
intersectObjects ray = foldl (intersects ray) Nothing

colorAtPoint :: [ObjectW] -> Ray -> Rand Color
colorAtPoint objects r@(Ray _ dir) =
  case intersection of
    Nothing -> return $ vmap (* (1.0 - t)) (Vec3D 1.0 1.0 1.0) + vmap (* t) (Vec3D 0.5 0.7 1.0)
    Just (Intersection p ray (OW d)) -> do
      randomVector <- randomInUnitSphere
      let target = intersectionPoint ray p + getNormal (intersectionPoint ray p) d + randomVector
      color <- colorAtPoint objects (Ray (intersectionPoint ray p) (target - intersectionPoint ray p))
      vmapM (* 0.5) color
  where
    (Vec3D _ y _) = normalize dir
    t             = 0.5 * (y + 1.0)
    intersection  = intersectObjects r objects

width :: Int
width = 200

height :: Int
height = 100

pixels :: [(Int, Int)]
pixels = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

randomInUnitSphere :: Rand Vec3D
randomInUnitSphere = do
    x <- getDouble
    y <- getDouble
    z <- getDouble
    let p = vmap (* 2.0) (Vec3D x y z) - Vec3D 1.0 1.0 1.0
    if p <.> p >= 10
    then randomInUnitSphere
    else return p

tracePixel :: [ObjectW] -> (Int, Int) -> Rand Color
tracePixel objects (y, x) = do
  rs <- getDoubles 100
  colors <- mapM getColor rs
  vmapM (/ 100) . foldl (+) zeroVector $ colors
  where
    getColor = colorAtPoint objects . getRay . transform
    transform (v, u) = ((v + fromIntegral y) / fromIntegral height, (u + fromIntegral x) / fromIntegral width)

trace :: [ObjectW] -> [(Int, Int)] -> Rand [Color]
trace = mapM . tracePixel

objs :: [ObjectW]
objs = [ OW $ Sphere (Vec3D 0.0 0.0 (-1.0)) 0.5
       , OW $ Sphere (Vec3D 0.0 (-100.5) (-1.0)) 100
       ]

someFunc :: IO ()
someFunc = do
  rnd <- newPureMT
  let res = evalRandom (trace objs pixels) rnd
  writeFile "image.ppm" $ makePpm 200 100 res
