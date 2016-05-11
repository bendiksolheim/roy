module Lib
    ( someFunc
    ) where

import           Camera
import           Math
import           Ppm
import           Random
import           Surface

import           Data.List                     (foldl')
import           System.Random.Mersenne.Pure64

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing Nothing = Nothing
minMaybe Nothing a = a
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just $ min a b

intersectObjects :: Ray -> [AnyObject] -> Maybe Intersection
intersectObjects ray = foldl' reduce Nothing
  where
    reduce int obj = minMaybe (intersects ray obj) int

colorAtPoint :: [AnyObject] -> Ray -> Rand Color
colorAtPoint objects r@(Ray _ dir) =
  case intersection of
    Nothing -> return $ vmap (* (1.0 - t)) (Vec3D 1.0 1.0 1.0) + vmap (* t) (Vec3D 0.5 0.7 1.0)
    Just int -> do
      randomVector <- randomInUnitSphere
      let target = intPoint int + intGetNormal (intPoint int) int + randomVector
      color <- colorAtPoint objects (Ray (intPoint int) (target - intPoint int))
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

tracePixel :: [AnyObject] -> (Int, Int) -> Rand Color
tracePixel objects (y, x) = do
  rs <- getDoubles 100
  colors <- mapM getColor rs
  vmapM (/ 100) . foldl (+) zeroVector $ colors
  where
    getColor = colorAtPoint objects . getRay . transform
    transform (v, u) = ((v + fromIntegral y) / fromIntegral height, (u + fromIntegral x) / fromIntegral width)

trace :: [AnyObject] -> [(Int, Int)] -> Rand [Color]
trace = mapM . tracePixel

objs :: [AnyObject]
objs = [ AnyObject $ Sphere (Vec3D 0.0 0.0 (-1.0)) 0.5
       , AnyObject $ Sphere (Vec3D 0.0 (-100.5) (-1.0)) 100
       ]

someFunc :: IO ()
someFunc = do
  rnd <- newPureMT
  let res = evalRandom (trace objs pixels) rnd
  writeFile "image.ppm" $ makePpm 200 100 res
