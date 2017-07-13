module Lib
    ( someFunc
    ) where

import           Camera
import           Math
import           Ppm
import           Random
import           Surface
import           Material
import           Intersection
import           Entity

import           Data.List                     (foldl')
import           System.Random.Mersenne.Pure64
import           Debug.Trace
import           Data.Maybe                    (fromJust)

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing Nothing = Nothing
minMaybe Nothing a = a
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just $ min a b

intersectObjects :: Ray -> [Entity] -> Maybe Intersection
intersectObjects ray = foldl' reduce Nothing
  where
    reduce int obj = minMaybe (intersects ray obj) int

scatter :: Ray -> Intersection -> Material -> Rand (Maybe Scattered)
scatter _ int (Lambertian albedo) = do
  randomVector <- randomInUnitSphere
  let target = intPoint int + intGetNormal (intPoint int) int + randomVector
  let s = Ray (intPoint int) (target - intPoint int)
  return $ Just (Scattered s albedo)

scatter (Ray _ dir) int (Metal albedo fuzz) = do
  let reflected = reflect (normalize dir) (intGetNormal (intPoint int) int)
  r <- randomInUnitSphere
  let s@(Ray _ sDir) = Ray (intPoint int) (reflected + vmap (* fuzz) r )
  let scattered = Scattered s albedo
  if sDir <.> intGetNormal (intPoint int) int > 0
  then return (Just scattered)
  else return Nothing

scatter (Ray _ dir) int@(Intersection _ p _) (Dielectric ri) = do
  let norm = intGetNormal (intPoint int) int
  let predicate = ( dir <.> norm ) > 0
  let reflected = reflect dir norm
  let outwardNormal = getOutwardNormal predicate norm
  let ni_over_nt = getNi_over_nt predicate
  let cosine = getCosine predicate norm
  let att = Vec3D 1.0 1.0 1.0
  let ref = refract dir outwardNormal ni_over_nt
  let reflectProb = getReflectProb ref cosine
  prob <- getDouble
  if prob < reflectProb
  then return . Just $ Scattered (Ray p reflected) att
  else return . Just $ Scattered (Ray p (fromJust ref)) att
-- case refract dir outwardNormal ni_over_nt of
  --   Just a -> do
  --     prob <- getDouble
  --     let schl = schlick cosine ri
  --     if prob < schl
  --     then return . trace "1" $ Just (Scattered (Ray p a) attenuation)
  --     else return . trace "2" $ Just (Scattered (Ray p reflected) attenuation)
    -- Nothing -> return $ Just (Scattered (Ray p reflected) attenuation)
  where
    getOutwardNormal predicate norm = if predicate then negate norm else norm
    getNi_over_nt predicate = if predicate then ri else 1.0 / ri
    tmp norm = dir <.> norm / len dir
    getCosine predicate norm = if predicate
                               then sqrt $ 1 - ri * ri * (1 - tmp norm * tmp norm)
                               else -1.0 * (dir <.> norm) / len dir
    getReflectProb (Just _) cosine = schlick cosine ri
    getReflectProb Nothing  _ = 1.0


-- NOTE: Får en evig rekursjon i en Dielectric. Havner inne i Just (Scattered .. ).. hver gang
-- helt til depth er større enn 50. "intersection" burde bli Nothing før depth blir 50, men
-- det ser ikke ut til å skje. Feil med måten man intersecter på?
colorAtPoint :: [Entity] -> Int -> Ray -> Rand Color
colorAtPoint objects depth r@(Ray _ dir) =
  case intersection of
    Nothing -> return $ vmap (* (1.0 - t)) (Vec3D 1.0 1.0 1.0) + vmap (* t) (Vec3D 0.5 0.7 1.0)
    Just int ->
      if depth >= 50
      then return $ trace (show (entityMaterial (intEntity int))) zeroVector
      else do
        s <- scatter r int (entityMaterial (intEntity int))
        case s of
          Nothing -> return zeroVector
          Just (Scattered sc att) -> do
            tmp <- colorAtPoint objects (depth + 1) sc
            return $ att * tmp
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

tracePixel :: [Entity] -> (Int, Int) -> Rand Color
tracePixel objects (y, x) = do
  rs <- getDoubles 100
  colors <- mapM getColor rs
  vmapM (/ 100) . foldl (+) zeroVector $ colors
  where
    getColor = colorAtPoint objects 0 . getRay . transform
    transform (v, u) = ((v + fromIntegral y) / fromIntegral height, (u + fromIntegral x) / fromIntegral width)

tracePixels :: [Entity] -> [(Int, Int)] -> Rand [Color]
tracePixels = mapM . tracePixel

objs :: [Entity]
objs = [ Entity (AnyObject (Sphere (Vec3D 0.0 0.0 (-1.0)) 0.5)) (Dielectric 1.5)
       -- , Entity (AnyObject (Sphere (Vec3D 0.0 0.0 (-1.0)) 0.5)) (Lambertian (Vec3D 0.1 0.2 0.5))
       , Entity (AnyObject (Sphere (Vec3D 0.0 (-100.5) (-1.0)) 100)) (Lambertian (Vec3D 0.8 0.8 0.0))
       , Entity (AnyObject (Sphere (Vec3D 1.0 0.0 (-1.0)) 0.5)) (Metal (Vec3D 0.8 0.6 0.2) 0.3)
       , Entity (AnyObject (Sphere (Vec3D (-1.0) 0.0 (-1.0)) 0.5)) (Dielectric 1.5)
       -- , Entity (AnyObject (Sphere (Vec3D (-1.0) 0.0 (-1.0)) (-0.45))) (Dielectric 1.5)
       ]

someFunc :: IO ()
someFunc = do
  rnd <- newPureMT
  let res = evalRandom (tracePixels objs pixels) rnd
  writeFile "image.ppm" $ makePpm width height res
