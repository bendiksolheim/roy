module Lib
    ( someFunc
    ) where

import Types
import Ppm
import Data.Maybe (isNothing, isJust, fromJust)

-- data Diffuse = Solid Color | Perlin (Point3D -> Color)
-- data Texture = Texture Diffuse Double Int Double Double
-- type TexturedObject = (Object, Texture)
-- type Intensity = Vec3D
-- data Light = PointLight Point3D Intensity | AmbientLight Intensity
-- data Camera = Camera Point3D Dimension
-- data Scene = Scene Camera Color [TexturedObject] [Light]
-- type Image = Point2D -> Color

-- intDist :: Maybe Intersection -> Double
-- intDist Nothing = 0.0
-- intDist ( Just (Intersection d _ _) ) = d
-- intText :: Maybe Intersection -> Texture
-- intText Nothing = Texture (Solid zeroVector) 0.0 0 0.0 0.0
-- intText (Just (Intersection _ _ (_, t))) = t
-- intPoint :: Maybe Intersection -> Point3D
-- intPoint Nothing = zeroVector
-- intPoint (Just (Intersection d (Ray start dir) _)) = start + vmap (* d) dir
-- colorAt :: Maybe Intersection -> Color
-- colorAt Nothing = Vec3D 0.0 0.0 0.0
-- colorAt (Just (Intersection _ _ (_, Texture (Solid color) _ _ _ _))) = color
-- colorAt i@(Just (Intersection _ _ (_, Texture (Perlin f) _ _ _ _))) = f (intPoint i)
-- normalAt :: Maybe Intersection -> Normal
-- normalAt Nothing = Vec3D 0.0 0.0 0.0
-- normalAt i@(Just (Intersection _ _ (o, _))) = normal (intPoint i) o

-- fstPos :: [Double] -> Double
-- fstPos [] = 0.0
-- fstPos (x:xs) = if x > epsilon then x else fstPos xs

-- closestInt :: Ray -> Maybe Intersection -> TexturedObject -> Maybe Intersection
-- closestInt r i (o, m) =
--   if d > epsilon && (isNothing i || d < intDist i)
       --   then Just (Intersection d r (o, m))
--   else i
--   where
--     d = fstPos (r `intersect` o)

-- intersects :: Ray -> [TexturedObject] -> Maybe Intersection
-- intersects r = foldl (closestInt r) Nothing

-- diff :: Maybe Intersection -> Light -> Color
-- diff _ (AmbientLight _) = zeroVector
-- diff i (PointLight pos int) = vmap (* mkNormVect (intPoint i) pos <.> normalAt i) int * colorAt i

-- spec :: Maybe Intersection -> Vec3D -> Light -> Color
-- spec _ _ (AmbientLight _) = zeroVector
-- spec i d (PointLight pos int) = vmap (* (reflCoef * ( (normalAt i <.> h) ** fromIntegral specCoef))) int
--   where
--     h = normalize (negate d) + mkNormVect (intPoint i) pos
--     (Texture _ reflCoef specCoef _ _) = intText i

-- shadePt :: Intersection -> Vec3D -> [TexturedObject] -> Light -> Color
-- shadePt _ _ _ (AmbientLight int) = int
-- shadePt i d o l@(PointLight pos _)
--   | s = zeroVector
--   | otherwise = diff (Just i) l + spec (Just i) d l
--   where
--     s = isJust i_s && intDist i_s <= dist (intPoint (Just i)) pos
--     i_s = intersects (mkRay (intPoint (Just i)) pos) o

-- reflectPt :: Int -> Intersection -> Vec3D -> [TexturedObject] -> [Light] -> Color
-- reflectPt depth i d = colorAtPoint depth (Ray (intPoint (Just i)) (reflect d (normalAt (Just i)))) zeroVector

-- refractPt :: Int -> Intersection -> Vec3D -> Color -> [TexturedObject] -> [Light] -> Color
-- refractPt depth i d b =
--   if refractedDir == zeroVector
--   then (\x y -> zeroVector)
--   else colorAtPoint depth (Ray (intPoint (Just i)) refractedDir) (vmap (* refrCoef) b)
--   where
--     refractedDir = refract d (normalAt (Just i)) refrIndex
--     (Texture _ _ _ refrCoef refrIndex) = intText (Just i)

-- colorAtPoint :: Int -> Ray -> Color -> [TexturedObject] -> [Light] -> Color
-- colorAtPoint (-1) _ _ _ _ = zeroVector
-- colorAtPoint d r@(Ray _ dir) b o l =
--   if isNothing i
--   then b
--   else clip $ shadeColor + reflectColor + refractColor
--   where
--     shadeColor = foldl (+) zeroVector (map (shadePt (fromJust i) dir o) l)
--     reflectColor =
--       if reflCoef == 0.0
--       then zeroVector
--       else vmap (* reflCoef) (reflectPt (d - 1) (fromJust i) dir o l)
--     refractColor =
--       if refrCoef == 0.0
--       then zeroVector
--       else vmap (* refrCoef) (refractPt (d - 1) (fromJust i) dir b o l)
--     i = intersects r o
--     (Texture _ reflCoef _ refrCoef _) = intText i

-- rayTracePt :: Int -> Scene -> Point3D -> Color
-- rayTracePt d (Scene (Camera eye _) b o l) p = colorAtPoint d (Ray p (mkNormVect eye p)) b o l

-- rayTrace :: Int -> Resolution -> Scene -> Image
-- rayTrace d r s@(Scene (Camera _ dim) _ _ _) = rayTracePt d s . mapToWindow r dim
-- intPoint (Just (Intersection d (Ray start dir) _)) = start + vmap (* d) dir

data Intersection = Intersection Double Ray Object

intersectionPoint :: Ray -> Scalar -> Point3D
intersectionPoint (Ray orig dir) t = orig + vmap (* t) dir

sphereCenter :: Intersection -> Point3D
sphereCenter (Intersection _ _ (Sphere _ center)) = center

intDist :: Maybe Intersection -> Scalar
intDist Nothing = 0.0
intDist (Just (Intersection t _ _)) = t

fstPos :: [Scalar] -> Scalar
fstPos [] = 0.0
fstPos (x:xs) = if x > epsilon then x else fstPos xs

intersects :: Ray -> Maybe Intersection -> Object -> Maybe Intersection
intersects ray int obj =
  if t > epsilon && (isNothing int || t < intDist int)
         then Just (Intersection t ray obj)
  else int
  where
    t = fstPos $ ray `intersect` obj

intersectObjects :: Ray -> [Object] -> Maybe Intersection
intersectObjects ray = foldl (intersects ray) Nothing

colorAtPoint :: Ray -> [Object] -> Color
colorAtPoint r@(Ray _ dir) objs =
  case intersection of
    Nothing -> vmap (* (1.0 - t)) (Vec3D 1.0 1.0 1.0) + vmap (* t) (Vec3D 0.5 0.7 1.0)
    Just d  -> vmap (* 0.5) $ vmap (+ 1.0) (mkNormVect (intersectionPoint r t) (sphereCenter d))
  where
    (Vec3D _ y _) = normalize dir
    t             = 0.5 * (y + 1.0)
    intersection  = intersectObjects r objs

width :: Int
width = 200

height :: Int
height = 100

camera :: Point3D
camera = Vec3D 0.0 0.0 0.0

pixels :: [(Int, Int)]
pixels = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

start :: Vec3D
start = Vec3D (-2.0) (-1.0) (-1.0)

horizontal :: Vec3D
horizontal = Vec3D 4.0 0.0 0.0

vertical :: Vec3D
vertical = Vec3D 0.0 2.0 0.0

trace :: [(Int, Int)] -> [Object] -> [Color]
trace pixels objs = map (\(vy, vx) -> colorAtPoint (ray vy vx) objs) pixels
  where
    horiz w = vmap (* (fromIntegral w / fromIntegral width)) horizontal
    verti h = vmap (* (fromIntegral h / fromIntegral height)) vertical
    pointAt (Vec3D xs ys zs) (Vec3D xh _ _) (Vec3D _ yw _) = Vec3D (xs + xh) (ys + yw) zs
    getPoint y' x' = pointAt start (horiz x') (verti y')
    ray vy vx = Ray camera $ getPoint vy vx

objs :: [Object]
objs = [ Sphere 0.5 (Vec3D 0.0 0.0 (-1.0))
       , Sphere 100 (Vec3D 0.0 (-100.5) (-1.0))
       ]

someFunc :: IO ()
someFunc = writeFile "image.ppm" . makePpm 200 100 $ trace pixels objs
