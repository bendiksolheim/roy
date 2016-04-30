module Ppm (makePpm) where

import Math
import Data.List

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)

toRGBDecimal :: Double -> String
toRGBDecimal v = show  (round (v * 255.0) :: Int)

makePpm :: Int -> Int -> [Color] -> String
makePpm w h pixels = concat ["P3\n", show w, " ", show h, "\n255\n", p]
  where
    p = intercalate "\n" . reverse . map unwords . chunksOf w . map toRGB $ pixels
    toRGB (Vec3D r g b) = toRGBDecimal r ++ " " ++ toRGBDecimal g ++ " " ++ toRGBDecimal b
