module Mandel (mandelImage) where

import Data.Complex
import Codec.Picture

type C = Complex Float

bounded :: Int -> C -> Maybe Int
bounded i c = bounded' 0 i
  where bounded' z n | n == 0 = Nothing
                     | magnitude z > 2 = Just n
                     | otherwise = bounded' (z * z + c) (n - 1)

scale :: Float -> Float -> Int -> Int -> Float
scale lo hi n x = xr / nr * (hi - lo) + lo
  where nr = fromIntegral n
        xr = fromIntegral x

rlo, rhi, ilo, ihi :: Float
rlo = -1.5
rhi =  0.5
ilo = -1.0
ihi =  1.0

color :: Int -> Int -> Pixel8
color i imax = floor (ir / mr * 255.0)
  where ir = fromIntegral i :: Float
        mr = fromIntegral imax :: Float

pixel :: Int -> Int -> Int -> Int -> PixelRGB8
pixel n maxIter x y =
  case bounded maxIter (r :+ i) of
    Just iter -> let c = color iter maxIter in PixelRGB8 0 0 c
    Nothing -> PixelRGB8 0 0 0
  where r = scale rlo rhi n x
        i = scale ilo ihi n y

mandelImage :: Int -> Int -> Image PixelRGB8
mandelImage n maxIter = generateImage (pixel n maxIter) n n
