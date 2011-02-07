{-# LANGUAGE BangPatterns #-}

module Mandelbrot (classicCardiodGrid,escapes) where

import Data.Complex
import Util

classicCardiodGrid :: RealFloat a => Int -> [Complex a]
classicCardiodGrid n = complexGrid n (-1.5) 0.5 (-1) 1

escapes1 :: RealFloat a => Int -> Complex a -> Bool
escapes1 maxi c = any ((> 2) . magnitude) ps
   where ps = take maxi $ iterate p (0 :+ 0)
         p = \z -> c + z^(2 :: Int)

escapes2 :: RealFloat a => Int -> Complex a -> Bool
escapes2 i c = escapes2' i c (0 :+ 0)

escapes2' :: RealFloat a => Int -> Complex a -> Complex a -> Bool
escapes2' 0 _ _ = False
escapes2' !i !c !z = 
  if magnitude z' > 2 then True else escapes2' (pred i) c z'
  where z' = c + z^(2 :: Int)

escapes :: RealFloat a => Int -> Complex a -> Bool
escapes = escapes1
