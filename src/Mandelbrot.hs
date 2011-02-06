module Mandelbrot where

import Data.Complex
import Util

classicCardiodGrid :: RealFloat a => Int -> [Complex a]
classicCardiodGrid n = complexGrid n (-1.5) 0.5 (-1) 1

escapes :: RealFloat a => Int -> Complex a -> Bool
escapes maxi c = any ((> 2) . magnitude) ps
   where ps = take maxi $ iterate p (0 :+ 0)
         p = \z -> c + z^(2 :: Int)
