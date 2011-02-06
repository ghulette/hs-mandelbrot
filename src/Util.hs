module Util where

import Data.Complex

reshape :: Int -> [a] -> [[a]]
reshape n xs = 
  case drop n xs of
    [] -> [take n xs]
    rs -> (take n xs) : (reshape n rs)

range :: (RealFloat a) => Int -> a -> a -> [a]
range n start end = map ((+) start . (*) scale . fromIntegral) [0..bnd]
  where bnd = pred n
        scale = (end - start) / (fromIntegral bnd)

complexGrid :: (RealFloat a) => Int -> a -> a -> a -> a -> [Complex a]
complexGrid n minx maxx miny maxy = do
  y <- range n miny maxy
  x <- range n minx maxx
  return (x :+ y)
