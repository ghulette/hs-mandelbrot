-- Currently this only supports writing of the P3 format.  Based on the ppm
-- library on Hackage by Jinjing Wang.

module Codec.Image.PPM (Profile(..),PPM,ppm) where

import Control.Monad.Writer

data Profile = P1 | P2 | P3 | P4 | P5 | P6 deriving Show
type Color = (Int,Int,Int)

data PPM = PPM
  { profile  :: Profile
  , maxDepth :: Int
  , columns  :: Int
  , rows     :: Int
  , pixels   :: [Color]
  }
  deriving (Show)

write :: Show a => a -> Writer String ()
write = tell . show

writeEOL :: Writer String ()
writeEOL = tell "\n"

writePixel :: Profile -> Color -> Writer String ()
writePixel P3 (r,g,b) = do
  write r
  tell " "
  write g
  tell " "
  write b
  writeEOL
writePixel _ _ = error "Format not supported"

render :: PPM -> String
render img = execWriter $ do
  write $ profile img
  writeEOL
  write $ columns img
  tell " "
  write $ rows img
  writeEOL
  write $ maxDepth img
  writeEOL
  mapM_ (writePixel (profile img)) (pixels img)

-- | Convert an array of color values to a PPM string.  Note, this currently
-- | supports only P3.
ppm :: Profile -> [[Color]] -> String
ppm P3 = render . fromList
ppm _ = error "Format not supported"

fromList :: [[Color]] -> PPM
fromList [] = PPM P3 255 0 0 []
fromList cs = (fromList [])
  { columns = length $ head cs
  , rows    = length cs
  , pixels  = concat cs
  }
