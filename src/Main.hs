import System.Environment (getArgs)
import Data.Complex
import Codec.Image.PPM
import Mandelbrot
import Util

renderPPM :: Int -> [Bool] -> IO ()
renderPPM n xs = do
  let white = (255,255,255)
  let black = (0,0,0)
  let color = \b -> if b then white else black
  let pixels = reshape n (map color xs)
  putStrLn $ ppm P3 pixels

main :: IO ()
main = do
  [gridSizeStr] <- getArgs
  let gridSize = read gridSizeStr
  let maxIter = 1024
  let inputGrid = classicCardiodGrid gridSize :: [Complex Double]
  let outputGrid = map (escapes maxIter) inputGrid
  renderPPM gridSize outputGrid
