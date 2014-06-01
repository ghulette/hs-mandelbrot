import System.Environment (getArgs)
import Control.Monad (liftM)
import Codec.Picture
import Mandel

main :: IO ()
main = do
  n <- liftM (read . head) getArgs
  writePng "mandel.png" $ mandelImage n 512
