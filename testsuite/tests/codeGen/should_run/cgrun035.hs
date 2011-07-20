module Main (main) where

import Foreign.C
import System.IO.Unsafe ( unsafePerformIO )

c :: Double -> Double
c x = cos x
  where
    cos :: Double -> Double
    cos x = realToFrac (unsafePerformIO (c_cos (realToFrac x)))

foreign import ccall unsafe "cos"
  c_cos :: CDouble -> IO CDouble

main = putStr (shows (c 0.0) "\n")
