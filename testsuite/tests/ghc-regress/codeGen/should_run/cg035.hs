module Main (main) where

import IOExts ( unsafePerformIO )

c :: Double -> Double
c x = sin x
  where
    sin :: Double -> Double
    sin x = unsafePerformIO (_ccall_ sin x)

main = putStr (shows (c 0.0) "\n")
