module Main (main) where

--import PreludeGlaST
import ST
import STBase

po :: Double -> Double
po rd = 0.5 + 0.5 * erf ((rd / 1.04) / sqrt 2)
  where
    erf :: Double -> Double
    erf x = unsafePerformPrimIO (_ccall_ erf x)

main = putStr (shows (po 2.0) "\n")
