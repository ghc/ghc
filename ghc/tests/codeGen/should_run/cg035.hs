{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

-- The above option is required because 'erf' isn't a POSIX function, and we
-- need a prototype for it in order to compile the following code correctly.
-- Defining NON_POSIX_SOURCE tells the RTS not to define _POSIX_SOURCE.

module Main (main) where

import IOExts ( unsafePerformIO )

po :: Double -> Double
po rd = 0.5 + 0.5 * erf ((rd / 1.04) / sqrt 2)
  where
    erf :: Double -> Double
    erf x = unsafePerformIO (_ccall_ erf x)

main = putStr (shows (po 2.0) "\n")
