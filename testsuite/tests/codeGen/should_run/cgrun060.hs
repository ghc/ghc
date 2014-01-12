-- tickled a bug in stack squeezing in 6.8.2.  unsafePerformIO calls
-- noDuplicate#, which marks the update frames on the stack, and was
-- preventing subsequent update frames from being collapsed with the
-- marked frame.

module Main where

import System.IO.Unsafe

main = print (sim (replicate 100000 ()))

sim []     = True
sim (_:xs) = badStack (sim xs)

goodStack x = fromJust (Just x)          --no stack overflow
badStack  x = unsafePerformIO (return x) --stack overflow

fromJust (Just x) = x
