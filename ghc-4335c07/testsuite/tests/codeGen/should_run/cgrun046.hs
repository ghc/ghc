module Main where

import System.IO

-- !!! CAF space leaks

main = lots_of_xs 10000

lots_of_xs 0 = return ()
lots_of_xs n = putChar 'x' >> lots_of_xs (n-1)
