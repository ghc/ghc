-- Testing showInt, lightly.

module Main(main) where

import Numeric

main = 
  do
   putStrLn (showInt (343023920121::Integer) [])
   putStrLn (showInt (3430239::Int) [])
   putStrLn (showInt (1212 :: Int) [])
   putStrLn (showInt (591125662431 `div` (517::Int)) [])
   -- showInt just works over naturals, wrap it up inside
   -- a use of Numeric.showSigned to show negative nums.
   putStrLn (showSigned (showInt) 0 (-111::Int) [])
   putStrLn (showInt (232189458241::Integer) [])

