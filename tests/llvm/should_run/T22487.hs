
module Main where

add :: Double -> Double -> Double
add x y = x + y
{-# NOINLINE add #-}
main = do putStrLn "Hello world!"
          print (add 1.0 2.0)
