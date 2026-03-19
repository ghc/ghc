module Main where

f :: Double -> Double
f x = sum [recip $ i * (i + x) + 0.0 :: Double | i <- [-99.5 .. 99.5]]

main :: IO ()
main = print (f 0.5)
