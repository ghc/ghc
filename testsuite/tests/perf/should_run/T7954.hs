module Main where

norm ::  [Double] -> Double
norm = sqrt . sum . map (\x -> x*x)
   
main :: IO ()
main = print (norm (enumFromTo 0 10000000) > 100) 
