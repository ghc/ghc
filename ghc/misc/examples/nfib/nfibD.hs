module Main where

main = print (nfib 28)

nfib :: Double -> Double

nfib n | n <= 1 = 1
       | otherwise = (n1 + n2 + 1)
                     where n1 = nfib (n-1) 
                           n2 = nfib (n-2)
