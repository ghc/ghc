module Main where

main = print (nfib 20)

nfib :: Rational -> Rational

nfib n | n <= 1 = 1
       | otherwise = (n1 + n2 + 1)
                     where n1 = nfib (n-1) 
                           n2 = nfib (n-2)
