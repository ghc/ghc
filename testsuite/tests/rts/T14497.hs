module Main (main) where

import System.Timeout

fuc :: Integer -> Integer
fuc 0 = 1
fuc n = n * fuc (n - 1)

main :: IO ()
main = do
  let x = fuc 30000
  timeout 1000 (print x)
  print (x > 0)
