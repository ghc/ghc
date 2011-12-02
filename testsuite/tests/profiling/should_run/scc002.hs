module Main (main) where

import Data.Array.Unboxed
import System.Environment

main :: IO ()
main = print (yan 30 + yan 40)

{- we're looking for:
MAIN        MAIN                    106           0    0.0    0.0     0.0  100.0
 CAF        Main                    211           0    0.0    0.0     0.0   99.1
  yan       Main                    213           2    0.0    0.0     0.0   98.9
   yan.e    Main                    217           1    0.0    0.0     0.0   98.9
    big     Main                    218           1    0.0   98.9     0.0   98.9
  main      Main                    212           1    0.0    0.2     0.0    0.2
   yan      Main                    214           0    0.0    0.0     0.0    0.0
    yan.\   Main                    215           2    0.0    0.0     0.0    0.0
     yan1   Main                    216           2    0.0    0.0     0.0    0.0

Note the 2 entries for yan.\, the lambda expression inside yan.
-}

big :: Int -> Int
big x = (array (0,1000000) [(0,x)] :: UArray Int Int) ! 0

yan = let e = big 20 in \x -> {-# SCC yan1 #-} x + e
