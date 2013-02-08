module Main where

main = print (syn [-1])

syn :: [Int] -> [Int]
syn (d:ds) = rem d 0x40000000 : syn ds
syn [] = []
