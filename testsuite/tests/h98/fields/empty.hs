module Main where

data T = T1 Int Int Int
       | T2 Float Float Float

f (T1 {}) = True
f (T2 {}) = False

main = print (f (T1 1 2 3))
