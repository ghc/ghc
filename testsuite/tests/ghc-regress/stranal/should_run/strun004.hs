module Main where

f 0 = 0
f x = x + g (x-1)

g 0 = 0
g x = x - f (x-1)

main = print (f 300)

