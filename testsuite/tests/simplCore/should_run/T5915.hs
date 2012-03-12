module Main where

test = seq (seq id (\a -> undefined a)) 

main = print (test [0])
