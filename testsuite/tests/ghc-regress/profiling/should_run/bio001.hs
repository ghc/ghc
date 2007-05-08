-- Test for #1227
module Main where

loop n = foldr (+) 0 [1..n]

main = print (loop 100000)
