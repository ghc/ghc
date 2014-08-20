module Main where

main :: IO()
main = print $ length [1..(2^(20::Int)::Integer)]
