module Closure where

add :: Int -> [Int] -> [Int]
add x = map (\n -> n + x) 
