module T13002b where

testb :: Int -> Int
testb n = length ([0..(10^n)] :: [Int])
