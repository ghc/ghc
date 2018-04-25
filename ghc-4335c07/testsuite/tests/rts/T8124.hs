module T8124 where

f :: Int -> Int
f x = x + 1

foreign export ccall "f" f :: Int -> Int
