module Test where

f :: Int -> Int
f x = x + 1

foreign export ccall f :: Int -> Int
