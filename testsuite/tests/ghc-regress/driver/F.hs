{-# OPTIONS_GHC -fffi #-}
module TestStub where
foreign export ccall f :: Int -> Int
f :: Int -> Int
f n = n + 1
