{-# LANGUAGE ForeignFunctionInterface #-}
module TestStub017 where
foreign export ccall f :: Int -> Int
f :: Int -> Int
f n = n + 1
