{-# LANGUAGE ForeignFunctionInterface #-}
module B.F where
foreign export ccall f :: Int -> Int
f :: Int -> Int
f n = n + 1
