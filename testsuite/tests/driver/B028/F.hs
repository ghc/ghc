{-# LANGUAGE ForeignFunctionInterface #-}
module B028.F where
foreign export ccall f :: Int -> Int
f :: Int -> Int
f n = n + 1
