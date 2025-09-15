module T25255 where

foreign export ccall foo :: Int -> Int

foo :: Int -> Int
foo x = x + 10
