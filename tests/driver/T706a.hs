{-# LANGUAGE Haskell2010 #-}
module T706 where

foreign export ccall f :: Int -> Int

f :: Int -> Int
f x = x + 1
