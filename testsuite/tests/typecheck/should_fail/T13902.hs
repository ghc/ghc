{-# LANGUAGE TypeApplications #-}
module T13902 where

f :: a -> a
f x = x

g :: Int
g = f @Int 42 5
