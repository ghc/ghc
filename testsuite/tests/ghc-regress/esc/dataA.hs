module DataA where

data A a = B a | C a Int

{-# CONTRACT g :: A {x | x>0} -> any #-}
g :: A Int -> Int
g (B x) = x
