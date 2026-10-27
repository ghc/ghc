{-# LANGUAGE MagicHash #-}
-- The example of Note [Honour INLINE on 0-ary bindings] (#15519): x is
-- inlined, and g a0 b0 must still be computed once, not at every call of f.
module Main (main) where

import GHC.Exts
import Debug.Trace (trace)

data T = T Int#

g :: Int -> Int -> Int
g a b = trace "g" a * b + 1
{-# NOINLINE g #-}

a0, b0 :: Int
a0 = 3
b0 = 4
{-# NOINLINE a0 #-}
{-# NOINLINE b0 #-}

x :: T
x = case g a0 b0 of I# r -> T r
{-# INLINE x #-}

h :: T -> Int -> Int
h (T r) y = I# r + y

f :: Int -> Int
f y = h x y
{-# NOINLINE f #-}

main :: IO ()
main = print (sum (map f [1 .. 3]))
