module T23764 where

f :: a `op` b
f = f

g :: (Int, Bool)
g = f @Int @(,) @Bool
