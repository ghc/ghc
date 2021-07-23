module Foo( wombat ) where

-- We expect to eta-expand f to arity 2, but not to arity 3
-- See Note [Bottoming bindings] in GHC.Core.Opt.Simplify
f :: String -> Int -> Int -> Int
{-# NOINLINE f #-}
f s = error s

g :: (Int -> Int -> Int) -> Maybe Int
{-# NOINLINE g #-}
g h = let h1 = h 2 in Just (h1 2 + h1 3)

wombat s = g (f s)
