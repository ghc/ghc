-- This test exposes the bug in GHC 7.0.1 (and earlier)
-- which did the following rule rewrite:
--
--    f (let v = 2 in g v) (let v = 3 in g v)
--    --->  let v = 2 in let v = 3 in g v + g v
--
-- which is wrong because of the shadowing of v

module Main where
foo :: Int -> Int
{-# INLINE foo #-}
foo x = g (bar (x,x))

bar :: (Int,Int) -> Int
{-# NOINLINE bar #-}
bar (x,y) = x

baz :: Int -> Int
{-# NOINLINE baz #-}
baz x = x

f :: Int -> Int -> Int
{-# NOINLINE f #-}
f x y = x+y

g :: Int -> Int
{-# NOINLINE g #-}
g x = x

{-# RULES

 "f/g" [1] forall x y. f (g x) (g y) = x + y

 #-}

main = print $ f (foo (baz 1)) (foo (baz 2))
-- Should print 3
-- Bug means that it prints 4

