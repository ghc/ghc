module T18078 where

newtype N = N { unN :: Int -> Int }

-- This an example of a worker/wrapper thing
-- See Note [Cast worker/wrappers] in Simplify
-- We should get good code, with a $wf calling itself
-- but in 8.10 we do not
f :: N
{-# NOINLINE f #-}
f = N (\n -> if n==0 then 0 else unN f (n-1))

g x = unN f (x+1)
