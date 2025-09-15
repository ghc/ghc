module Main where

{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = x

{-# RULES "foo/5" forall (f :: Int -> Int). foo (f 5) = foo (f 42) #-}
-- highly suspect, of course!

main = print $ foo (let {-# NOINLINE j #-}
                        j :: Int -> Int
                        j n = n + 1 in j 5)

{-
If we're not careful, this will get rewritten to

  main = print $ let <join> j n = n + 1 in foo (j 42)

which violates the join point invariant (can't invoke a join point from
non-tail context). Solution is to refuse to float join points when matching
RULES.
-}
