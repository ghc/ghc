{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, BangPatterns, RankNTypes #-}

{-
This is a simplified version of Data.OldList module from base.
This caused an assertion failure in earlier version of linear
types implementation.
-}

module Data.OldList where

import GHC.Base
import GHC.Types (Multiplicity)

sortBy :: forall a . (a -> a -> Ordering) -> [a]
sortBy cmp = []
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

--    descending :: a -> [a] -> [a] -> [[a]]
    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending :: a -> (forall (i :: Multiplicity) . [a] %i -> [a]) -> [a] -> [[a]]
    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b foo bs
      where
        foo :: [a] %(k :: Multiplicity) -> [a]
        foo ys = as (a:ys)
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs


