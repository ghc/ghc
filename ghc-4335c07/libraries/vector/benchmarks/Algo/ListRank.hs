module Algo.ListRank
where

import Data.Vector.Unboxed as V

listRank :: Int -> Vector Int
{-# NOINLINE listRank #-}
listRank n = pointer_jump xs val
  where
    xs = 0 `V.cons` V.enumFromTo 0 (n-2)

    val = V.zipWith (\i j -> if i == j then 0 else 1)
                    xs (V.enumFromTo 0 (n-1))

    pointer_jump pt val
      | npt == pt = val
      | otherwise = pointer_jump npt nval
      where
        npt  = V.backpermute pt pt
        nval = V.zipWith (+) val (V.backpermute val pt)

