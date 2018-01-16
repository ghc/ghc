{-# OPTIONS -fno-spec-constr-count #-}
module Algo.AwShCC (awshcc) where

import Data.Vector.Unboxed as V

awshcc :: (Int, Vector Int, Vector Int) -> Vector Int
{-# NOINLINE awshcc #-}
awshcc (n, es1, es2) = concomp ds es1' es2'
    where
      ds = V.enumFromTo 0 (n-1) V.++ V.enumFromTo 0 (n-1)
      es1' = es1 V.++ es2
      es2' = es2 V.++ es1

      starCheck ds = V.backpermute st' gs
        where
          gs  = V.backpermute ds ds
          st  = V.zipWith (==) ds gs
          st' = V.update st . V.filter (not . snd)
                            $ V.zip gs st

      concomp ds es1 es2
        | V.and (starCheck ds'') = ds''
        | otherwise              = concomp (V.backpermute ds'' ds'') es1 es2
        where
          ds'  = V.update ds
               . V.map (\(di, dj, gi) -> (di, dj))
               . V.filter (\(di, dj, gi) -> gi == di && di > dj)
               $ V.zip3 (V.backpermute ds es1)
                        (V.backpermute ds es2)
                        (V.backpermute ds (V.backpermute ds es1))

          ds'' = V.update ds'
               . V.map (\(di, dj, st) -> (di, dj))
               . V.filter (\(di, dj, st) -> st && di /= dj)
               $ V.zip3 (V.backpermute ds' es1)
                        (V.backpermute ds' es2)
                        (V.backpermute (starCheck ds') es1)

