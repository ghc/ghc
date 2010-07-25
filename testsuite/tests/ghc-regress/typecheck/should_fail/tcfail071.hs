-- !!! Mis-matched contexts in a mutually recursive group

{-# LANGUAGE NoRelaxedPolyRec #-}

module ShouldFail where

f :: (Ord d) => d -> d
f c = g c

g :: e -> e
g c = c
  where p = f (1 :: Int)
