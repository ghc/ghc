-- !!! Mis-matched contexts in a mutually recursive group

{-  # LANGUAGE NoRelaxedPolyRec #-}
-- With the new type checker you can't turn off RelaxedPolyRec
-- so this test always succeeds

module ShouldFail where

f :: (Ord d) => d -> d
f c = g c

g :: e -> e
g c = c
  where p = f (1 :: Int)
