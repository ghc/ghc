-- 'go' is specialised for two different call patterns, A and B, and
-- both rebox (t is scrutinised but also passed whole to 'sink').
-- Expect one warning for 'go' listing both call patterns.
module T27628k where

data T = A Int | B Int | C

-- The guard makes the use of 't' lazy, so boxity analysis keeps the
-- box: $wsink wants it.
sink :: T -> Int -> Int
sink t k
  | k < 0     = k
  | otherwise = case t of A n -> n; B n -> n; C -> 0
{-# NOINLINE sink #-}

go :: T -> Int -> Int
go t k = case t of
  A n -> if k == 0 then sink t k else go (B n) (k - 1)
  B n -> if k == 0 then sink t k else go (A n) (k - 1)
  C   -> 0
