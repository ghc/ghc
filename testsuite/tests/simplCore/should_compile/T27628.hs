-- AndreasK's reproducer from #27628: SpecConstr specialises 'go' on LC,
-- but 'subst' wants the box, so the specialisation reboxes.
-- Expect a -Wspec-constr-reboxing warning for 'go'.
module T27628 where

data LC = LC Int Int

-- The guard makes the use of 'lc' lazy, so boxity analysis decides *not*
-- to unbox LC in subst's worker -- $wsubst wants the box.
subst :: LC -> Int -> Int
subst lc t
  | t < 0     = t
  | otherwise = case lc of LC n _ -> n
{-# NOINLINE subst #-}

go :: LC -> Int -> Int -> Int
go _ 0 acc = acc
go lc@(LC n _) k acc
  | even k    = go (LC n k) (k - 1) (acc + subst lc k)  -- SpecConstr fires here
  | otherwise = go lc       (k - 1) (acc + subst lc k)

f :: Int
f = go (LC 1 0) 20000000 0
