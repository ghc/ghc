{-# LANGUAGE LinearTypes, ViewPatterns #-}
module T28095 (f) where

-- This test is the same as T27892a, but the let of clause 3 comes from
-- the beta-reduction of a linear lambda. Thus its binder has multiplicity
-- One, and the binder of the source let in clause 2 has multiplicity
-- Many. The multiplicity of a let binder has no meaning (see
-- Note [Multiplicity of let binders] in GHC.Types.Var). The checker must
-- give the two expressions the same representative and find f exhaustive.

data P = MkP Int Int
data W = Wrap (Int %1 -> P) | Other

expensive :: Int -> Int
expensive n = n * n
{-# NOINLINE expensive #-}

f :: Int -> Int
f ((\v -> expensive v) -> 0) = 0
f ((\v -> let x = expensive v in Wrap (\z -> MkP z x)) -> Other) = 1
f ((\v -> ((\x -> Wrap (\z -> MkP z x)) :: Int %1 -> W) (expensive v)) -> Wrap _) = 2
