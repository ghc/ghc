{-# LANGUAGE ViewPatterns #-}
module T27892a (f) where

-- The pattern-match checker gives the same representative to the view
-- expressions of clauses 2 and 3. Thus clause 2 excludes Other, and
-- clause 3 covers the rest: f is exhaustive. Clause 1 puts an unrelated
-- key into the CoreMap of the checker. Thus the lookups of clauses 2 and
-- 3 go through the trie and not through the singleton comparison. The
-- lambda prevents the simple optimiser from inlining the let. See #27892.

data P = MkP Int Int
data W = Wrap (Int -> P) | Other

expensive :: Int -> Int
expensive n = n * n
{-# NOINLINE expensive #-}

f :: Int -> Int
f ((\v -> expensive v) -> 0) = 0
f ((\v -> let x = expensive v in Wrap (\z -> MkP z x)) -> Other) = 1
f ((\v -> let x = expensive v in Wrap (\z -> MkP z x)) -> Wrap _) = 2
