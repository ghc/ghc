{-# LANGUAGE PatternSynonyms, GADTs #-}
module T13441a where

data S where
  MkS :: Eq a => [a] -> S

-- Implicitly-bidirectional pattern binding;
-- the existential is more specific than needed,
-- and hence should be rejected
-- c.f. T13441a
pattern P :: () => Eq x => x -> S
pattern P x = MkS x
