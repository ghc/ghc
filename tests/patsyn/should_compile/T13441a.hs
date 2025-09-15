{-# LANGUAGE PatternSynonyms, GADTs #-}
module T13441a where

data S where
  MkS :: Eq a => [a] -> S

-- Unidirectional pattern binding;
-- the existential is more specific than needed
-- c.f. T13441b
pattern P :: () => Eq x => x -> S
pattern P x <- MkS x
