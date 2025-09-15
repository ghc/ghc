{-# LANGUAGE TypeFamilies #-}

module T18875 where

-- This exercises Note [Type equality cycles] in GHC.Tc.Solver.Equality

type family G a b where
  G (Maybe c) d = d

h :: (e ~ Maybe (G e f)) => e -> f
h (Just x) = x
