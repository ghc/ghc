{-# LANGUAGE TypeFamilies #-}

module T18875 where

-- This exercises Note [Type variable cycles] in GHC.Tc.Solver.Canonical

type family G a b where
  G (Maybe c) d = d

h :: (e ~ Maybe (G e f)) => e -> f
h (Just x) = x
