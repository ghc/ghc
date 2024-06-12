{-# LANGUAGE TypeFamilies, GADTSyntax, ExistentialQuantification #-}

-- This one should be rejected.
-- See Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet

module LocalGivenEqs2 where

type family F a
type family G b

data T where
  MkT :: F a ~ G b => a -> b -> T

f (MkT _ _) = True
