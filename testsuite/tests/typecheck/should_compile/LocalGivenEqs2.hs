{-# LANGUAGE TypeFamilies, GADTSyntax, ExistentialQuantification #-}

-- This is a simple case that exercises the LocalGivenEqs bullet
-- of Note [When does an implication have given equalities?] in GHC.Tc.Solver.Monad
-- If a future change rejects this, that's not the end of the world, but it's nice
-- to be able to infer `f`.

module LocalGivenEqs2 where

type family F a
type family G b

data T where
  MkT :: F a ~ G b => a -> b -> T

f (MkT _ _) = True
