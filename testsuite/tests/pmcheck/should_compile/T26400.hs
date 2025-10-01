{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module T26400 where

import GHC.Exts ( UnliftedType )
import GHC.TypeLits ( TypeError, ErrorMessage(..) )

data N = Z | S N

-- Make this type unlifted to avoid any subtleties about laziness
type SNat :: N -> UnliftedType
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

type (-) :: N -> N -> N
type family a - b where
  n   - Z   = n
  Z   - S _ = TypeError ( Text "impossible" )
  S n - S m = n - m

testFn :: SNat n -> SNat m -> SNat (n - m) -> Int
testFn SZ (SS _) SZ     = 666
testFn SZ (SS _) (SS _) = 999
  -- [G] g1 :: n ~ Z
  -- [G] g2 :: m ~ S m1
  -- [G] g3 :: (n-m) ~ S m2
  -- Use the first two givens to substitute in the third, we get:
  -- [G] g3' :: Z - S m1 ~ S m2
  -- Reduce the LHS using the type family
  -- [G] g3'' :: TypeError ... ~ S m2
  -- Hence g3'' is insoluble and the equation can never match
testFn _ _ _ = 1
