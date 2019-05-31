{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs, ConstraintKinds #-}
{-# LANGUAGE TopLevelKindSignatures #-}

module T12919 where

import Data.Kind

data N = Z

data V :: N -> Type where
  VZ :: V Z

type family VC (n :: N) :: Type where
  VC Z = Type

type VF :: V n -> VC n -> Type
type family VF xs f where
  VF VZ f = f

data Dict c where
  Dict :: c => Dict c

prop :: xs ~ VZ => Dict (VF xs f ~ f)
prop = Dict
