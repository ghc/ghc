{-# LANGUAGE TypeInType, TypeFamilies, GADTs, ConstraintKinds #-}

module T12919 where

import Data.Kind

data N = Z

data V :: N -> Type where
  VZ :: V Z

type family VC (n :: N) :: Type where
  VC Z = Type

type family VF (xs :: V n) (f :: VC n) :: Type where
  VF VZ f = f

data Dict c where
  Dict :: c => Dict c

prop :: xs ~ VZ => Dict (VF xs f ~ f)
prop = Dict
