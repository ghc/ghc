{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE RankNTypes, StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module ParametricFilterByType where

import Data.Type.Equality ((:~:)(Refl))
import ParametricFilter (FilterKind)
import qualified ParametricFilterAdapters as A

-- | Select the result by element type.
type ByType :: FilterKind
type family ByType p xs where
    ByType @Bool p xs = xs
    ByType @Ordering p xs = '[]

-- | A witness for a family of kind 'FilterKind'.
data AtFilterKind (r :: FilterKind) = AtFilterKind

fits :: AtFilterKind ByType
fits = AtFilterKind

-- | Convert the list before applying the family.
type Lhs = ByType A.isGT (map A.boolToOrdering '[ 'True ])

-- | Convert the result of applying the family.
type Rhs = map A.boolToOrdering (ByType A.isTrue '[ 'True ])

-- | Check each side of the proposed law.
expected :: (Lhs :~: '[], Rhs :~: '[ 'GT ])
expected = (Refl, Refl)

#ifdef WRONG
wrong :: Lhs :~: Rhs
wrong = Refl
#endif
