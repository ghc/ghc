{-# LANGUAGE DataKinds, KindSignatures, ListTuplePuns, NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds, Safe, TypeFamilies #-}

-- | Field selectors used by pair patterns in type-level lets.
module GHC.Internal.TupleProjection (PairFirst, PairSecond) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base.
import GHC.Internal.Tuple ()

-- | Select the first field of a promoted pair.
type family PairFirst (pair :: (a, b)) :: a where
  PairFirst '(x, _) = x

-- | Select the second field of a promoted pair.
type family PairSecond (pair :: (a, b)) :: b where
  PairSecond '(_, y) = y
