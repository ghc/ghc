{-# LANGUAGE DataKinds, KindSignatures, ListTuplePuns, NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds, Safe, TypeFamilies #-}

-- | Field selectors used by pair patterns in type-level lets.
module GHC.Internal.TupleProjection (PairFirst, PairSecond, ProductUnapply, ProductField) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base.
import GHC.Internal.Tuple ()

-- | Select the first field of a promoted pair.
type family PairFirst (pair :: (a, b)) :: a where
  PairFirst '(x, _) = x

-- | Select the second field of a promoted pair.
type family PairSecond (pair :: (a, b)) :: b where
  PairSecond '(_, y) = y

-- | Split the last application. The witness fixes the kinds of both parts.
type family ProductUnapply (witness :: a -> p) (product :: p) :: (a -> p, a) where
  ProductUnapply witness (f x) = '(f, x)

-- | Keep a field when the constructor matches the head of the scrutinee.
type family ProductField (con :: k) (head :: k) (field :: a) :: a where
  ProductField con con field = field
