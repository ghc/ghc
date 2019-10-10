{-# OPTIONS_GHC -fno-strictness #-}

{-# LANGUAGE Unsafe, NoImplicitPrelude, MagicHash, GADTs, TypeApplications,
             ScopedTypeVariables, TypeOperators, KindSignatures, PolyKinds,
             StandaloneKindSignatures #-}

module Unsafe.Coerce
  ( unsafeCoerce
  , unsafeEqualityProof
  , unsafeHeteroEqualityProof
  , unsafeCastWith
  , UnsafeEquality (..)
  , UnsafeHeteroEquality (..)
  ) where

import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base
import GHC.Natural () -- See Note [Depend on GHC.Natural] in GHC.Base
import Data.Type.Equality
import GHC.Base

{-# INLINE unsafeCoerce #-}
unsafeCoerce :: forall a b . a -> b
unsafeCoerce x = unsafeCastWith (unsafeEqualityProof @a @b) x

data UnsafeEquality a b where
  UnsafeRefl :: UnsafeEquality a a

{-# NOINLINE unsafeEqualityProof #-}
unsafeEqualityProof :: forall a b . UnsafeEquality a b
unsafeEqualityProof = case unsafeEqualityProof @a @b of UnsafeRefl -> UnsafeRefl

{-# INLINE unsafeCastWith #-}
unsafeCastWith :: UnsafeEquality a b -> a -> b
unsafeCastWith UnsafeRefl x = x

type UnsafeHeteroEquality :: k1 -> k2 -> Type

data UnsafeHeteroEquality a b where
  UnsafeHRefl :: UnsafeHeteroEquality a a

{-# NOINLINE unsafeHeteroEqualityProof #-}
unsafeHeteroEqualityProof :: forall a b . UnsafeHeteroEquality a b
unsafeHeteroEqualityProof =
  (case unsafeEqualityProof @k1 @k2 of
    UnsafeRefl -> case unsafeEqualityProof @a' @b' of
      UnsafeRefl -> UnsafeHRefl)
        :: forall k1 k2 (a' :: k1) (b' :: k2) . UnsafeHeteroEquality a' b'
