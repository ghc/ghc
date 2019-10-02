-- We don't to strictness analysis on this file to avoid turning loopy unsafe
-- equality terms below to actual loops. See Note [unsafeCoerce magic] below for
-- how they're supposed to be compiled.
{-# OPTIONS_GHC -fno-strictness #-}

{-# LANGUAGE Unsafe, NoImplicitPrelude, MagicHash, GADTs, TypeApplications,
             ScopedTypeVariables, TypeOperators, KindSignatures, PolyKinds,
             StandaloneKindSignatures #-}

module Unsafe.Coerce
  ( unsafeCoerce
  , unsafeCoerceUnlifted
  , unsafeCoerceAddr
  , unsafeEqualityProof
  , unsafeHeteroEqualityProof
  , UnsafeEquality (..)
  , UnsafeHeteroEquality (..)
  ) where

import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base
import GHC.Natural () -- See Note [Depend on GHC.Natural] in GHC.Base
import GHC.Base
import GHC.Types

{-
Note [unsafeCoerce magic]
~~~~~~~~~~~~~~~~~~~~~~~~~

- The compiler transforms

      case unsafeEqualityProof of UnsafeRefl -> blah
      ==>
      blah

  in Core-to-STG pass.

- The compiler is careful not to eliminate the case alternative

      UnsafeRefl (g :: Int ~ Bool) -> blah

  even though the coercion is "impossible". See DataCon.dataConCannotMatch.
-}

data UnsafeEquality a b where
  UnsafeRefl :: UnsafeEquality a a

{-# NOINLINE unsafeEqualityProof #-}
unsafeEqualityProof :: forall a b . UnsafeEquality a b
unsafeEqualityProof = case unsafeEqualityProof @a @b of UnsafeRefl -> UnsafeRefl

{-# INLINE unsafeCoerce #-}
-- The INLINE will almost certainly happen automatically, but it's almost
-- certain to generate (slightly) better code, so let's do it.  For example
--
--   case (unsafeCoerce blah) of ...
--
-- will turn into
--
--   case unsafeEqualityProov of UnsafeRefl -> case blah of ...
--
-- which is definitely better.
unsafeCoerce :: forall (a :: Type) (b :: Type) . a -> b
unsafeCoerce x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

{-# INLINE unsafeCoerceUnlifted #-}
unsafeCoerceUnlifted :: forall (a :: TYPE 'UnliftedRep) (b :: TYPE 'UnliftedRep) . a -> b
unsafeCoerceUnlifted x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

{-# INLINE unsafeCoerceAddr #-}
unsafeCoerceAddr :: forall (a :: TYPE 'AddrRep) (b :: TYPE 'AddrRep) . a -> b
-- Addr# and (StablePtr# a) both have kind (TYPE AddrRep), and we might want to
-- coerce between them. See castStablePtrToPtr and castPtrToStablePtr in
-- GHC.Stable.
unsafeCoerceAddr x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

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
