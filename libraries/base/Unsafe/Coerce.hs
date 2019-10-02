-- We don't to strictness analysis on this file to avoid turning loopy unsafe
-- equality terms below to actual loops. See Note [unsafeCoerce magic] below for
-- how they're supposed to be compiled.
{-# OPTIONS_GHC -fno-strictness #-}

{-# LANGUAGE Unsafe, NoImplicitPrelude, MagicHash, GADTs, TypeApplications,
             ScopedTypeVariables, TypeOperators, KindSignatures, PolyKinds,
             StandaloneKindSignatures #-}

module Unsafe.Coerce
  ( unsafeCoerce
  , unsafeEqualityProof
  , UnsafeEquality (..)
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
