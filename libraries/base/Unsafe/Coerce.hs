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
  , unsafeCoerce#
  ) where

import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base
import GHC.Natural () -- See Note [Depend on GHC.Natural] in GHC.Base
import GHC.Base

import GHC.Types  ( TYPE )

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

-- | This type is treated magically within GHC. Any pattern match of the
-- form @case unsafeEqualityProof of UnsafeRefl -> body@ gets transformed just into @body@.
-- This is ill-typed, but the transformation takes place after type-checking is
-- complete. It is used to implement 'unsafeCoerce'. You probably don't want to
-- use 'UnsafeRefl' in an expression, but you might conceivably want to pattern-match
-- on it. Use 'unsafeEqualityProof' to create one of these.
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
--   case unsafeEqualityProof of UnsafeRefl -> case blah of ...
--
-- which is definitely better.

-- | Coerce a value from one type to another, bypassing the type-checker.
--
-- There are several legitimate ways to use 'unsafeCoerce':
--
--   1. To coerce e.g. @Int@ to @HValue@, put it in a list of @HValue@,
--      and then later coerce it back to @Int@ before using it.
--
--   2. To produce e.g. @(a+b) :~: (b+a)@ from @unsafeCoerce Refl@.
--      Here the two sides really are the same type -- so nothing unsafe is happening
--      -- but GHC is not clever enough to see it.
--
--   3. In @Data.Typeable@ we have
--
--      @
--        eqTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
--                     TypeRep a -> TypeRep b -> Maybe (a :~~: b)
--        eqTypeRep a b
--          | sameTypeRep a b = Just (unsafeCoerce HRefl)
--          | otherwise       = Nothing
--      @
--
--      Here again, the @unsafeCoerce HRefl@ is safe, because the two types really
--      are the same  -- but the proof of that relies on the complex, trusted
--      implementation of @Typeable@.
--
--   4. The "reflection trick", which takes advantanage of the fact that in
--      @class C a where { op :: ty }@, we can safely coerce between @C a@ and @ty@
--      (which have different kinds!) because it's really just a newtype.
--      Note: there is /no guarantee, at all/ that this behavior will be supported
--      into perpetuity.
unsafeCoerce :: forall (a :: Type) (b :: Type) . a -> b
unsafeCoerce x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

-- | Highly, terribly dangerous coercion from one representation type
-- to another. Misuse of this function can invite the garbage collector
-- to trounce upon your data and then laugh in your face. You don't want
-- this function. Really.
unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                        (a :: TYPE r1) (b :: TYPE r2).
                 a -> b
unsafeCoerce# = unsafeCoerce#
