{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}

{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for working with type-level naturals should be defined in a separate library.

@since 4.10.0.0
-}

module GHC.TypeNats
  ( -- * Nat Kind
    Nat -- declared in GHC.Types in package ghc-prim

    -- * Linking type and value level
  , KnownNat, natVal, natVal'
  , SomeNat(..)
  , someNatVal
  , sameNat

    -- * Functions on type literals
  , type (<=), type (<=?), type (+), type (*), type (^), type (-)
  , CmpNat
  , Div, Mod, Log2

  ) where

import GHC.Base(Eq(..), Ord(..), Bool(True), Ordering(..), otherwise)
import GHC.Types( Nat )
import GHC.Natural(Natural)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Prim(magicDict, Proxy#)
import Data.Maybe(Maybe(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality((:~:)(Refl))
import Unsafe.Coerce(unsafeCoerce)

--------------------------------------------------------------------------------

-- | This class gives the integer associated with a type-level natural.
-- There are instances of the class for every concrete literal: 0, 1, 2, etc.
--
-- @since 4.7.0.0
class KnownNat (n :: Nat) where
  natSing :: SNat n

-- | @since 4.10.0.0
natVal :: forall n proxy. KnownNat n => proxy n -> Natural
natVal _ = case natSing :: SNat n of
             SNat x -> x

-- | @since 4.10.0.0
natVal' :: forall n. KnownNat n => Proxy# n -> Natural
natVal' _ = case natSing :: SNat n of
             SNat x -> x

-- | This type represents unknown type-level natural numbers.
--
-- @since 4.10.0.0
data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)

-- | Convert an integer into an unknown type-level natural.
--
-- @since 4.10.0.0
someNatVal :: Natural -> SomeNat
someNatVal n = withSNat SomeNat (SNat n) Proxy
{-# NOINLINE someNatVal #-} -- See Note [NOINLINE someNatVal]

{- Note [NOINLINE someNatVal]

`someNatVal` converts a natural number to an existentially quantified
dictionary for `KnowNat` (aka `SomeNat`).  The existential quantification
is very important, as it captures the fact that we don't know the type
statically, although we do know that it exists.   Because this type is
fully opaque, we should never be able to prove that it matches anything else.
This is why coherence should still hold:  we can manufacture a `KnownNat k`
dictionary, but it can never be confused with a `KnownNat 33` dictionary,
because we should never be able to prove that `k ~ 33`.

But how to implement `someNatVal`?  We can't quite implement it "honestly"
because `SomeNat` needs to "hide" the type of the newly created dictionary,
but we don't know what the actual type is!  If `someNatVal` was built into
the language, then we could manufacture a new skolem constant,
which should behave correctly.

Since extra language constructors have additional maintenance costs,
we use a trick to implement `someNatVal` in the library.  The idea is that
instead of generating a "fresh" type for each use of `someNatVal`, we simply
use GHC's placeholder type `Any` (of kind `Nat`). So, the elaborated
version of the code is:

  someNatVal n = withSNat @T (SomeNat @T) (SNat @T n) (Proxy @T)
    where type T = Any Nat

After inlining and simplification, this ends up looking something like this:

  someNatVal n = SomeNat @T (KnownNat @T (SNat @T n)) (Proxy @T)
    where type T = Any Nat

`KnownNat` is the constructor for dictionaries for the class `KnownNat`.
See Note [magicDictId magic] in "basicType/MkId.hs" for details on how
we actually construct the dictionry.

Note that using `Any Nat` is not really correct, as multilple calls to
`someNatVal` would violate coherence:

  type T = Any Nat

  x = SomeNat @T (KnownNat @T (SNat @T 1)) (Proxy @T)
  y = SomeNat @T (KnownNat @T (SNat @T 2)) (Proxy @T)

Note that now the code has two dictionaries with the same type, `KnownNat Any`,
but they have different implementations, namely `SNat 1` and `SNat 2`.  This
is not good, as GHC assumes coherence, and it is free to interchange
dictionaries of the same type, but in this case this would produce an incorrect
result.   See #16586 for examples of this happening.

We can avoid this problem by making the definition of `someNatVal` opaque
and we do this by using a `NOINLINE` pragma.  This restores coherence, because
GHC can only inspect the result of `someNatVal` by pattern matching on the
existential, which would generate a new type.  This restores correctness,
at the cost of having a little more allocation for the `SomeNat` constructors.
-}



-- | @since 4.7.0.0
instance Eq SomeNat where
  SomeNat x == SomeNat y = natVal x == natVal y

-- | @since 4.7.0.0
instance Ord SomeNat where
  compare (SomeNat x) (SomeNat y) = compare (natVal x) (natVal y)

-- | @since 4.7.0.0
instance Show SomeNat where
  showsPrec p (SomeNat x) = showsPrec p (natVal x)

-- | @since 4.7.0.0
instance Read SomeNat where
  readsPrec p xs = do (a,ys) <- readsPrec p xs
                      [(someNatVal a, ys)]

--------------------------------------------------------------------------------

infix  4 <=?, <=
infixl 6 +, -
infixl 7 *, `Div`, `Mod`
infixr 8 ^

-- | Comparison of type-level naturals, as a constraint.
--
-- @since 4.7.0.0
type x <= y = (x <=? y) ~ 'True

-- | Comparison of type-level naturals, as a function.
--
-- @since 4.7.0.0
type family CmpNat    (m :: Nat)    (n :: Nat)    :: Ordering

{- | Comparison of type-level naturals, as a function.
NOTE: The functionality for this function should be subsumed
by 'CmpNat', so this might go away in the future.
Please let us know, if you encounter discrepancies between the two. -}
type family (m :: Nat) <=? (n :: Nat) :: Bool

-- | Addition of type-level naturals.
--
-- @since 4.7.0.0
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
--
-- @since 4.7.0.0
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
--
-- @since 4.7.0.0
type family (m :: Nat) ^ (n :: Nat) :: Nat

-- | Subtraction of type-level naturals.
--
-- @since 4.7.0.0
type family (m :: Nat) - (n :: Nat) :: Nat

-- | Division (round down) of natural numbers.
-- @Div x 0@ is undefined (i.e., it cannot be reduced).
--
-- @since 4.11.0.0
type family Div (m :: Nat) (n :: Nat) :: Nat

-- | Modulus of natural numbers.
-- @Mod x 0@ is undefined (i.e., it cannot be reduced).
--
-- @since 4.11.0.0
type family Mod (m :: Nat) (n :: Nat) :: Nat

-- | Log base 2 (round down) of natural numbers.
-- @Log 0@ is undefined (i.e., it cannot be reduced).
--
-- @since 4.11.0.0
type family Log2 (m :: Nat) :: Nat

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level numbers, or 'Nothing'.
--
-- @since 4.7.0.0
sameNat :: (KnownNat a, KnownNat b) =>
           Proxy a -> Proxy b -> Maybe (a :~: b)
sameNat x y
  | natVal x == natVal y = Just (unsafeCoerce Refl)
  | otherwise            = Nothing

--------------------------------------------------------------------------------
-- PRIVATE:

newtype SNat    (n :: Nat)    = SNat    Natural

data WrapN a b = WrapN (KnownNat    a => Proxy a -> b)

-- See Note [magicDictId magic] in "basicType/MkId.hs"
withSNat :: (KnownNat a => Proxy a -> b)
         -> SNat a      -> Proxy a -> b
withSNat f x y = magicDict (WrapN f) x y
