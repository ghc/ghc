{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for working with type-level naturals should be defined in a separate library.

@since base-4.10.0.0
-}

module GHC.Internal.TypeNats
  ( -- * Nat Kind
    Natural -- declared in GHC.Num.Natural in package ghc-bignum
  , Nat
    -- * Linking type and value level
  , KnownNat(natSing), natVal, natVal'
  , SomeNat(..)
  , someNatVal
  , sameNat
  , decideNat
    -- ** Singleton values
  , SNat (UnsafeSNat)
      -- We export a pattern synonym instead of the real constructor:
      -- See Note [Preventing unsafe coercions for singleton types].
  , pattern SNat
  , fromSNat
  , withSomeSNat
  , withKnownNat
  , unsafeWithSNatCo

    -- * Functions on type literals
  , type (<=), type (<=?), type (+), type (*), type (^), type (-)
  , CmpNat
  , cmpNat
  , Div, Mod, Log2

  ) where

import GHC.Internal.Base( Eq(..), Functor(..), Ord(..), WithDict(..), (.), otherwise
               , Void, errorWithoutStackTrace, (++))
import GHC.Types
import GHC.Num.Natural(Natural)
import GHC.Internal.Show(Show(..), appPrec, appPrec1, showParen, showString)
import GHC.Internal.Read(Read(..))
import GHC.Prim(Proxy#)
import GHC.Internal.Data.Either(Either(..))
import GHC.Internal.Data.Maybe(Maybe(..))
import GHC.Internal.Data.Proxy (Proxy(..))
import GHC.Internal.Data.Type.Coercion (Coercion(..), TestCoercion(..))
import GHC.Internal.Data.Type.Equality((:~:)(Refl), TestEquality(..))
import GHC.Internal.Data.Type.Ord(OrderingI(..), type (<=), type (<=?))
import GHC.Internal.Unsafe.Coerce(unsafeCoerce)

import GHC.Internal.TypeNats.Internal(CmpNat)

-- | A type synonym for 'Natural'.
--
-- Previously, this was an opaque data type, but it was changed to a type
-- synonym.
--
-- @since base-4.16.0.0

type Nat = Natural
--------------------------------------------------------------------------------

-- | This class gives the integer associated with a type-level natural.
-- There are instances of the class for every concrete literal: 0, 1, 2, etc.
--
-- @since base-4.7.0.0
class KnownNat (n :: Nat) where
  natSing :: SNat n

-- | @since base-4.10.0.0
natVal :: forall n proxy. KnownNat n => proxy n -> Natural
natVal _ = case natSing :: SNat n of
             UnsafeSNat x -> x

-- | @since base-4.10.0.0
natVal' :: forall n. KnownNat n => Proxy# n -> Natural
natVal' _ = case natSing :: SNat n of
             UnsafeSNat x -> x

-- | This type represents unknown type-level natural numbers.
--
-- @since base-4.10.0.0
data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)

-- | Convert an integer into an unknown type-level natural.
--
-- @since base-4.10.0.0
someNatVal :: Natural -> SomeNat
someNatVal n = withSomeSNat n (\(sn :: SNat n) ->
               withKnownNat sn (SomeNat @n Proxy))

{-
Note [NOINLINE withSomeSNat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function

    withSomeSNat :: forall rep (r :: TYPE rep).
                    Natural -> (forall k. SNat k -> r) -> r

converts a `Natural` number to a singleton natural `SNat k`, where the `k` is
locally quantified in a continuation (hence the `forall k`). The local
quantification is important: we can manufacture an `SNat k` value, but it can
never be confused with (say) an `SNat 33` value, because we should never be
able to prove that `k ~ 33`. Moreover, if we call `withSomeSNat` twice, we'll
get an `SNat k1` value and an `SNat k2` value, but again we can't confuse them.
`SNat` is a singleton type!

But how to implement `withSomeSNat`? We have no way to make up a fresh type
variable. To do that we need `runExists`: see #19675.

Lacking `runExists`, we use a trick to implement `withSomeSNat`: instead of
generating a "fresh" type for each use of `withSomeSNat`, we simply use GHC's
placeholder type `Any` (of kind `Nat`), thus (in Core):

  withSomeSNat n f = f @T (UnsafeSNat @T n)
    where type T = Any @Nat

***  BUT we must mark `withSomeSNat` as NOINLINE! ***
(And the same for withSomeSSymbol and withSomeSChar in GHC.TypeLits.)

If we inline it we'll lose the type distinction between separate calls (those
"fresh" type variables just turn into `T`). And that can interact badly with
GHC's type-class specialiser. Consider this definition, where
`foo :: KnownNat n => blah`:

  ex :: Natural
  ex = withSomeSNat 1 (\(s1 :: SNat one) -> withKnownNat @one s1 $
       withSomeSNat 2 (\(s2 :: SNat two) -> withKnownNat @two s2 $
       foo @one ... + foo @two ...))

In the last line we have in scope two distinct dictionaries of types
`KnownNat one` and `KnownNat two`. The calls `foo @one` and `foo @two` each pick
out one of those dictionaries to pass to `foo`.

But if we inline `withSomeSNat` we'll get (switching to Core):

  ex = withKnownNat @T (UnsafeSNat @T 1) (\(kn1 :: KnownNat T) ->
       withKnownNat @T (UnsafeSNat @T 2) (\(kn2 :: KnownNat T) ->
       foo @T kn1 ... + foo @T kn2 ...))
    where type T = Any Nat

We are now treading on thin ice. We have two dictionaries `kn1` and `kn2`, both
of type `KnownNat T`, but with different implementations. GHC may specialise
`foo` at type `T` using one of these dictionaries and use that same
specialisation for the other. See #16586 for more examples of where something
like this has actually happened.

`KnownNat` should be a singleton type, but if we allow `withSomeSNat` to inline
it won't be a singleton type any more. We have lost the "fresh type variable".

TL;DR. We avoid this problem by making the definition of `withSomeSNat` opaque,
using an `NOINLINE` pragma. When we get `runExists` (#19675) we will be able to
stop using this hack.
-}


-- | @since base-4.7.0.0
instance Eq SomeNat where
  SomeNat x == SomeNat y = natVal x == natVal y

-- | @since base-4.7.0.0
instance Ord SomeNat where
  compare (SomeNat x) (SomeNat y) = compare (natVal x) (natVal y)

-- | @since base-4.7.0.0
instance Show SomeNat where
  showsPrec p (SomeNat x) = showsPrec p (natVal x)

-- | @since base-4.7.0.0
instance Read SomeNat where
  readsPrec p xs = do (a,ys) <- readsPrec p xs
                      [(someNatVal a, ys)]

--------------------------------------------------------------------------------

infixl 6 +, -
infixl 7 *, `Div`, `Mod`
infixr 8 ^

-- | Addition of type-level naturals.
--
-- @since base-4.7.0.0
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
--
-- @since base-4.7.0.0
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
--
-- @since base-4.7.0.0
type family (m :: Nat) ^ (n :: Nat) :: Nat

-- | Subtraction of type-level naturals.
--
-- @since base-4.7.0.0
type family (m :: Nat) - (n :: Nat) :: Nat

-- | Division (round down) of natural numbers.
-- @Div x 0@ is undefined (i.e., it cannot be reduced).
--
-- @since base-4.11.0.0
type family Div (m :: Nat) (n :: Nat) :: Nat

-- | Modulus of natural numbers.
-- @Mod x 0@ is undefined (i.e., it cannot be reduced).
--
-- @since base-4.11.0.0
type family Mod (m :: Nat) (n :: Nat) :: Nat

-- | Log base 2 (round down) of natural numbers.
-- @Log 0@ is undefined (i.e., it cannot be reduced).
--
-- @since base-4.11.0.0
type family Log2 (m :: Nat) :: Nat

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level numbers, or 'Nothing'.
--
-- @since base-4.7.0.0
sameNat :: forall a b proxy1 proxy2.
           (KnownNat a, KnownNat b) =>
           proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameNat _ _ = testEquality (natSing @a) (natSing @b)

-- | We either get evidence that this function was instantiated with the
-- same type-level numbers, or that the type-level numbers are distinct.
--
-- @since base-4.19.0.0
decideNat :: forall a b proxy1 proxy2.
           (KnownNat a, KnownNat b) =>
           proxy1 a -> proxy2 b -> Either (a :~: b -> Void) (a :~: b)
decideNat _ _ = decNat (natSing @a) (natSing @b)

-- Not exported: See [Not exported decNat, decSymbol and decChar]
decNat :: SNat a -> SNat b -> Either (a :~: b -> Void) (a :~: b)
decNat (UnsafeSNat x) (UnsafeSNat y)
  | x == y    = Right (unsafeCoerce Refl)
  | otherwise = Left (\Refl -> errorWithoutStackTrace ("decideNat: Impossible equality proof " ++ show x ++ " :~: " ++ show y))

{-
Note [Not exported decNat, decSymbol and decChar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The decNat, decSymbol and decChar are not (yet) exported.

There are two development paths:
1. export them.
2. Add `decideEquality :: f a -> f b -> Either (a :~: b -> Void) (a :~: b)`
   to the `Data.Type.Equality.TestEquality` typeclass.

The second option looks nicer given the current base API:
there aren't `eqNat :: SNat a -> SNat b -> Maybe (a :~: b)` like functions,
they are abstracted by `TestEquality` typeclass.

Also TestEquality class has a law that testEquality result
should be Just Refl iff the types applied to are equal:

testEquality (x :: f a) (y :: f b) = Just Refl  <=> a = b

As consequence we have that testEquality should be Nothing
iff the types applied are inequal:

testEquality (x :: f a) (y :: f b) = Nothing   <=> a /= b

And the decideEquality would enforce that.

However, adding a new method is a breaking change,
as default implementation cannot be (safely) provided.
Also there are unlawful instances of `TestEquality` out there,
(e.g. https://hackage.haskell.org/package/parameterized-utils Index instance
      https://hackage.haskell.org/package/witness various types)
which makes adding unsafe default implementation a bad idea.

Adding own typeclass:

class TestEquality f => DecideEquality f where
  decideEquality :: f a -> f b -> Either (a :~: b -> Void) (a :~: b)

is bad design, as `TestEquality` already implies that it should be possible.
In other words, every f with (lawful) `TestEquality` instance should have
`DecideEquality` instance as well.

We hold on doing either 1. or 2. yet, as doing 2. is "harder",
but if it is done eventually, doing 1. is pointless.
In other words the paths can be thought as mutually exclusive.

Fortunately the dec* functions can be simulated using decide* variants
if needed, so there is no hurry to commit to either development paths.

-}

-- | Like 'sameNat', but if the numbers aren't equal, this additionally
-- provides proof of LT or GT.
--
-- @since base-4.16.0.0
cmpNat :: forall a b proxy1 proxy2. (KnownNat a, KnownNat b)
       => proxy1 a -> proxy2 b -> OrderingI a b
cmpNat x y = case compare (natVal x) (natVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpNat a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpNat a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpNat a b :~: 'GT) of
    Refl -> GTI



--------------------------------------------------------------------------------
-- Singleton values

-- | A value-level witness for a type-level natural number. This is commonly
-- referred to as a /singleton/ type, as for each @n@, there is a single value
-- that inhabits the type @'SNat' n@ (aside from bottom).
--
-- The definition of 'SNat' is intentionally left abstract. To obtain an 'SNat'
-- value, use one of the following:
--
-- 1. The 'natSing' method of 'KnownNat'.
--
-- 2. The @SNat@ pattern synonym.
--
-- 3. The 'withSomeSNat' function, which creates an 'SNat' from a 'Natural'
--    number.
--
-- @since base-4.18.0.0
newtype SNat (n :: Nat) = UnsafeSNat_ Natural
-- nominal role: See Note [Preventing unsafe coercions for singleton types]
type role SNat nominal

{-
Note [Preventing unsafe coercions for singleton types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a singleton type like this one:

  newtype SNat (n :: Nat) = UnsafeSNat_ Natural

We operate under the fiction that a (non-bottom) value
``UnsafeSNat_ v :: SNat n`` also contains evidence that the value
``v :: Natural`` is the same as the type ``n :: Natural``.
Such evidence can only be safely ``coerce``d to evidence that the
value ``v`` is the same as some other type ``n2`` if we know that
``n ~ n2``, at nominal role. (This is #23454.)

So, to preserve that fiction, we:

 1. Provide a role annotation indicating that ``SNat``'s type argument has
    nominal role rather than the phantom role that would be inferred.
 2. Ensure that the real newtype constructor ``UnsafeSNat_`` is not
    exported even from ghc-internal.  Whenever that constructor is in
    scope, typechecking of ``coerce`` will ignore the role annotation
    and just unwrap the newtype.

But users may wish to write functions like this one (#23478):

  plusSNat :: SNat a -> SNat b -> SNat (a + b)

We could ask them to use ``unsafeCoerce``, but it seems a bit more polite
to provide a pattern synonym ``UnsafeSNat :: forall n. Natural -> SNat n``
as an escape hatch (exported from ghc-internal only), so that such a function
can be written as follows:

  plusSNat (UnsafeSNat x) (UnsafeSNat y) = UnsafeSNat (x + y)

Crucially, these pattern synonyms (unlike real newtype constructors) do not
cause ``coerce`` to bypass our role annotation when they are in scope.

To allow casting data structures containing SNats, we provide a
further escape hatch in ``unsafeWithSNatCo``, which enables ``coerce`` to
bypass our role annotation on ``SNat``, but /only within its argument/:

  unsafeWithSNatCo
    :: forall r. ((forall n. Coercible (SNat n) Natural) => r) -> r


The above reasoning applies identically for the other singleton types
'SChar' and 'SSymbol' as well.
-}


-- | A explicitly bidirectional pattern synonym relating an 'SNat' to a
-- 'KnownNat' constraint.
--
-- As an __expression__: Constructs an explicit @'SNat' n@ value from an
-- implicit @'KnownNat' n@ constraint:
--
-- @
-- SNat @n :: 'KnownNat' n => 'SNat' n
-- @
--
-- As a __pattern__: Matches on an explicit @'SNat' n@ value bringing
-- an implicit @'KnownNat' n@ constraint into scope:
--
-- @
-- f :: 'SNat' n -> ..
-- f SNat = {- KnownNat n in scope -}
-- @
--
-- @since base-4.18.0.0
pattern SNat :: forall n. () => KnownNat n => SNat n
pattern SNat <- (knownNatInstance -> KnownNatInstance)
  where SNat = natSing
{-# COMPLETE SNat #-}

-- An internal data type that is only used for defining the SNat pattern
-- synonym.
data KnownNatInstance (n :: Nat) where
  KnownNatInstance :: KnownNat n => KnownNatInstance n

-- An internal function that is only used for defining the SNat pattern
-- synonym.
knownNatInstance :: SNat n -> KnownNatInstance n
knownNatInstance sn = withKnownNat sn KnownNatInstance

-- | A pattern that can be used to manipulate the
-- 'Natural' that an @SNat n@ contains under the hood.
--
-- When using this pattern to construct an @SNat n@, the actual
-- @Natural@ being stored in the @SNat n@ /must/ be equal to @n@.
-- The compiler will not help you verify this, hence the \'unsafe\' name.
pattern UnsafeSNat :: forall n. Natural -> SNat n
pattern UnsafeSNat guts = UnsafeSNat_ guts
{-# COMPLETE UnsafeSNat #-}

-- | 'unsafeWithSNatCo' allows uses of @coerce@ in its argument to see the
-- real representation of @SNat n@, without undermining the type-safety of
-- @coerce@ elsewhere in the module.
--
-- See also the documentation for 'UnsafeSNat'.
unsafeWithSNatCo
  :: forall r. ((forall n. Coercible (SNat n) Natural) => r) -> r
unsafeWithSNatCo v = v

-- | @since base-4.19.0.0
instance Eq (SNat n) where
  _ == _ = True

-- | @since base-4.19.0.0
instance Ord (SNat n) where
  compare _ _ = EQ

-- | @since base-4.18.0.0
instance Show (SNat n) where
  showsPrec p (UnsafeSNat n)
    = showParen (p > appPrec)
      ( showString "SNat @"
        . showsPrec appPrec1 n
      )

-- | @since base-4.18.0.0
instance TestEquality SNat where
  testEquality a b = case decNat a b of
    Right x -> Just x
    Left _  -> Nothing

-- | @since base-4.18.0.0
instance TestCoercion SNat where
  testCoercion x y = fmap (\Refl -> Coercion) (testEquality x y)

-- | Return the 'Natural' number corresponding to @n@ in an @'SNat' n@ value.
--
-- @since base-4.18.0.0
fromSNat :: SNat n -> Natural
fromSNat (UnsafeSNat n) = n

-- | Convert an explicit @'SNat' n@ value into an implicit @'KnownNat' n@
-- constraint.
--
-- @since base-4.18.0.0
withKnownNat :: forall n rep (r :: TYPE rep).
                SNat n -> (KnownNat n => r) -> r
withKnownNat = withDict @(KnownNat n)
-- See Note [withDict] in "GHC.Tc.Instance.Class" in GHC

-- | Convert a 'Natural' number into an @'SNat' n@ value, where @n@ is a fresh
-- type-level natural number.
--
-- @since base-4.18.0.0
withSomeSNat :: forall rep (r :: TYPE rep).
                Natural -> (forall n. SNat n -> r) -> r
withSomeSNat n k = k (UnsafeSNat n)
{-# NOINLINE withSomeSNat #-} -- See Note [NOINLINE withSomeSNat]
