{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}  -- for compiling instances of (==)
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

  ) where

import GHC.Base(Eq(..), Ord(..), Bool(True,False), Ordering(..), otherwise)
import GHC.Types( Nat )
import GHC.Natural(Natural)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Prim(magicDict, Proxy#)
import Data.Maybe(Maybe(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality(type (==), (:~:)(Refl))
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

type family EqNat (a :: Nat) (b :: Nat) where
  EqNat a a = 'True
  EqNat a b = 'False
type instance a == b = EqNat a b

--------------------------------------------------------------------------------

infix  4 <=?, <=
infixl 6 +, -
infixl 7 *
infixr 8 ^

-- | Comparison of type-level naturals, as a constraint.
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
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat

-- | Subtraction of type-level naturals.
--
-- @since 4.7.0.0
type family (m :: Nat) - (n :: Nat) :: Nat

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
