{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

{-|
GHC's @DataKinds@ language extension lifts data constructors, natural
numbers, and strings to the type level. This module provides the
primitives needed for working with type-level numbers (the 'Nat' kind),
strings (the 'Symbol' kind), and characters (the 'Char' kind). It also defines the 'TypeError' type
family, a feature that makes use of type-level strings to support user
defined type errors.

For now, this module is the API for working with type-level literals.
However, please note that it is a work in progress and is subject to change.
Once the design of the @DataKinds@ feature is more stable, this will be
considered only an internal GHC module, and the programmer interface for
working with type-level data will be defined in a separate library.

@since 4.6.0.0
-}

module GHC.TypeLits
  ( -- * Kinds
    N.Natural, N.Nat, Symbol  -- Symbol is declared in GHC.Types in package ghc-prim

    -- * Linking type and value level
  , N.KnownNat, natVal, natVal'
  , KnownSymbol, symbolVal, symbolVal'
  , KnownChar, charVal, charVal'
  , N.SomeNat(..), SomeSymbol(..), SomeChar(..)
  , someNatVal, someSymbolVal, someCharVal
  , N.sameNat, sameSymbol, sameChar
  , OrderingI(..)
  , N.cmpNat, cmpSymbol, cmpChar


    -- * Functions on type literals
  , type (N.<=), type (N.<=?), type (N.+), type (N.*), type (N.^), type (N.-)
  , type N.Div, type N.Mod, type N.Log2
  , AppendSymbol
  , N.CmpNat, CmpSymbol, CmpChar
  , ConsSymbol, UnconsSymbol
  , CharToNat, NatToChar

  -- * User-defined type errors
  , TypeError
  , ErrorMessage(..)

  ) where

import GHC.Base(Eq(..), Ord(..), Ordering(..), String, otherwise, withDict)
import GHC.Types(Symbol, Char)
import GHC.TypeError(ErrorMessage(..), TypeError)
import GHC.Num(Integer, fromInteger)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Real(toInteger)
import GHC.Prim(Proxy#)
import Data.Maybe(Maybe(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality((:~:)(Refl))
import Data.Type.Ord(OrderingI(..))
import Unsafe.Coerce(unsafeCoerce)

import GHC.TypeLits.Internal(CmpSymbol, CmpChar)
import qualified GHC.TypeNats as N

--------------------------------------------------------------------------------

-- | This class gives the string associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
--
-- @since 4.7.0.0
class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n

-- | @since 4.7.0.0
natVal :: forall n proxy. N.KnownNat n => proxy n -> Integer
natVal p = toInteger (N.natVal p)

-- | @since 4.7.0.0
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = case symbolSing :: SSymbol n of
                SSymbol x -> x

-- | @since 4.8.0.0
natVal' :: forall n. N.KnownNat n => Proxy# n -> Integer
natVal' p = toInteger (N.natVal' p)

-- | @since 4.8.0.0
symbolVal' :: forall n. KnownSymbol n => Proxy# n -> String
symbolVal' _ = case symbolSing :: SSymbol n of
                SSymbol x -> x


-- | This type represents unknown type-level symbols.
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)
                  -- ^ @since 4.7.0.0

-- | @since 4.16.0.0
class KnownChar (n :: Char) where
  charSing :: SChar n

charVal :: forall n proxy. KnownChar n => proxy n -> Char
charVal _ = case charSing :: SChar n of
                 SChar x -> x

charVal' :: forall n. KnownChar n => Proxy# n -> Char
charVal' _ = case charSing :: SChar n of
                SChar x -> x

data SomeChar = forall n. KnownChar n => SomeChar (Proxy n)

-- | Convert an integer into an unknown type-level natural.
--
-- @since 4.7.0.0
someNatVal :: Integer -> Maybe N.SomeNat
someNatVal n
  | n >= 0        = Just (N.someNatVal (fromInteger n))
  | otherwise     = Nothing

-- | Convert a string into an unknown type-level symbol.
--
-- @since 4.7.0.0
someSymbolVal :: String -> SomeSymbol
someSymbolVal n   = withSSymbol SomeSymbol (SSymbol n) Proxy
{-# NOINLINE someSymbolVal #-}
-- For details see Note [NOINLINE someNatVal] in "GHC.TypeNats"
-- The issue described there applies to `someSymbolVal` as well.

-- | @since 4.7.0.0
instance Eq SomeSymbol where
  SomeSymbol x == SomeSymbol y = symbolVal x == symbolVal y

-- | @since 4.7.0.0
instance Ord SomeSymbol where
  compare (SomeSymbol x) (SomeSymbol y) = compare (symbolVal x) (symbolVal y)

-- | @since 4.7.0.0
instance Show SomeSymbol where
  showsPrec p (SomeSymbol x) = showsPrec p (symbolVal x)

-- | @since 4.7.0.0
instance Read SomeSymbol where
  readsPrec p xs = [ (someSymbolVal a, ys) | (a,ys) <- readsPrec p xs ]


-- | Convert a character into an unknown type-level char.
--
-- @since 4.16.0.0
someCharVal :: Char -> SomeChar
someCharVal n   = withSChar SomeChar (SChar n) Proxy
{-# NOINLINE someCharVal #-}

instance Eq SomeChar where
  SomeChar x == SomeChar y = charVal x == charVal y

instance Ord SomeChar where
  compare (SomeChar x) (SomeChar y) = compare (charVal x) (charVal y)

instance Show SomeChar where
  showsPrec p (SomeChar x) = showsPrec p (charVal x)

instance Read SomeChar where
  readsPrec p xs = [ (someCharVal a, ys) | (a,ys) <- readsPrec p xs ]

--------------------------------------------------------------------------------

-- | Concatenation of type-level symbols.
--
-- @since 4.10.0.0
type family AppendSymbol (m ::Symbol) (n :: Symbol) :: Symbol

-- Char-related type families

-- | Extending a type-level symbol with a type-level character
--
-- @since 4.16.0.0
type family ConsSymbol (a :: Char) (b :: Symbol) :: Symbol

-- | This type family yields type-level `Just` storing the first character
-- of a symbol and its tail if it is defined and `Nothing` otherwise.
--
-- @since 4.16.0.0
type family UnconsSymbol (a :: Symbol) :: Maybe (Char, Symbol)

-- | Convert a character to its Unicode code point (cf. `Data.Char.ord`)
--
-- @since 4.16.0.0
type family CharToNat (c :: Char) :: N.Nat

-- | Convert a Unicode code point to a character (cf. `Data.Char.chr`)
--
-- @since 4.16.0.0
type family NatToChar (n :: N.Nat) :: Char

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level symbols, or 'Nothing'.
--
-- @since 4.7.0.0
sameSymbol :: (KnownSymbol a, KnownSymbol b) =>
              proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameSymbol x y
  | symbolVal x == symbolVal y  = Just (unsafeCoerce Refl)
  | otherwise                   = Nothing


-- | We either get evidence that this function was instantiated with the
-- same type-level characters, or 'Nothing'.
--
-- @since 4.16.0.0
sameChar :: (KnownChar a, KnownChar b) =>
              proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameChar x y
  | charVal x == charVal y  = Just (unsafeCoerce Refl)
  | otherwise                = Nothing

-- | Like 'sameSymbol', but if the symbols aren't equal, this additionally
-- provides proof of LT or GT.
--
-- @since 4.16.0.0
cmpSymbol :: forall a b proxy1 proxy2. (KnownSymbol a, KnownSymbol b)
          => proxy1 a -> proxy2 b -> OrderingI a b
cmpSymbol x y = case compare (symbolVal x) (symbolVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpSymbol a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpSymbol a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpSymbol a b :~: 'GT) of
    Refl -> GTI

-- | Like 'sameChar', but if the Chars aren't equal, this additionally
-- provides proof of LT or GT.
--
-- @since 4.16.0.0
cmpChar :: forall a b proxy1 proxy2. (KnownChar a, KnownChar b)
        => proxy1 a -> proxy2 b -> OrderingI a b
cmpChar x y = case compare (charVal x) (charVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpChar a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpChar a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpChar a b :~: 'GT) of
    Refl -> GTI


--------------------------------------------------------------------------------
-- PRIVATE:

newtype SSymbol (s :: Symbol) = SSymbol String

-- See Note [withDict] in "GHC.Tc.Instance.Class" in GHC
withSSymbol :: forall a b.
               (KnownSymbol a => Proxy a -> b)
            -> SSymbol a      -> Proxy a -> b
withSSymbol f x y = withDict @(KnownSymbol a) x f y

newtype SChar (s :: Char) = SChar Char

-- See Note [withDict] in "GHC.Tc.Instance.Class" in GHC
withSChar :: forall a b.
             (KnownChar a => Proxy a -> b)
            -> SChar a      -> Proxy a -> b
withSChar f x y = withDict @(KnownChar a) x f y
