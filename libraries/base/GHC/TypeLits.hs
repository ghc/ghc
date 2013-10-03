{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for working with type-level naturals should be defined in a separate library.

/Since: 4.6.0.0/
-}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol

    -- * Linking type and value level
  , KnownNat(..), KnownSymbol(..)
  , SomeNat(..), SomeSymbol(..)
  , someNatVal, someSymbolVal

    -- * Functions on type nats
  , type (<=), type (<=?), type (+), type (*), type (^), type (-)

  ) where

import GHC.Base(Eq(..), Ord(..), Bool(True), otherwise)
import GHC.Num(Integer)
import GHC.Base(String)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Prim(magicSingI)
import Data.Maybe(Maybe(..))
import Data.Proxy(Proxy(..))

-- | (Kind) This is the kind of type-level natural numbers.
data Nat

-- | (Kind) This is the kind of type-level symbols.
data Symbol


--------------------------------------------------------------------------------

-- | This class gives the integer associated with a type-level natural.
-- There are instances of the class for every concrete literal: 0, 1, 2, etc.
class KnownNat (n :: Nat) where
  natVal :: proxy n -> Integer

-- | This class gives the integer associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
class KnownSymbol (n :: Symbol) where
  symbolVal :: proxy n -> String

-- | This type represents unknown type-level natural numbers.
data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)

-- | This type represents unknown type-level symbols.
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)

instance SingI n => KnownNat n where
  natVal _ = case sing :: Sing n of
               SNat x -> x

instance SingI n => KnownSymbol n where
  symbolVal _ = case sing :: Sing n of
                  SSym x -> x

-- | Convert an integer into an unknown type-level natural.
someNatVal :: Integer -> Maybe SomeNat
someNatVal n
  | n >= 0        = Just (forgetSingNat (SNat n))
  | otherwise     = Nothing

-- | Convert a string into an unknown type-level symbol.
someSymbolVal :: String -> SomeSymbol
someSymbolVal n   = forgetSingSymbol (SSym n)

instance Eq SomeNat where
  SomeNat x == SomeNat y = natVal x == natVal y

instance Ord SomeNat where
  compare (SomeNat x) (SomeNat y) = compare (natVal x) (natVal y)

instance Show SomeNat where
  showsPrec p (SomeNat x) = showsPrec p (natVal x)

instance Read SomeNat where
  readsPrec p xs = do (a,ys) <- readsPrec p xs
                      case someNatVal a of
                        Nothing -> []
                        Just n  -> [(n,ys)]


instance Eq SomeSymbol where
  SomeSymbol x == SomeSymbol y = symbolVal x == symbolVal y

instance Ord SomeSymbol where
  compare (SomeSymbol x) (SomeSymbol y) = compare (symbolVal x) (symbolVal y)

instance Show SomeSymbol where
  showsPrec p (SomeSymbol x) = showsPrec p (symbolVal x)

instance Read SomeSymbol where
  readsPrec p xs = [ (someSymbolVal a, ys) | (a,ys) <- readsPrec p xs ]


--------------------------------------------------------------------------------

-- | Comparison of type-level naturals, as a constraint.
type x <= y = (x <=? y) ~ True

-- | Comparison of type-level naturals, as a function.
type family (m :: Nat) <=? (n :: Nat) :: Bool

-- | Addition of type-level naturals.
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat

-- | Subtraction of type-level naturals.
--
-- /Since: 4.7.0.0/
type family (m :: Nat) - (n :: Nat) :: Nat



--------------------------------------------------------------------------------
-- PRIVATE:

-- | This is an internal GHC class.  It has built-in instances in the compiler.
class SingI a where
  sing :: Sing a

-- | This is used only in the type of the internal `SingI` class.
data family      Sing (n :: k)
newtype instance Sing (n :: Nat)    = SNat Integer
newtype instance Sing (n :: Symbol) = SSym String


{- PRIVATE:
The functions below convert a value of type `Sing n` into a dictionary
for `SingI` for `Nat` and `Symbol`.

NOTE: The implementation is a bit of a hack at present,
hence all the very special annotations.  See Note [magicSingIId magic]
for more details.
-}
forgetSingNat :: forall n. Sing (n :: Nat) -> SomeNat
forgetSingNat x = withSingI x it Proxy
  where
  it :: SingI n => Proxy n -> SomeNat
  it = SomeNat

forgetSingSymbol :: forall n. Sing (n :: Symbol) -> SomeSymbol
forgetSingSymbol x = withSingI x it Proxy
  where
  it :: SingI n => Proxy n -> SomeSymbol
  it = SomeSymbol

-- | THIS IS NOT SUPPOSED TO MAKE SENSE.
-- See Note [magicSingIId magic]
{-# NOINLINE withSingI #-}
withSingI :: Sing n -> (SingI n => a) -> a
withSingI x = magicSingI x ((\f -> f) :: () -> ())



