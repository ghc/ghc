{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for workin with type-level naturals should be defined in a separate library. -}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol

    -- * Singleton types
  , TNat(..), TSymbol(..)

    -- * Linking type and value level
  , NatI(..), SymbolI(..)

    -- * Working with singletons
  , tNatInteger, withTNat, tNatThat
  , tSymbolString, withTSymbol, tSymbolThat

    -- * Functions on type nats
  , type (<=), type (+), type (*), type (^)

    -- * Destructing type-nats.
  , isZero, IsZero(..)
  , isEven, IsEven(..)
  ) where

import GHC.Base(Bool(..), ($), otherwise, (==), (.))
import GHC.Num(Integer, (-))
import GHC.Base(String)
import GHC.Read(Read(..))
import GHC.Show(Show(..))
import Unsafe.Coerce(unsafeCoerce)
import Data.Bits(testBit,shiftR)
import Data.Maybe(Maybe(..))
import Data.List((++))

-- | This is the *kind* of type-level natural numbers.
data Nat

-- | This is the *kind* of type-level symbols.
data Symbol


--------------------------------------------------------------------------------

-- | The type @TNat n@ is m \"singleton\" type containing only the value @n@.
-- (Technically, there is also a bottom element).
-- This type relates type-level naturals to run-time values.
newtype TNat (n :: Nat) = TNat Integer

-- | The type @TSymbol s@ is m \"singleton\" type containing only the value @s@.
-- (Technically, there is also a bottom element).
-- This type relates type-level symbols to run-time values.
newtype TSymbol (n :: Symbol) = TSymbol String



--------------------------------------------------------------------------------

-- | The class 'NatI' provides a \"smart\" constructor for values
-- of type @Nat n@.  There are built-in instances for all natural numbers
-- that fit in a 'Word'.  The 'Word' restriction can be lifted but that
-- would require a bunch of code in "deSugar/DsBinds" to be monadified,
-- because making integer expression is a monadic operation.  Not hard,
-- but not yet done.
--
-- NOTE: The instances for 'NatI' are provided directly by GHC.
-- The built-in instances use the number corresponding to the instance
-- as evidence.  This works because of the following two details about GHC:
--   * The "dictionary" for classes with a single method is the method itself,
--     so GHC simply coerces the dictionary into the value, and
--   * Newtype use the same representation as their definition types.
-- (This is a bit of a hack but it seems to work pretty well.
--  It is also possible to implement the same API in a different way.)

class NatI (n :: Nat) where

  -- | The only defined element of type @TNat n@.
  tNat :: TNat n



class SymbolI (n :: Symbol) where

  -- | The only defined element of type @TSymbol n@.
  tSymbol :: TSymbol n




-- | Comparsion of type-level naturals.
class (m :: Nat) <= (n :: Nat)

-- | Addition of type-level naturals.
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat


--------------------------------------------------------------------------------

{-# INLINE tNatInteger #-}
{-# INLINE withTNat #-}
{-# INLINE tNatThat #-}


tNatInteger :: TNat n -> Integer
tNatInteger (TNat n) = n

withTNat :: NatI n => (TNat n -> a) -> a
withTNat f = f tNat

tNatThat :: NatI n => (Integer -> Bool) -> Maybe (TNat n)
tNatThat p = withTNat $ \x -> if p (tNatInteger x) then Just x else Nothing

instance Show (TNat n) where
  showsPrec p = showsPrec p . tNatInteger

instance NatI n => Read (TNat n) where
  readsPrec p cs = do (x,ys) <- readsPrec p cs
                      case tNatThat (== x) of
                        Just y  -> [(y,ys)]
                        Nothing -> []


--------------------------------------------------------------------------------

tSymbolString :: TSymbol s -> String
tSymbolString (TSymbol s) = s

withTSymbol :: SymbolI s => (TSymbol s -> a) -> a
withTSymbol f = f tSymbol

tSymbolThat :: SymbolI s => (String -> Bool) -> Maybe (TSymbol s)
tSymbolThat p = withTSymbol $ \x -> if p (tSymbolString x) then Just x
                                                           else Nothing

instance Show (TSymbol n) where
  showsPrec p = showsPrec p . tSymbolString

instance SymbolI n => Read (TSymbol n) where
  readsPrec p cs = do (x,ys) <- readsPrec p cs
                      case tSymbolThat (== x) of
                        Just y  -> [(y,ys)]
                        Nothing -> []



--------------------------------------------------------------------------------

data IsZero :: Nat -> * where
  IsZero :: IsZero 0
  IsSucc :: !(TNat n) -> IsZero (n + 1)

isZero :: TNat n -> IsZero n
isZero (TNat n) | n == 0    = unsafeCoerce IsZero
                | otherwise = unsafeCoerce (IsSucc (TNat (n-1)))

instance Show (IsZero n) where
  show IsZero     = "0"
  show (IsSucc n) = "(" ++ show n ++ " + 1)"

data IsEven :: Nat -> * where
  IsEvenZero :: IsEven 0
  IsEven     :: !(TNat (n+1)) -> IsEven (2 * n + 2)
  IsOdd      :: !(TNat n)     -> IsEven (2 * n + 1)

isEven :: TNat n -> IsEven n
isEven (TNat n) | n == 0      = unsafeCoerce IsEvenZero
                | testBit n 0 = unsafeCoerce (IsOdd  (TNat (n `shiftR` 1)))
                | otherwise   = unsafeCoerce (IsEven (TNat (n `shiftR` 1)))

instance Show (IsEven n) where
  show IsEvenZero = "0"
  show (IsEven x) = "(2 * " ++ show x ++ ")"
  show (IsOdd  x) = "(2 * " ++ show x ++ " + 1)"



