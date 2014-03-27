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
  , KnownNat, natVal
  , KnownSymbol, symbolVal
  , SomeNat(..), SomeSymbol(..)
  , someNatVal, someSymbolVal
  , sameNat, sameSymbol


    -- * Functions on type literals
  , type (<=), type (<=?), type (+), type (*), type (^), type (-)
  , CmpNat, CmpSymbol

  ) where

import GHC.Base(Eq(..), Ord(..), Bool(True,False), Ordering(..), otherwise)
import GHC.Num(Integer)
import GHC.Base(String)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Prim(magicDict)
import Data.Maybe(Maybe(..))
import Data.Proxy(Proxy(..))
import Data.Type.Equality(type (==), (:~:)(Refl))
import Unsafe.Coerce(unsafeCoerce)

-- | (Kind) This is the kind of type-level natural numbers.
data Nat

-- | (Kind) This is the kind of type-level symbols.
data Symbol


--------------------------------------------------------------------------------

-- | This class gives the integer associated with a type-level natural.
-- There are instances of the class for every concrete literal: 0, 1, 2, etc.
--
-- /Since: 4.7.0.0/
class KnownNat (n :: Nat) where
  natSing :: SNat n

-- | This class gives the integer associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
--
-- /Since: 4.7.0.0/
class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n

-- | /Since: 4.7.0.0/
natVal :: forall n proxy. KnownNat n => proxy n -> Integer
natVal _ = case natSing :: SNat n of
             SNat x -> x

-- | /Since: 4.7.0.0/
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = case symbolSing :: SSymbol n of
                SSymbol x -> x



-- | This type represents unknown type-level natural numbers.
data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)
                  -- ^ /Since: 4.7.0.0/

-- | This type represents unknown type-level symbols.
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)
                  -- ^ /Since: 4.7.0.0/

-- | Convert an integer into an unknown type-level natural.
--
-- /Since: 4.7.0.0/
someNatVal :: Integer -> Maybe SomeNat
someNatVal n
  | n >= 0        = Just (withSNat SomeNat (SNat n) Proxy)
  | otherwise     = Nothing

-- | Convert a string into an unknown type-level symbol.
--
-- /Since: 4.7.0.0/
someSymbolVal :: String -> SomeSymbol
someSymbolVal n   = withSSymbol SomeSymbol (SSymbol n) Proxy



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

type family EqNat (a :: Nat) (b :: Nat) where
  EqNat a a = True
  EqNat a b = False
type instance a == b = EqNat a b

type family EqSymbol (a :: Symbol) (b :: Symbol) where
  EqSymbol a a = True
  EqSymbol a b = False
type instance a == b = EqSymbol a b

--------------------------------------------------------------------------------

infix  4 <=?, <=
infixl 6 +, -
infixl 7 *
infixr 8 ^

-- | Comparison of type-level naturals, as a constraint.
type x <= y = (x <=? y) ~ True

-- | Comparison of type-level symbols, as a function.
--
-- /Since: 4.7.0.0/
type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering

-- | Comparison of type-level naturals, as a function.
--
-- /Since: 4.7.0.0/
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
-- /Since: 4.7.0.0/
type family (m :: Nat) - (n :: Nat) :: Nat


--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level numbers, or 'Nothing'.
--
-- /Since: 4.7.0.0/
sameNat :: (KnownNat a, KnownNat b) =>
           Proxy a -> Proxy b -> Maybe (a :~: b)
sameNat x y
  | natVal x == natVal y = Just (unsafeCoerce Refl)
  | otherwise            = Nothing

-- | We either get evidence that this function was instantiated with the
-- same type-level symbols, or 'Nothing'.
--
-- /Since: 4.7.0.0/
sameSymbol :: (KnownSymbol a, KnownSymbol b) =>
              Proxy a -> Proxy b -> Maybe (a :~: b)
sameSymbol x y
  | symbolVal x == symbolVal y  = Just (unsafeCoerce Refl)
  | otherwise                   = Nothing

--------------------------------------------------------------------------------
-- PRIVATE:

newtype SNat    (n :: Nat)    = SNat    Integer
newtype SSymbol (s :: Symbol) = SSymbol String

data WrapN a b = WrapN (KnownNat    a => Proxy a -> b)
data WrapS a b = WrapS (KnownSymbol a => Proxy a -> b)

-- See Note [magicDictId magic] in "basicType/MkId.hs"
withSNat :: (KnownNat a => Proxy a -> b)
         -> SNat a      -> Proxy a -> b
withSNat f x y = magicDict (WrapN f) x y

-- See Note [magicDictId magic] in "basicType/MkId.hs"
withSSymbol :: (KnownSymbol a => Proxy a -> b)
            -> SSymbol a      -> Proxy a -> b
withSSymbol f x y = magicDict (WrapS f) x y


