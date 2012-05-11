{-# LANGUAGE DataKinds #-}              -- to declare the kinds
{-# LANGUAGE KindSignatures #-}         -- (used all over)
{-# LANGUAGE MultiParamTypeClasses #-}  -- for <=
{-# LANGUAGE TypeFamilies #-}           -- for declaring operators + sing family
{-# LANGUAGE TypeOperators #-}          -- for declaring operator
{-# LANGUAGE EmptyDataDecls #-}         -- for declaring the kinds
{-# LANGUAGE GADTs #-}                  -- for examining type nats
{-# LANGUAGE PolyKinds #-}              -- for Sing family
{-# LANGUAGE UndecidableInstances #-}   -- for Read and Show instances
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for workin with type-level naturals should be defined in a separate library. -}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol

    -- * Linking type and value level
  , Sing, SingI, Kind, sing, SingRep, fromSing
  , unsafeMakeSing

    -- * Working with singletons
  , withSing, singThat

    -- * Functions on type nats
  , type (<=), type (<=?), type (+), type (*), type (^)

    -- * Destructing type-nats.
  , isZero, IsZero(..)
  , isEven, IsEven(..)
  ) where

import GHC.Base(Eq((==)), Bool(..), ($), otherwise, (.))
import GHC.Num(Integer, (-))
import GHC.Base(String)
import GHC.Read(Read(..))
import GHC.Show(Show(..))
import GHC.Prim(Any)
import Unsafe.Coerce(unsafeCoerce)
import Data.Bits(testBit,shiftR)
import Data.Maybe(Maybe(..))
import Data.List((++))

-- | A type synonym useful for passing kinds as parameters.
type Kind = Any


-- | This is the *kind* of type-level natural numbers.
data Nat

-- | This is the *kind* of type-level symbols.
data Symbol


--------------------------------------------------------------------------------

-- | A family of singleton types, used to link the type-level literals
-- to run-time values.
type family SingRep a

-- | Type-level natural numbers are linked to (positive) integers.
type instance SingRep (Kind :: Nat)    = Integer

-- | Type-level symbols are linked to strings.
type instance SingRep (Kind :: Symbol) = String

newtype Sing (n :: k) = Sing (SingRep (Kind :: k))

--------------------------------------------------------------------------------

-- | The class 'SingI' provides a \"smart\" constructor for singleton types.
-- There are built-in instances for the singleton types corresponding
-- to type literals.

class SingI a where

  -- | The only value of type @Sing a@
  sing :: Sing a


-- | Comparsion of type-level naturals.
class (m :: Nat) <= (n :: Nat)

type family (m :: Nat) <=? (n :: Nat) :: Bool

-- | Addition of type-level naturals.
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat


--------------------------------------------------------------------------------

{- | Turn a value into it's corresponding singleton.
WARNING: There is no checking that the value matches the singleton!
Use this only when you are sugre that the representation matches the
singleton, or there will be lots of confusion!
In general, the safe way to create singleton values is by using
the 'SingI' class, so 'unsafeMakeSing' should be used only to define
these instances. -}
unsafeMakeSing :: SingRep (Kind :: k) -> Sing (a :: k)
unsafeMakeSing = Sing

fromSing :: Sing (a :: k) -> SingRep (Kind :: k)
fromSing (Sing n) = n

withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

singThat :: SingI (a :: k) => (SingRep (Kind :: k) -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

instance Show (SingRep (Kind :: k)) => Show (Sing (a :: k)) where
  showsPrec p = showsPrec p . fromSing

instance (SingI a, Read (SingRep (Kind :: k))
                 , Eq   (SingRep (Kind :: k))) => Read (Sing (a :: k)) where
  readsPrec p cs = do (x,ys) <- readsPrec p cs
                      case singThat (== x) of
                        Just y  -> [(y,ys)]
                        Nothing -> []





--------------------------------------------------------------------------------

data IsZero :: Nat -> * where
  IsZero :: IsZero 0
  IsSucc :: !(Sing n) -> IsZero (n + 1)

isZero :: Sing n -> IsZero n
isZero (Sing n) | n == 0    = unsafeCoerce IsZero
                | otherwise = unsafeCoerce (IsSucc (Sing (n-1)))

instance Show (IsZero n) where
  show IsZero     = "0"
  show (IsSucc n) = "(" ++ show n ++ " + 1)"

data IsEven :: Nat -> * where
  IsEvenZero :: IsEven 0
  IsEven     :: !(Sing (n+1)) -> IsEven (2 * n + 2)
  IsOdd      :: !(Sing n)     -> IsEven (2 * n + 1)

isEven :: Sing n -> IsEven n
isEven (Sing n) | n == 0      = unsafeCoerce IsEvenZero
                | testBit n 0 = unsafeCoerce (IsOdd  (Sing (n `shiftR` 1)))
                | otherwise   = unsafeCoerce (IsEven (Sing (n `shiftR` 1)))

instance Show (IsEven n) where
  show IsEvenZero = "0"
  show (IsEven x) = "(2 * " ++ show x ++ ")"
  show (IsOdd  x) = "(2 * " ++ show x ++ " + 1)"



