{-# LANGUAGE DataKinds #-}              -- to declare the kinds
{-# LANGUAGE KindSignatures #-}         -- (used all over)
{-# LANGUAGE TypeFamilies #-}           -- for declaring operators + sing family
{-# LANGUAGE TypeOperators #-}          -- for declaring operator
{-# LANGUAGE EmptyDataDecls #-}         -- for declaring the kinds
{-# LANGUAGE GADTs #-}                  -- for examining type nats
{-# LANGUAGE PolyKinds #-}              -- for Sing family
{-# LANGUAGE UndecidableInstances #-}   -- for a bunch of the instances
{-# LANGUAGE FlexibleInstances #-}      -- for kind parameters
{-# LANGUAGE FlexibleContexts #-}       -- for kind parameters
{-# LANGUAGE ScopedTypeVariables #-}    -- for kind parameters
{-# LANGUAGE MultiParamTypeClasses #-}  -- for <=, singRep, SingE
{-# LANGUAGE FunctionalDependencies #-} -- for SingRep and SingE
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for workin with type-level naturals should be defined in a separate library. -}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol

    -- * Linking type and value level
  , Sing, SingI, SingE, SingRep, sing, fromSing
  , unsafeSingNat, unsafeSingSymbol
  , Kind

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
data family Sing (n :: k)

newtype instance Sing (n :: Nat)    = SNat Integer

newtype instance Sing (n :: Symbol) = SSym String

unsafeSingNat :: Integer -> Sing (n :: Nat)
unsafeSingNat = SNat

unsafeSingSymbol :: String -> Sing (n :: Symbol)
unsafeSingSymbol = SSym

--------------------------------------------------------------------------------

-- | The class 'SingI' provides a \"smart\" constructor for singleton types.
-- There are built-in instances for the singleton types corresponding
-- to type literals.

class SingI a where

  -- | The only value of type @Sing a@
  sing :: Sing a

--------------------------------------------------------------------------------
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

{- | A class that converts singletons of a given kind into values of some
representation type (i.e., we "forget" the extra information carried
by the singletons, and convert them to ordinary values).

Note that 'fromSing' is overloaded based on the /kind/ of the values
and not their type---all types of a given kind are processed by the
same instances.
-}

class (kparam ~ Kind) => SingE (kparam :: k) rep | kparam -> rep where
  fromSing :: Sing (a :: k) -> rep

instance SingE (Kind :: Nat) Integer where
  fromSing (SNat n) = n

instance SingE (Kind :: Symbol) String where
  fromSing (SSym s) = s


{- | A convenience class, useful when we need to both introduce and eliminate
a given singleton value. Users should never need to define instances of
this classes. -}
class    (SingI a, SingE (Kind :: k) rep) => SingRep (a :: k) rep | a -> rep
instance (SingI a, SingE (Kind :: k) rep) => SingRep (a :: k) rep


{- | A convenience function useful when we need to name a singleton value
multiple times.  Without this function, each use of 'sing' could potentially
refer to a different singleton, and one has to use type signatures to
ensure that they are the same. -}

withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing


{- | A convenience function that names a singleton satisfying a certain
property.  If the singleton does not satisfy the property, then the function
returns 'Nothing'. The property is expressed in terms of the underlying
representation of the singleton. -}

singThat :: (SingRep a rep) => (rep -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing



instance (SingE (Kind :: k) rep, Show rep) => Show (Sing (a :: k)) where
  showsPrec p = showsPrec p . fromSing

instance (SingRep a rep, Read rep, Eq rep) => Read (Sing a) where
  readsPrec p cs = do (x,ys) <- readsPrec p cs
                      case singThat (== x) of
                        Just y  -> [(y,ys)]
                        Nothing -> []



--------------------------------------------------------------------------------
data IsZero :: Nat -> * where
  IsZero :: IsZero 0
  IsSucc :: !(Sing n) -> IsZero (n + 1)

isZero :: Sing n -> IsZero n
isZero (SNat n) | n == 0    = unsafeCoerce IsZero
                | otherwise = unsafeCoerce (IsSucc (SNat (n-1)))

instance Show (IsZero n) where
  show IsZero     = "0"
  show (IsSucc n) = "(" ++ show n ++ " + 1)"

data IsEven :: Nat -> * where
  IsEvenZero :: IsEven 0
  IsEven     :: !(Sing (n+1)) -> IsEven (2 * n + 2)
  IsOdd      :: !(Sing n)     -> IsEven (2 * n + 1)

isEven :: Sing n -> IsEven n
isEven (SNat n) | n == 0      = unsafeCoerce IsEvenZero
                | testBit n 0 = unsafeCoerce (IsOdd  (SNat (n `shiftR` 1)))
                | otherwise   = unsafeCoerce (IsEven (SNat (n `shiftR` 1)))

instance Show (IsEven n) where
  show IsEvenZero = "0"
  show (IsEven x) = "(2 * " ++ show x ++ ")"
  show (IsOdd  x) = "(2 * " ++ show x ++ " + 1)"


