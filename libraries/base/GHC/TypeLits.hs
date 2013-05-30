{-# LANGUAGE DataKinds #-}              -- to declare the kinds
{-# LANGUAGE KindSignatures #-}         -- (used all over)
{-# LANGUAGE TypeFamilies #-}           -- for declaring operators + sing family
{-# LANGUAGE TypeOperators #-}          -- for declaring operator
{-# LANGUAGE EmptyDataDecls #-}         -- for declaring the kinds
{-# LANGUAGE GADTs #-}                  -- for examining type nats
{-# LANGUAGE PolyKinds #-}              -- for Sing family
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}  -- for <=
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for working with type-level naturals should be defined in a separate library. -}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol

    -- * Linking type and value level
  , Sing, SingI, SingE, SingRep, sing, fromSing
  , unsafeSingNat, unsafeSingSymbol

    -- * Working with singletons
  , withSing, singThat

    -- * Functions on type nats
  , type (<=), type (<=?), type (+), type (*), type (^)
  , type (-)

    -- * Comparing for equality
  , type (:~:) (..), eqSingNat, eqSingSym

    -- * Destructing type-nat singletons.
  , isZero, IsZero(..)

-- Commented out; see definition below; SLPJ Jan 13
--  , isEven, IsEven(..)


    -- * Matching on type-nats
  , Nat1(..), FromNat1

    -- * Kind parameters
  , OfKind(..), Demote, DemoteRep
  , KindOf
  ) where

import GHC.Base(Eq((==)), Bool(..), ($), otherwise, (.))
import GHC.Num(Integer, (-))
import GHC.Base(String)
import GHC.Read(Read(..))
import GHC.Show(Show(..))
import Unsafe.Coerce(unsafeCoerce)
-- import Data.Bits(testBit,shiftR)
import Data.Maybe(Maybe(..))
import Data.List((++))

-- | (Kind) A kind useful for passing kinds as parameters.
data OfKind (a :: *) = KindParam

{- | A shortcut for naming the kind parameter corresponding to the
kind of a some type.  For example, @KindOf Int ~ (KindParam :: OfKind *)@,
but @KindOf 2 ~ (KindParam :: OfKind Nat)@. -}
type KindOf (a :: k) = (KindParam :: OfKind k)


-- | (Kind) This is the kind of type-level natural numbers.
data Nat

-- | (Kind) This is the kind of type-level symbols.
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
-- | Comparison of type-level naturals.
class (m <=? n) ~ True => (m :: Nat) <= (n :: Nat)
instance ((m <=? n) ~ True) => m <= n

type family (m :: Nat) <=? (n :: Nat) :: Bool

-- | Addition of type-level naturals.
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat

-- | Subtraction of type-level naturals.
-- Note that this operation is unspecified for some inputs.
type family (m :: Nat) - (n :: Nat) :: Nat


--------------------------------------------------------------------------------

{- | A class that converts singletons of a given kind into values of some
representation type (i.e., we "forget" the extra information carried
by the singletons, and convert them to ordinary values).

Note that 'fromSing' is overloaded based on the /kind/ of the values
and not their type---all types of a given kind are processed by the
same instances.
-}

class (kparam ~ KindParam) => SingE (kparam :: OfKind k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam

instance SingE (KindParam :: OfKind Nat) where
  type DemoteRep (KindParam :: OfKind Nat) = Integer
  fromSing (SNat n) = n

instance SingE (KindParam :: OfKind Symbol) where
  type DemoteRep (KindParam :: OfKind Symbol) = String
  fromSing (SSym s) = s

{- | A convenient name for the type used to representing the values
for a particular singleton family.  For example, @Demote 2 ~ Integer@,
and also @Demote 3 ~ Integer@, but @Demote "Hello" ~ String@. -}
type Demote a = DemoteRep (KindOf a)

{- | A convenience class, useful when we need to both introduce and eliminate
a given singleton value. Users should never need to define instances of
this classes. -}
class    (SingI a, SingE (KindOf a)) => SingRep (a :: k)
instance (SingI a, SingE (KindOf a)) => SingRep (a :: k)


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

singThat :: SingRep a => (Demote a -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing


instance (SingE (KindOf a), Show (Demote a)) => Show (Sing a) where
  showsPrec p = showsPrec p . fromSing

instance (SingRep a, Read (Demote a), Eq (Demote a)) => Read (Sing a) where
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

{- ----------------------------------------------------------------------------

This IsEven code is commented out for now.  The trouble is that the 
IsEven constructor has an ambiguous type, at least until (+) becomes
suitably injective. 

data IsEven :: Nat -> * where
  IsEvenZero :: IsEven 0
  IsEven     :: !(Sing (n+1)) -> IsEven (2 * n + 2)
  IsEven     :: !(Sing (n)) -> IsEven (2 * n + 1)
  IsOdd      :: !(Sing n)     -> IsEven (2 * n + 1)

isEven :: Sing n -> IsEven n
isEven (SNat n) | n == 0      = unsafeCoerce IsEvenZero
                | testBit n 0 = unsafeCoerce (IsOdd  (SNat (n `shiftR` 1)))
                | otherwise   = unsafeCoerce (IsEven (SNat (n `shiftR` 1)))

instance Show (IsEven n) where
  show IsEvenZero = "0"
  show (IsEven x) = "(2 * " ++ show x ++ ")"
  show (IsOdd  x) = "(2 * " ++ show x ++ " + 1)"

------------------------------------------------------------------------------ -}

-- | Unary implementation of natural numbers.
-- Used both at the type and at the value level.
data Nat1 = Zero | Succ Nat1

type family FromNat1 (n :: Nat1) :: Nat
type instance FromNat1 Zero     = 0
type instance FromNat1 (Succ n) = 1 + FromNat1 n

--------------------------------------------------------------------------------

-- | A type that provides evidence for equality between two types.
data (:~:) :: k -> k -> * where
  Refl :: a :~: a

instance Show (a :~: b) where
  show Refl = "Refl"

{- | Check if two type-natural singletons of potentially different types
are indeed the same, by comparing their runtime representations.

WARNING: in combination with `unsafeSingNat` this may lead to unsoundness:

> eqSingNat (sing :: Sing 1) (unsafeSingNat 1 :: Sing 2)
> == Just (Refl :: 1 :~: 2)
-}

eqSingNat :: Sing (m :: Nat) -> Sing (n :: Nat) -> Maybe (m :~: n)
eqSingNat x y
  | fromSing x == fromSing y  = Just (unsafeCoerce Refl)
  | otherwise                 = Nothing


{- | Check if two symbol singletons of potentially different types
are indeed the same, by comparing their runtime representations.
WARNING: in combination with `unsafeSingSymbol` this may lead to unsoundness
(see `eqSingNat` for an example).
-}

eqSingSym:: Sing (m :: Symbol) -> Sing (n :: Symbol) -> Maybe (m :~: n)
eqSingSym x y
  | fromSing x == fromSing y  = Just (unsafeCoerce Refl)
  | otherwise                 = Nothing



