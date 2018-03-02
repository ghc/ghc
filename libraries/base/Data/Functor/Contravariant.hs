{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Contravariant
-- Copyright   :  (C) 2007-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Contravariant' functors, sometimes referred to colloquially as @Cofunctor@,
-- even though the dual of a 'Functor' is just a 'Functor'. As with 'Functor'
-- the definition of 'Contravariant' for a given ADT is unambiguous.
--
-- @since 4.12.0.0
----------------------------------------------------------------------------

module Data.Functor.Contravariant (
  -- * Contravariant Functors
    Contravariant(..)
  , phantom

  -- * Operators
  , (>$<), (>$$<), ($<)

  -- * Predicates
  , Predicate(..)

  -- * Comparisons
  , Comparison(..)
  , defaultComparison

  -- * Equivalence Relations
  , Equivalence(..)
  , defaultEquivalence
  , comparisonEquivalence

  -- * Dual arrows
  , Op(..)
  ) where

import Control.Applicative
import Control.Category
import Data.Function (on)

import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

import Data.Monoid (Alt(..))
import Data.Semigroup (Semigroup(..))
import Data.Proxy
import GHC.Generics

import Prelude hiding ((.),id)

-- | The class of contravariant functors.
--
-- Whereas in Haskell, one can think of a 'Functor' as containing or producing
-- values, a contravariant functor is a functor that can be thought of as
-- /consuming/ values.
--
-- As an example, consider the type of predicate functions  @a -> Bool@. One
-- such predicate might be @negative x = x < 0@, which
-- classifies integers as to whether they are negative. However, given this
-- predicate, we can re-use it in other situations, providing we have a way to
-- map values /to/ integers. For instance, we can use the @negative@ predicate
-- on a person's bank balance to work out if they are currently overdrawn:
--
-- @
-- newtype Predicate a = Predicate { getPredicate :: a -> Bool }
--
-- instance Contravariant Predicate where
--   contramap f (Predicate p) = Predicate (p . f)
--                                          |   `- First, map the input...
--                                          `----- then apply the predicate.
--
-- overdrawn :: Predicate Person
-- overdrawn = contramap personBankBalance negative
-- @
--
-- Any instance should be subject to the following laws:
--
-- > contramap id = id
-- > contramap f . contramap g = contramap (g . f)
--
-- Note, that the second law follows from the free theorem of the type of
-- 'contramap' and the first law, so you need only check that the former
-- condition holds.

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

  -- | Replace all locations in the output with the same value.
  -- The default definition is @'contramap' . 'const'@, but this may be
  -- overridden with a more efficient version.
  (>$) :: b -> f b -> f a
  (>$) = contramap . const

-- | If 'f' is both 'Functor' and 'Contravariant' then by the time you factor
-- in the laws of each of those classes, it can't actually use its argument in
-- any meaningful capacity.
--
-- This method is surprisingly useful. Where both instances exist and are
-- lawful we have the following laws:
--
-- @
-- 'fmap' f ≡ 'phantom'
-- 'contramap' f ≡ 'phantom'
-- @
phantom :: (Functor f, Contravariant f) => f a -> f b
phantom x = () <$ x $< ()

infixl 4 >$, $<, >$<, >$$<

-- | This is '>$' with its arguments flipped.
($<) :: Contravariant f => f b -> b -> f a
($<) = flip (>$)

-- | This is an infix alias for 'contramap'.
(>$<) :: Contravariant f => (a -> b) -> f b -> f a
(>$<) = contramap

-- | This is an infix version of 'contramap' with the arguments flipped.
(>$$<) :: Contravariant f => f b -> (a -> b) -> f a
(>$$<) = flip contramap

deriving instance Contravariant f => Contravariant (Alt f)
deriving instance Contravariant f => Contravariant (Rec1 f)
deriving instance Contravariant f => Contravariant (M1 i c f)

instance Contravariant V1 where
  contramap _ x = case x of

instance Contravariant U1 where
  contramap _ _ = U1

instance Contravariant (K1 i c) where
  contramap _ (K1 c) = K1 c

instance (Contravariant f, Contravariant g) => Contravariant (f :*: g) where
  contramap f (xs :*: ys) = contramap f xs :*: contramap f ys

instance (Functor f, Contravariant g) => Contravariant (f :.: g) where
  contramap f (Comp1 fg) = Comp1 (fmap (contramap f) fg)

instance (Contravariant f, Contravariant g) => Contravariant (f :+: g) where
  contramap f (L1 xs) = L1 (contramap f xs)
  contramap f (R1 ys) = R1 (contramap f ys)

instance (Contravariant f, Contravariant g) => Contravariant (Sum f g) where
  contramap f (InL xs) = InL (contramap f xs)
  contramap f (InR ys) = InR (contramap f ys)

instance (Contravariant f, Contravariant g)
  => Contravariant (Product f g) where
    contramap f (Pair a b) = Pair (contramap f a) (contramap f b)

instance Contravariant (Const a) where
  contramap _ (Const a) = Const a

instance (Functor f, Contravariant g) => Contravariant (Compose f g) where
  contramap f (Compose fga) = Compose (fmap (contramap f) fga)

instance Contravariant Proxy where
  contramap _ _ = Proxy

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

-- | A 'Predicate' is a 'Contravariant' 'Functor', because 'contramap' can
-- apply its function argument to the input of the predicate.
instance Contravariant Predicate where
  contramap f g = Predicate $ getPredicate g . f

instance Semigroup (Predicate a) where
  Predicate p <> Predicate q = Predicate $ \a -> p a && q a

instance Monoid (Predicate a) where
  mempty = Predicate $ const True

-- | Defines a total ordering on a type as per 'compare'.
--
-- This condition is not checked by the types. You must ensure that the
-- supplied values are valid total orderings yourself.
newtype Comparison a = Comparison { getComparison :: a -> a -> Ordering }

deriving instance Semigroup (Comparison a)
deriving instance Monoid (Comparison a)

-- | A 'Comparison' is a 'Contravariant' 'Functor', because 'contramap' can
-- apply its function argument to each input of the comparison function.
instance Contravariant Comparison where
  contramap f g = Comparison $ on (getComparison g) f

-- | Compare using 'compare'.
defaultComparison :: Ord a => Comparison a
defaultComparison = Comparison compare

-- | This data type represents an equivalence relation.
--
-- Equivalence relations are expected to satisfy three laws:
--
-- __Reflexivity__:
--
-- @
-- 'getEquivalence' f a a = True
-- @
--
-- __Symmetry__:
--
-- @
-- 'getEquivalence' f a b = 'getEquivalence' f b a
-- @
--
-- __Transitivity__:
--
-- If @'getEquivalence' f a b@ and @'getEquivalence' f b c@ are both 'True'
-- then so is @'getEquivalence' f a c@.
--
-- The types alone do not enforce these laws, so you'll have to check them
-- yourself.
newtype Equivalence a = Equivalence { getEquivalence :: a -> a -> Bool }

-- | Equivalence relations are 'Contravariant', because you can
-- apply the contramapped function to each input to the equivalence
-- relation.
instance Contravariant Equivalence where
  contramap f g = Equivalence $ on (getEquivalence g) f

instance Semigroup (Equivalence a) where
  Equivalence p <> Equivalence q = Equivalence $ \a b -> p a b && q a b

instance Monoid (Equivalence a) where
  mempty = Equivalence (\_ _ -> True)

-- | Check for equivalence with '=='.
--
-- Note: The instances for 'Double' and 'Float' violate reflexivity for @NaN@.
defaultEquivalence :: Eq a => Equivalence a
defaultEquivalence = Equivalence (==)

comparisonEquivalence :: Comparison a -> Equivalence a
comparisonEquivalence (Comparison p) = Equivalence $ \a b -> p a b == EQ

-- | Dual function arrows.
newtype Op a b = Op { getOp :: b -> a }

deriving instance Semigroup a => Semigroup (Op a b)
deriving instance Monoid a => Monoid (Op a b)

instance Category Op where
  id = Op id
  Op f . Op g = Op (g . f)

instance Contravariant (Op a) where
  contramap f g = Op (getOp g . f)

instance Num a => Num (Op a b) where
  Op f + Op g = Op $ \a -> f a + g a
  Op f * Op g = Op $ \a -> f a * g a
  Op f - Op g = Op $ \a -> f a - g a
  abs (Op f) = Op $ abs . f
  signum (Op f) = Op $ signum . f
  fromInteger = Op . const . fromInteger

instance Fractional a => Fractional (Op a b) where
  Op f / Op g = Op $ \a -> f a / g a
  recip (Op f) = Op $ recip . f
  fromRational = Op . const . fromRational

instance Floating a => Floating (Op a b) where
  pi = Op $ const pi
  exp (Op f) = Op $ exp . f
  sqrt (Op f) = Op $ sqrt . f
  log (Op f) = Op $ log . f
  sin (Op f) = Op $ sin . f
  tan (Op f) = Op $ tan . f
  cos (Op f) = Op $ cos . f
  asin (Op f) = Op $ asin . f
  atan (Op f) = Op $ atan . f
  acos (Op f) = Op $ acos . f
  sinh (Op f) = Op $ sinh . f
  tanh (Op f) = Op $ tanh . f
  cosh (Op f) = Op $ cosh . f
  asinh (Op f) = Op $ asinh . f
  atanh (Op f) = Op $ atanh . f
  acosh (Op f) = Op $ acosh . f
  Op f ** Op g = Op $ \a -> f a ** g a
  logBase (Op f) (Op g) = Op $ \a -> logBase (f a) (g a)
