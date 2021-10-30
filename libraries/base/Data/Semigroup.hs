{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type @a@ is a 'Semigroup' if it provides an associative function ('<>')
-- that lets you combine any two values of type @a@ into one. Where being
-- associative means that the following must always hold:
--
-- prop> (a <> b) <> c == a <> (b <> c)
--
-- ==== __Examples__
--
-- The 'Min' 'Semigroup' instance for 'Int' is defined to always pick the smaller
-- number:
-- >>> Min 1 <> Min 2 <> Min 3 <> Min 4 :: Min Int
-- Min {getMin = 1}
--
-- If we need to combine multiple values we can use the 'sconcat' function
-- to do so. We need to ensure however that we have at least one value to
-- operate on, since otherwise our result would be undefined. It is for this
-- reason that 'sconcat' uses "Data.List.NonEmpty.NonEmpty" - a list that
-- can never be empty:
--
-- >>> (1 :| [])
-- 1 :| []               -- equivalent to [1] but guaranteed to be non-empty.
--
-- >>> (1 :| [2, 3, 4])
-- 1 :| [2,3,4]          -- equivalent to [1,2,3,4] but guaranteed to be non-empty.
--
-- Equipped with this guaranteed to be non-empty data structure, we can combine
-- values using 'sconcat' and a 'Semigroup' of our choosing. We can try the 'Min'
-- and 'Max' instances of 'Int' which pick the smallest, or largest number
-- respectively:
--
-- >>> sconcat (1 :| [2, 3, 4]) :: Min Int
-- Min {getMin = 1}
-- >>> sconcat (1 :| [2, 3, 4]) :: Max Int
-- Max {getMax = 4}
--
-- String concatenation is another example of a 'Semigroup' instance:
--
-- >>> "foo" <> "bar"
-- "foobar"
--
-- A 'Semigroup' is a generalization of a 'Monoid'. Yet unlike the 'Semigroup', the 'Monoid'
-- requires the presence of a neutral element ('mempty') in addition to the associative
-- operator. The requirement for a neutral element prevents many types from being a full Monoid,
-- like "Data.List.NonEmpty.NonEmpty".
--
-- Note that the use of @(\<\>)@ in this module conflicts with an operator with the same
-- name that is being exported by "Data.Monoid". However, this package
-- re-exports (most of) the contents of Data.Monoid, so to use semigroups
-- and monoids in the same package just
--
-- > import Data.Semigroup
--
-- @since 4.9.0.0
----------------------------------------------------------------------------
module Data.Semigroup (
    Semigroup(..)
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  -- * Semigroups
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , WrappedMonoid(..)
  -- * Re-exported monoids from Data.Monoid
  , Dual(..)
  , Endo(..)
  , All(..)
  , Any(..)
  , Sum(..)
  , Product(..)
  -- * Difference lists of a semigroup
  , diff
  , cycle1
  -- * ArgMin, ArgMax
  , Arg(..)
  , ArgMin
  , ArgMax
  ) where

import           Prelude             hiding (foldr1)

import GHC.Base (Semigroup(..))

import           Data.Semigroup.Internal

import           Control.Applicative
import           Control.Monad.Fix
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import           Data.Data
import           GHC.Generics
import qualified GHC.List as List

-- $setup
-- >>> import Prelude
-- >>> import Data.List.NonEmpty (NonEmpty (..))

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle1 :: Semigroup m => m -> m
cycle1 xs = xs' where xs' = xs <> xs'

-- | This lets you use a difference list of a 'Semigroup' as a 'Monoid'.
--
-- === __Example:__
-- >>> let hello = diff "Hello, "
-- >>> appEndo hello "World!"
-- "Hello, World!"
-- >>> appEndo (hello <> mempty) "World!"
-- "Hello, World!"
-- >>> appEndo (mempty <> hello) "World!"
-- "Hello, World!"
-- >>> let world = diff "World"
-- >>> let excl = diff "!"
-- >>> appEndo (hello <> (world <> excl)) mempty
-- "Hello, World!"
-- >>> appEndo ((hello <> world) <> excl) mempty
-- "Hello, World!"
diff :: Semigroup m => m -> Endo m
diff = Endo . (<>)

newtype Min a = Min { getMin :: a }
  deriving ( Bounded  -- ^ @since 4.9.0.0
           , Eq       -- ^ @since 4.9.0.0
           , Ord      -- ^ @since 4.9.0.0
           , Show     -- ^ @since 4.9.0.0
           , Read     -- ^ @since 4.9.0.0
           , Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance Enum a => Enum (Min a) where
  succ (Min a) = Min (succ a)
  pred (Min a) = Min (pred a)
  toEnum = Min . toEnum
  fromEnum = fromEnum . getMin
  enumFrom (Min a) = Min <$> enumFrom a
  enumFromThen (Min a) (Min b) = Min <$> enumFromThen a b
  enumFromTo (Min a) (Min b) = Min <$> enumFromTo a b
  enumFromThenTo (Min a) (Min b) (Min c) = Min <$> enumFromThenTo a b c


-- | @since 4.9.0.0
instance Ord a => Semigroup (Min a) where
  (<>) = coerce (min :: a -> a -> a)
  stimes = stimesIdempotent

-- | @since 4.9.0.0
instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = maxBound
  -- By default, we would get a lazy right fold. This forces the use of a strict
  -- left fold instead.
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- | @since 4.9.0.0
instance Functor Min where
  fmap f (Min x) = Min (f x)

-- | @since 4.9.0.0
instance Foldable Min where
  foldMap f (Min a) = f a

-- | @since 4.9.0.0
instance Traversable Min where
  traverse f (Min a) = Min <$> f a

-- | @since 4.9.0.0
instance Applicative Min where
  pure = Min
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce

-- | @since 4.9.0.0
instance Monad Min where
  (>>) = (*>)
  Min a >>= f = f a

-- | @since 4.9.0.0
instance MonadFix Min where
  mfix f = fix (f . getMin)

-- | @since 4.9.0.0
instance Num a => Num (Min a) where
  (Min a) + (Min b) = Min (a + b)
  (Min a) * (Min b) = Min (a * b)
  (Min a) - (Min b) = Min (a - b)
  negate (Min a) = Min (negate a)
  abs    (Min a) = Min (abs a)
  signum (Min a) = Min (signum a)
  fromInteger    = Min . fromInteger

newtype Max a = Max { getMax :: a }
  deriving ( Bounded  -- ^ @since 4.9.0.0
           , Eq       -- ^ @since 4.9.0.0
           , Ord      -- ^ @since 4.9.0.0
           , Show     -- ^ @since 4.9.0.0
           , Read     -- ^ @since 4.9.0.0
           , Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance Enum a => Enum (Max a) where
  succ (Max a) = Max (succ a)
  pred (Max a) = Max (pred a)
  toEnum = Max . toEnum
  fromEnum = fromEnum . getMax
  enumFrom (Max a) = Max <$> enumFrom a
  enumFromThen (Max a) (Max b) = Max <$> enumFromThen a b
  enumFromTo (Max a) (Max b) = Max <$> enumFromTo a b
  enumFromThenTo (Max a) (Max b) (Max c) = Max <$> enumFromThenTo a b c

-- | @since 4.9.0.0
instance Ord a => Semigroup (Max a) where
  (<>) = coerce (max :: a -> a -> a)
  stimes = stimesIdempotent

-- | @since 4.9.0.0
instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = minBound
  -- By default, we would get a lazy right fold. This forces the use of a strict
  -- left fold instead.
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- | @since 4.9.0.0
instance Functor Max where
  fmap f (Max x) = Max (f x)

-- | @since 4.9.0.0
instance Foldable Max where
  foldMap f (Max a) = f a

-- | @since 4.9.0.0
instance Traversable Max where
  traverse f (Max a) = Max <$> f a

-- | @since 4.9.0.0
instance Applicative Max where
  pure = Max
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce

-- | @since 4.9.0.0
instance Monad Max where
  (>>) = (*>)
  Max a >>= f = f a

-- | @since 4.9.0.0
instance MonadFix Max where
  mfix f = fix (f . getMax)

-- | @since 4.9.0.0
instance Num a => Num (Max a) where
  (Max a) + (Max b) = Max (a + b)
  (Max a) * (Max b) = Max (a * b)
  (Max a) - (Max b) = Max (a - b)
  negate (Max a) = Max (negate a)
  abs    (Max a) = Max (abs a)
  signum (Max a) = Max (signum a)
  fromInteger    = Max . fromInteger

-- | 'Arg' isn't itself a 'Semigroup' in its own right, but it can be
-- placed inside 'Min' and 'Max' to compute an arg min or arg max.
--
-- >>> minimum [ Arg (x * x) x | x <- [-10 .. 10] ]
-- Arg 0 0
data Arg a b = Arg
  a
  -- ^ The argument used for comparisons in 'Eq' and 'Ord'.
  b
  -- ^ The "value" exposed via the 'Functor', 'Foldable' etc. instances.
  deriving
  ( Show     -- ^ @since 4.9.0.0
  , Read     -- ^ @since 4.9.0.0
  , Data     -- ^ @since 4.9.0.0
  , Generic  -- ^ @since 4.9.0.0
  , Generic1 -- ^ @since 4.9.0.0
  )

-- |
-- >>> Min (Arg 0 ()) <> Min (Arg 1 ())
-- Min {getMin = Arg 0 ()}
type ArgMin a b = Min (Arg a b)

-- |
-- >>> Max (Arg 0 ()) <> Max (Arg 1 ())
-- Max {getMax = Arg 1 ()}
type ArgMax a b = Max (Arg a b)

-- | @since 4.9.0.0
instance Functor (Arg a) where
  fmap f (Arg x a) = Arg x (f a)

-- | @since 4.9.0.0
instance Foldable (Arg a) where
  foldMap f (Arg _ a) = f a

-- | @since 4.9.0.0
instance Traversable (Arg a) where
  traverse f (Arg x a) = Arg x <$> f a

-- | @since 4.9.0.0
instance Eq a => Eq (Arg a b) where
  Arg a _ == Arg b _ = a == b

-- | @since 4.9.0.0
instance Ord a => Ord (Arg a b) where
  Arg a _ `compare` Arg b _ = compare a b
  min x@(Arg a _) y@(Arg b _)
    | a <= b    = x
    | otherwise = y
  max x@(Arg a _) y@(Arg b _)
    | a >= b    = x
    | otherwise = y

-- | @since 4.9.0.0
instance Bifunctor Arg where
  bimap f g (Arg a b) = Arg (f a) (g b)

-- | @since 4.10.0.0
instance Bifoldable Arg where
  bifoldMap f g (Arg a b) = f a <> g b

-- | @since 4.10.0.0
instance Bitraversable Arg where
  bitraverse f g (Arg a b) = Arg <$> f a <*> g b

newtype First a = First { getFirst :: a }
  deriving ( Bounded  -- ^ @since 4.9.0.0
           , Eq       -- ^ @since 4.9.0.0
           , Ord      -- ^ @since 4.9.0.0
           , Show     -- ^ @since 4.9.0.0
           , Read     -- ^ @since 4.9.0.0
           , Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance Enum a => Enum (First a) where
  succ (First a) = First (succ a)
  pred (First a) = First (pred a)
  toEnum = First . toEnum
  fromEnum = fromEnum . getFirst
  enumFrom (First a) = First <$> enumFrom a
  enumFromThen (First a) (First b) = First <$> enumFromThen a b
  enumFromTo (First a) (First b) = First <$> enumFromTo a b
  enumFromThenTo (First a) (First b) (First c) = First <$> enumFromThenTo a b c

-- | @since 4.9.0.0
instance Semigroup (First a) where
  a <> _ = a
  stimes = stimesIdempotent

-- | @since 4.9.0.0
instance Functor First where
  fmap f (First x) = First (f x)

-- | @since 4.9.0.0
instance Foldable First where
  foldMap f (First a) = f a

-- | @since 4.9.0.0
instance Traversable First where
  traverse f (First a) = First <$> f a

-- | @since 4.9.0.0
instance Applicative First where
  pure x = First x
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce

-- | @since 4.9.0.0
instance Monad First where
  (>>) = (*>)
  First a >>= f = f a

-- | @since 4.9.0.0
instance MonadFix First where
  mfix f = fix (f . getFirst)

newtype Last a = Last { getLast :: a }
  deriving ( Bounded  -- ^ @since 4.9.0.0
           , Eq       -- ^ @since 4.9.0.0
           , Ord      -- ^ @since 4.9.0.0
           , Show     -- ^ @since 4.9.0.0
           , Read     -- ^ @since 4.9.0.0
           , Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance Enum a => Enum (Last a) where
  succ (Last a) = Last (succ a)
  pred (Last a) = Last (pred a)
  toEnum = Last . toEnum
  fromEnum = fromEnum . getLast
  enumFrom (Last a) = Last <$> enumFrom a
  enumFromThen (Last a) (Last b) = Last <$> enumFromThen a b
  enumFromTo (Last a) (Last b) = Last <$> enumFromTo a b
  enumFromThenTo (Last a) (Last b) (Last c) = Last <$> enumFromThenTo a b c

-- | @since 4.9.0.0
instance Semigroup (Last a) where
  _ <> b = b
  stimes = stimesIdempotent

-- | @since 4.9.0.0
instance Functor Last where
  fmap f (Last x) = Last (f x)
  a <$ _ = Last a

-- | @since 4.9.0.0
instance Foldable Last where
  foldMap f (Last a) = f a

-- | @since 4.9.0.0
instance Traversable Last where
  traverse f (Last a) = Last <$> f a

-- | @since 4.9.0.0
instance Applicative Last where
  pure = Last
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce

-- | @since 4.9.0.0
instance Monad Last where
  (>>) = (*>)
  Last a >>= f = f a

-- | @since 4.9.0.0
instance MonadFix Last where
  mfix f = fix (f . getLast)

-- | Provide a Semigroup for an arbitrary Monoid.
--
-- __NOTE__: This is not needed anymore since 'Semigroup' became a superclass of
-- 'Monoid' in /base-4.11/ and this newtype be deprecated at some point in the future.
newtype WrappedMonoid m = WrapMonoid { unwrapMonoid :: m }
  deriving ( Bounded  -- ^ @since 4.9.0.0
           , Eq       -- ^ @since 4.9.0.0
           , Ord      -- ^ @since 4.9.0.0
           , Show     -- ^ @since 4.9.0.0
           , Read     -- ^ @since 4.9.0.0
           , Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance Monoid m => Semigroup (WrappedMonoid m) where
  (<>) = coerce (mappend :: m -> m -> m)

-- | @since 4.9.0.0
instance Monoid m => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty
  -- This ensures that we use whatever mconcat is defined for the wrapped
  -- Monoid.
  mconcat = coerce (mconcat :: [m] -> m)

-- | @since 4.9.0.0
instance Enum a => Enum (WrappedMonoid a) where
  succ (WrapMonoid a) = WrapMonoid (succ a)
  pred (WrapMonoid a) = WrapMonoid (pred a)
  toEnum = WrapMonoid . toEnum
  fromEnum = fromEnum . unwrapMonoid
  enumFrom (WrapMonoid a) = WrapMonoid <$> enumFrom a
  enumFromThen (WrapMonoid a) (WrapMonoid b) = WrapMonoid <$> enumFromThen a b
  enumFromTo (WrapMonoid a) (WrapMonoid b) = WrapMonoid <$> enumFromTo a b
  enumFromThenTo (WrapMonoid a) (WrapMonoid b) (WrapMonoid c) =
      WrapMonoid <$> enumFromThenTo a b c

-- | Repeat a value @n@ times.
--
-- > mtimesDefault n a = a <> a <> ... <> a  -- using <> (n-1) times
--
-- In many cases, `stimes 0 a` for a `Monoid` will produce `mempty`.
-- However, there are situations when it cannot do so. In particular,
-- the following situation is fairly common:
--
-- @
-- data T a = ...
--
-- class Constraint1 a
-- class Constraint1 a => Constraint2 a
--
-- @
-- instance Constraint1 a => 'Semigroup' (T a)
-- instance Constraint2 a => 'Monoid' (T a)
-- @
--
-- Since @Constraint1@ is insufficient to implement 'mempty',
-- 'stimes' for @T a@ cannot do so.
--
-- When working with such a type, or when working polymorphically with
-- 'Semigroup' instances, @mtimesDefault@ should be used when the
-- multiplier might be zero. It is implemented using 'stimes' when
-- the multiplier is nonzero and 'mempty' when it is zero.
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault n x
  | n == 0    = mempty
  | otherwise = stimes n x
