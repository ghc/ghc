{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE TypeOperators       #-}

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
-- In mathematics, a semigroup is an algebraic structure consisting of a
-- set together with an associative binary operation. A semigroup
-- generalizes a monoid in that there might not exist an identity
-- element. It also (originally) generalized a group (a monoid with all
-- inverses) to a type where every element did not have to have an inverse,
-- thus the name semigroup.
--
-- The use of @(\<\>)@ in this module conflicts with an operator with the same
-- name that is being exported by Data.Monoid. However, this package
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
  , Monoid(..)
  , Dual(..)
  , Endo(..)
  , All(..)
  , Any(..)
  , Sum(..)
  , Product(..)
  -- * A better monoid for Maybe
  , Option(..)
  , option
  -- * Difference lists of a semigroup
  , diff
  , cycle1
  -- * ArgMin, ArgMax
  , Arg(..)
  , ArgMin
  , ArgMax
  ) where

import           Prelude             hiding (foldr1)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Data.Bifunctor
import           Data.Coerce
import           Data.Data
import           Data.List.NonEmpty
import           Data.Monoid         (All (..), Any (..), Dual (..), Endo (..),
                                      Product (..), Sum (..))
import           Data.Monoid         (Alt (..))
import qualified Data.Monoid         as Monoid
import           Data.Void
import           GHC.Generics

infixr 6 <>

-- | The class of semigroups (types with an associative binary operation).
--
-- @since 4.9.0.0
class Semigroup a where
  -- | An associative operation.
  --
  -- @
  -- (a '<>' b) '<>' c = a '<>' (b '<>' c)
  -- @
  --
  -- If @a@ is also a 'Monoid' we further require
  --
  -- @
  -- ('<>') = 'mappend'
  -- @
  (<>) :: a -> a -> a

  default (<>) :: Monoid a => a -> a -> a
  (<>) = mappend

  -- | Reduce a non-empty list with @\<\>@
  --
  -- The default definition should be sufficient, but this can be
  -- overridden for efficiency.
  --
  sconcat :: NonEmpty a -> a
  sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b

  -- | Repeat a value @n@ times.
  --
  -- Given that this works on a 'Semigroup' it is allowed to fail if
  -- you request 0 or fewer repetitions, and the default definition
  -- will do so.
  --
  -- By making this a member of the class, idempotent semigroups and monoids can
  -- upgrade this to execute in /O(1)/ by picking
  -- @stimes = stimesIdempotent@ or @stimes = stimesIdempotentMonoid@
  -- respectively.
  stimes :: Integral b => b -> a -> a
  stimes y0 x0
    | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (pred y  `quot` 2) x
      g x y z
        | even y = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (pred y `quot` 2) (x <> z)

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle1 :: Semigroup m => m -> m
cycle1 xs = xs' where xs' = xs <> xs'

instance Semigroup () where
  _ <> _ = ()
  sconcat _ = ()
  stimes _ _ = ()

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a
  stimes n f e = stimes n (f e)

instance Semigroup [a] where
  (<>) = (++)
  stimes n x
    | n < 0 = errorWithoutStackTrace "stimes: [], negative multiplier"
    | otherwise = rep n
    where
      rep 0 = []
      rep i = x ++ rep (i - 1)

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)
  stimes _ Nothing  = Nothing
  stimes n (Just a) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Maybe, negative multiplier"
    EQ -> Nothing
    GT -> Just (stimes n a)

instance Semigroup (Either a b) where
  Left _ <> b = b
  a      <> _ = a
  stimes = stimesIdempotent

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a,b) <> (a',b') = (a<>a',b<>b')
  stimes n (a,b) = (stimes n a, stimes n b)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
  stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
  (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
  stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
  (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
  stimes n (a,b,c,d,e) =
      (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> y = y
  GT <> _ = GT
  stimes = stimesIdempotentMonoid

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)
  stimes n (Dual a) = Dual (stimes n a)

instance Semigroup (Endo a) where
  (<>) = coerce ((.) :: (a -> a) -> (a -> a) -> (a -> a))
  stimes = stimesMonoid

instance Semigroup All where
  (<>) = coerce (&&)
  stimes = stimesIdempotentMonoid

instance Semigroup Any where
  (<>) = coerce (||)
  stimes = stimesIdempotentMonoid


instance Num a => Semigroup (Sum a) where
  (<>) = coerce ((+) :: a -> a -> a)
  stimes n (Sum a) = Sum (fromIntegral n * a)

instance Num a => Semigroup (Product a) where
  (<>) = coerce ((*) :: a -> a -> a)
  stimes n (Product a) = Product (a ^ n)

-- | This is a valid definition of 'stimes' for a 'Monoid'.
--
-- Unlike the default definition of 'stimes', it is defined for 0
-- and so it should be preferred where possible.
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid n x0 = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesMonoid: negative multiplier"
  EQ -> mempty
  GT -> f x0 n
    where
      f x y
        | even y = f (x `mappend` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `mappend` x) (pred y  `quot` 2) x
      g x y z
        | even y = g (x `mappend` x) (y `quot` 2) z
        | y == 1 = x `mappend` z
        | otherwise = g (x `mappend` x) (pred y `quot` 2) (x `mappend` z)

-- | This is a valid definition of 'stimes' for an idempotent 'Monoid'.
--
-- When @mappend x x = x@, this definition should be preferred, because it
-- works in /O(1)/ rather than /O(log n)/
stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid n x = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesIdempotentMonoid: negative multiplier"
  EQ -> mempty
  GT -> x

-- | This is a valid definition of 'stimes' for an idempotent 'Semigroup'.
--
-- When @x <> x = x@, this definition should be preferred, because it
-- works in /O(1)/ rather than /O(log n)/.
stimesIdempotent :: Integral b => b -> a -> a
stimesIdempotent n x
  | n <= 0 = errorWithoutStackTrace "stimesIdempotent: positive multiplier expected"
  | otherwise = x

instance Semigroup a => Semigroup (Const a b) where
  (<>) = coerce ((<>) :: a -> a -> a)
  stimes n (Const a) = Const (stimes n a)

instance Semigroup (Monoid.First a) where
  Monoid.First Nothing <> b = b
  a                    <> _ = a
  stimes = stimesIdempotentMonoid

instance Semigroup (Monoid.Last a) where
  a <> Monoid.Last Nothing = a
  _ <> b                   = b
  stimes = stimesIdempotentMonoid

instance Alternative f => Semigroup (Alt f a) where
  (<>) = coerce ((<|>) :: f a -> f a -> f a)
  stimes = stimesMonoid

instance Semigroup Void where
  a <> _ = a
  stimes = stimesIdempotent

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)


newtype Min a = Min { getMin :: a }
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Bounded a => Bounded (Min a) where
  minBound = Min minBound
  maxBound = Min maxBound

instance Enum a => Enum (Min a) where
  succ (Min a) = Min (succ a)
  pred (Min a) = Min (pred a)
  toEnum = Min . toEnum
  fromEnum = fromEnum . getMin
  enumFrom (Min a) = Min <$> enumFrom a
  enumFromThen (Min a) (Min b) = Min <$> enumFromThen a b
  enumFromTo (Min a) (Min b) = Min <$> enumFromTo a b
  enumFromThenTo (Min a) (Min b) (Min c) = Min <$> enumFromThenTo a b c


instance Ord a => Semigroup (Min a) where
  (<>) = coerce (min :: a -> a -> a)
  stimes = stimesIdempotent

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = maxBound
  mappend = (<>)

instance Functor Min where
  fmap f (Min x) = Min (f x)

instance Foldable Min where
  foldMap f (Min a) = f a

instance Traversable Min where
  traverse f (Min a) = Min <$> f a

instance Applicative Min where
  pure = Min
  a <* _ = a
  _ *> a = a
  Min f <*> Min x = Min (f x)

instance Monad Min where
  (>>) = (*>)
  Min a >>= f = f a

instance MonadFix Min where
  mfix f = fix (f . getMin)

instance Num a => Num (Min a) where
  (Min a) + (Min b) = Min (a + b)
  (Min a) * (Min b) = Min (a * b)
  (Min a) - (Min b) = Min (a - b)
  negate (Min a) = Min (negate a)
  abs    (Min a) = Min (abs a)
  signum (Min a) = Min (signum a)
  fromInteger    = Min . fromInteger

newtype Max a = Max { getMax :: a }
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Bounded a => Bounded (Max a) where
  minBound = Max minBound
  maxBound = Max maxBound

instance Enum a => Enum (Max a) where
  succ (Max a) = Max (succ a)
  pred (Max a) = Max (pred a)
  toEnum = Max . toEnum
  fromEnum = fromEnum . getMax
  enumFrom (Max a) = Max <$> enumFrom a
  enumFromThen (Max a) (Max b) = Max <$> enumFromThen a b
  enumFromTo (Max a) (Max b) = Max <$> enumFromTo a b
  enumFromThenTo (Max a) (Max b) (Max c) = Max <$> enumFromThenTo a b c

instance Ord a => Semigroup (Max a) where
  (<>) = coerce (max :: a -> a -> a)
  stimes = stimesIdempotent

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = minBound
  mappend = (<>)

instance Functor Max where
  fmap f (Max x) = Max (f x)

instance Foldable Max where
  foldMap f (Max a) = f a

instance Traversable Max where
  traverse f (Max a) = Max <$> f a

instance Applicative Max where
  pure = Max
  a <* _ = a
  _ *> a = a
  Max f <*> Max x = Max (f x)

instance Monad Max where
  (>>) = (*>)
  Max a >>= f = f a

instance MonadFix Max where
  mfix f = fix (f . getMax)

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
data Arg a b = Arg a b deriving
  (Show, Read, Data, Typeable, Generic, Generic1)

type ArgMin a b = Min (Arg a b)
type ArgMax a b = Max (Arg a b)

instance Functor (Arg a) where
  fmap f (Arg x a) = Arg x (f a)

instance Foldable (Arg a) where
  foldMap f (Arg _ a) = f a

instance Traversable (Arg a) where
  traverse f (Arg x a) = Arg x <$> f a

instance Eq a => Eq (Arg a b) where
  Arg a _ == Arg b _ = a == b

instance Ord a => Ord (Arg a b) where
  Arg a _ `compare` Arg b _ = compare a b
  min x@(Arg a _) y@(Arg b _)
    | a <= b    = x
    | otherwise = y
  max x@(Arg a _) y@(Arg b _)
    | a >= b    = x
    | otherwise = y

instance Bifunctor Arg where
  bimap f g (Arg a b) = Arg (f a) (g b)

-- | Use @'Option' ('First' a)@ to get the behavior of
-- 'Data.Monoid.First' from "Data.Monoid".
newtype First a = First { getFirst :: a } deriving
  (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Bounded a => Bounded (First a) where
  minBound = First minBound
  maxBound = First maxBound

instance Enum a => Enum (First a) where
  succ (First a) = First (succ a)
  pred (First a) = First (pred a)
  toEnum = First . toEnum
  fromEnum = fromEnum . getFirst
  enumFrom (First a) = First <$> enumFrom a
  enumFromThen (First a) (First b) = First <$> enumFromThen a b
  enumFromTo (First a) (First b) = First <$> enumFromTo a b
  enumFromThenTo (First a) (First b) (First c) = First <$> enumFromThenTo a b c

instance Semigroup (First a) where
  a <> _ = a
  stimes = stimesIdempotent

instance Functor First where
  fmap f (First x) = First (f x)

instance Foldable First where
  foldMap f (First a) = f a

instance Traversable First where
  traverse f (First a) = First <$> f a

instance Applicative First where
  pure x = First x
  a <* _ = a
  _ *> a = a
  First f <*> First x = First (f x)

instance Monad First where
  (>>) = (*>)
  First a >>= f = f a

instance MonadFix First where
  mfix f = fix (f . getFirst)

-- | Use @'Option' ('Last' a)@ to get the behavior of
-- 'Data.Monoid.Last' from "Data.Monoid"
newtype Last a = Last { getLast :: a } deriving
  (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Bounded a => Bounded (Last a) where
  minBound = Last minBound
  maxBound = Last maxBound

instance Enum a => Enum (Last a) where
  succ (Last a) = Last (succ a)
  pred (Last a) = Last (pred a)
  toEnum = Last . toEnum
  fromEnum = fromEnum . getLast
  enumFrom (Last a) = Last <$> enumFrom a
  enumFromThen (Last a) (Last b) = Last <$> enumFromThen a b
  enumFromTo (Last a) (Last b) = Last <$> enumFromTo a b
  enumFromThenTo (Last a) (Last b) (Last c) = Last <$> enumFromThenTo a b c

instance Semigroup (Last a) where
  _ <> b = b
  stimes = stimesIdempotent

instance Functor Last where
  fmap f (Last x) = Last (f x)
  a <$ _ = Last a

instance Foldable Last where
  foldMap f (Last a) = f a

instance Traversable Last where
  traverse f (Last a) = Last <$> f a

instance Applicative Last where
  pure = Last
  a <* _ = a
  _ *> a = a
  Last f <*> Last x = Last (f x)

instance Monad Last where
  (>>) = (*>)
  Last a >>= f = f a

instance MonadFix Last where
  mfix f = fix (f . getLast)

-- | Provide a Semigroup for an arbitrary Monoid.
newtype WrappedMonoid m = WrapMonoid { unwrapMonoid :: m }
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Monoid m => Semigroup (WrappedMonoid m) where
  (<>) = coerce (mappend :: m -> m -> m)

instance Monoid m => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty
  mappend = (<>)

instance Bounded a => Bounded (WrappedMonoid a) where
  minBound = WrapMonoid minBound
  maxBound = WrapMonoid maxBound

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
-- Implemented using 'stimes' and 'mempty'.
--
-- This is a suitable definition for an 'mtimes' member of 'Monoid'.
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault n x
  | n == 0    = mempty
  | otherwise = unwrapMonoid (stimes n (WrapMonoid x))

-- | 'Option' is effectively 'Maybe' with a better instance of
-- 'Monoid', built off of an underlying 'Semigroup' instead of an
-- underlying 'Monoid'.
--
-- Ideally, this type would not exist at all and we would just fix the
-- 'Monoid' instance of 'Maybe'
newtype Option a = Option { getOption :: Maybe a }
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Functor Option where
  fmap f (Option a) = Option (fmap f a)

instance Applicative Option where
  pure a = Option (Just a)
  Option a <*> Option b = Option (a <*> b)

  Option Nothing  *>  _ = Option Nothing
  _               *>  b = b

instance Monad Option where
  Option (Just a) >>= k = k a
  _               >>= _ = Option Nothing
  (>>) = (*>)

instance Alternative Option where
  empty = Option Nothing
  Option Nothing <|> b = b
  a <|> _ = a

instance MonadPlus Option where
  mzero = Option Nothing
  mplus = (<|>)

instance MonadFix Option where
  mfix f = Option (mfix (getOption . f))

instance Foldable Option where
  foldMap f (Option (Just m)) = f m
  foldMap _ (Option Nothing)  = mempty

instance Traversable Option where
  traverse f (Option (Just a)) = Option . Just <$> f a
  traverse _ (Option Nothing)  = pure (Option Nothing)

-- | Fold an 'Option' case-wise, just like 'maybe'.
option :: b -> (a -> b) -> Option a -> b
option n j (Option m) = maybe n j m

instance Semigroup a => Semigroup (Option a) where
  (<>) = coerce ((<>) :: Maybe a -> Maybe a -> Maybe a)

  stimes _ (Option Nothing) = Option Nothing
  stimes n (Option (Just a)) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Option, negative multiplier"
    EQ -> Option Nothing
    GT -> Option (Just (stimes n a))

instance Semigroup a => Monoid (Option a) where
  mempty = Option Nothing
  mappend = (<>)

-- | This lets you use a difference list of a 'Semigroup' as a 'Monoid'.
diff :: Semigroup m => m -> Endo m
diff = Endo . (<>)

instance Semigroup (Proxy s) where
  _ <> _ = Proxy
  sconcat _ = Proxy
  stimes _ _ = Proxy
