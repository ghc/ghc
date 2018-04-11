{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Ap
-- Copyright   :  (c) Daniel Cartwright 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrapped applicative type.
--
-- @since 4.12.0.0
-----------------------------------------------------------------------------

module Data.Functor.Ap (
    Ap(..)
  ) where

import Control.Applicative (Alternative, liftA2)
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix  (MonadFix)
import Data.Data (Data)
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Classes
import Data.Traversable (Traversable(traverse))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

newtype Ap f a = Ap { getAp :: f a }
          deriving ( Alternative -- ^ @since 4.12.0.0
                   , Applicative -- ^ @since 4.12.0.0 
                   , Data        -- ^ @since 4.12.0.0 
                   , Generic     -- ^ @since 4.12.0.0
                   , Generic1    -- ^ @since 4.12.0.0 
                   , MonadFail   -- ^ @since 4.12.0.0
                   , MonadFix    -- ^ @since 4.12.0.0
                   , MonadPlus   -- ^ @since 4.12.0.0
                   , Monad       -- ^ @since 4.12.0.0 
                   ) 

-- | @since 4.12.0.0
instance (Applicative f, Enum a) => Enum (Ap f a) where
  toEnum   = toEnum 
  fromEnum = fromEnum 

-- | @since 4.12.0.0
instance (Applicative f, Bounded a) => Bounded (Ap f a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- | @since 4.12.0.0
instance (Applicative f, Num a) => Num (Ap f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = fmap negate
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

-- | @since 4.12.0.0
instance (Applicative f, Fractional a) => Fractional (Ap f a) where
  recip        = fmap recip
  fromRational = pure . fromRational

-- | @since 4.12.0.0
instance (Applicative f, Ord1 f, Real a) => Real (Ap f a) where
  toRational = toRational

-- | @since 4.12.0.0
instance (Applicative f, Ord1 f, Integral a) => Integral (Ap f a) where
  quotRem   = quotRem
  toInteger = toInteger

-- | @since 4.12.0.0
instance (Applicative f, Floating a) => Floating (Ap f a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

-- | @since 4.12.0.0
instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  (<>) = liftA2 (<>)

-- | @since 4.12.0.0
instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = pure mempty

-- | @since 4.12.0.0
instance (Eq1 f) => Eq1 (Ap f) where
    liftEq eq (Ap x) (Ap y) = liftEq eq x y

-- | @since 4.12.0.0
instance (Ord1 f) => Ord1 (Ap f) where
    liftCompare comp (Ap x) (Ap y) = liftCompare comp x y

-- | @since 4.12.0.0
instance (Read1 f) => Read1 (Ap f) where
    liftReadPrec rp rl = readData $
        readUnaryWith (liftReadPrec rp rl) "Ap" Ap

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.12.0.0
instance (Show1 f) => Show1 (Ap f) where
    liftShowsPrec sp sl d (Ap x) =
        showsUnaryWith (liftShowsPrec sp sl) "Ap" d x

-- | @since 4.12.0.0
instance (Eq1 f, Eq a) => Eq (Ap f a) where
    (==) = eq1
-- | @since 4.12.0.0
instance (Ord1 f, Ord a) => Ord (Ap f a) where
    compare = compare1
-- | @since 4.12.0.0
instance (Read1 f, Read a) => Read (Ap f a) where
    readPrec = readPrec1

    readListPrec = readListPrecDefault
    readList     = readListDefault
-- | @since 4.12.0.0
instance (Show1 f, Show a) => Show (Ap f a) where
    showsPrec = showsPrec1

-- | @since 4.12.0.0
instance (Functor f) => Functor (Ap f) where
    fmap f (Ap x) = Ap $ fmap f x 

-- | @since 4.12.0.0
instance (Foldable f) => Foldable (Ap f) where
    foldMap f (Ap x) = foldMap f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Ap f) where
    traverse f (Ap x) = Ap <$> traverse f x 
