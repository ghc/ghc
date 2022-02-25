{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Product
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Products, lifted to functors.
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

module Data.Functor.Product (
    Product(..),
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(mzipWith))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted product of functors.
data Product f g a = Pair (f a) (g a)
  deriving ( Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance (Eq1 f, Eq1 g) => Eq1 (Product f g) where
    liftEq eq (Pair x1 y1) (Pair x2 y2) = liftEq eq x1 x2 && liftEq eq y1 y2

-- | @since 4.9.0.0
instance (Ord1 f, Ord1 g) => Ord1 (Product f g) where
    liftCompare comp (Pair x1 y1) (Pair x2 y2) =
        liftCompare comp x1 x2 `mappend` liftCompare comp y1 y2

-- | @since 4.9.0.0
instance (Read1 f, Read1 g) => Read1 (Product f g) where
    liftReadPrec rp rl = readData $
        readBinaryWith (liftReadPrec rp rl) (liftReadPrec rp rl) "Pair" Pair

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance (Show1 f, Show1 g) => Show1 (Product f g) where
    liftShowsPrec sp sl d (Pair x y) =
        showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Pair" d x y

-- | @since 4.9.0.0
instance (Eq1 f, Eq1 g, Eq a) => Eq (Product f g a)
    where (==) = eq1

-- | @since 4.9.0.0
instance (Ord1 f, Ord1 g, Ord a) => Ord (Product f g a) where
    compare = compare1

-- | @since 4.9.0.0
instance (Read1 f, Read1 g, Read a) => Read (Product f g a) where
    readPrec = readPrec1

    readListPrec = readListPrecDefault
    readList     = readListDefault

-- | @since 4.9.0.0
instance (Show1 f, Show1 g, Show a) => Show (Product f g a) where
    showsPrec = showsPrec1

-- | @since 4.9.0.0
instance (Functor f, Functor g) => Functor (Product f g) where
    fmap f (Pair x y) = Pair (fmap f x) (fmap f y)
    a <$ (Pair x y) = Pair (a <$ x) (a <$ y)

-- | @since 4.9.0.0
instance (Foldable f, Foldable g) => Foldable (Product f g) where
    foldMap f (Pair x y) = foldMap f x `mappend` foldMap f y

-- | @since 4.9.0.0
instance (Traversable f, Traversable g) => Traversable (Product f g) where
    traverse f (Pair x y) = liftA2 Pair (traverse f x) (traverse f y)

-- | @since 4.9.0.0
instance (Applicative f, Applicative g) => Applicative (Product f g) where
    pure x = Pair (pure x) (pure x)
    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)
    liftA2 f (Pair a b) (Pair x y) = Pair (liftA2 f a x) (liftA2 f b y)

-- | @since 4.9.0.0
instance (Alternative f, Alternative g) => Alternative (Product f g) where
    empty = Pair empty empty
    Pair x1 y1 <|> Pair x2 y2 = Pair (x1 <|> x2) (y1 <|> y2)

-- | @since 4.9.0.0
instance (Monad f, Monad g) => Monad (Product f g) where
    Pair m n >>= f = Pair (m >>= fstP . f) (n >>= sndP . f)
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

-- | @since 4.9.0.0
instance (MonadPlus f, MonadPlus g) => MonadPlus (Product f g) where
    mzero = Pair mzero mzero
    Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `mplus` x2) (y1 `mplus` y2)

-- | @since 4.9.0.0
instance (MonadFix f, MonadFix g) => MonadFix (Product f g) where
    mfix f = Pair (mfix (fstP . f)) (mfix (sndP . f))
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

-- | @since 4.9.0.0
instance (MonadZip f, MonadZip g) => MonadZip (Product f g) where
    mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (mzipWith f x1 x2) (mzipWith f y1 y2)

-- | @since 4.16.0.0
instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Product f g a) where
    Pair x1 y1 <> Pair x2 y2 = Pair (x1 <> x2) (y1 <> y2)

-- | @since 4.16.0.0
instance (Monoid (f a), Monoid (g a)) => Monoid (Product f g a) where
    mempty = Pair mempty mempty
