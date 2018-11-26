{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Sum
-- Copyright   :  (c) Ross Paterson 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Sums, lifted to functors.
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

module Data.Functor.Sum (
    Sum(..),
  ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.
data Sum f g a = InL (f a) | InR (g a)
  deriving ( Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where
    liftEq eq (InL x1) (InL x2) = liftEq eq x1 x2
    liftEq _ (InL _) (InR _) = False
    liftEq _ (InR _) (InL _) = False
    liftEq eq (InR y1) (InR y2) = liftEq eq y1 y2

-- | @since 4.9.0.0
instance (Ord1 f, Ord1 g) => Ord1 (Sum f g) where
    liftCompare comp (InL x1) (InL x2) = liftCompare comp x1 x2
    liftCompare _ (InL _) (InR _) = LT
    liftCompare _ (InR _) (InL _) = GT
    liftCompare comp (InR y1) (InR y2) = liftCompare comp y1 y2

-- | @since 4.9.0.0
instance (Read1 f, Read1 g) => Read1 (Sum f g) where
    liftReadPrec rp rl = readData $
        readUnaryWith (liftReadPrec rp rl) "InL" InL <|>
        readUnaryWith (liftReadPrec rp rl) "InR" InR

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance (Show1 f, Show1 g) => Show1 (Sum f g) where
    liftShowsPrec sp sl d (InL x) =
        showsUnaryWith (liftShowsPrec sp sl) "InL" d x
    liftShowsPrec sp sl d (InR y) =
        showsUnaryWith (liftShowsPrec sp sl) "InR" d y

-- | @since 4.9.0.0
instance (Eq1 f, Eq1 g, Eq a) => Eq (Sum f g a) where
    (==) = eq1
-- | @since 4.9.0.0
instance (Ord1 f, Ord1 g, Ord a) => Ord (Sum f g a) where
    compare = compare1
-- | @since 4.9.0.0
instance (Read1 f, Read1 g, Read a) => Read (Sum f g a) where
    readPrec = readPrec1

    readListPrec = readListPrecDefault
    readList     = readListDefault
-- | @since 4.9.0.0
instance (Show1 f, Show1 g, Show a) => Show (Sum f g a) where
    showsPrec = showsPrec1

-- | @since 4.9.0.0
instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap f (InL x) = InL (fmap f x)
    fmap f (InR y) = InR (fmap f y)

-- | @since 4.9.0.0
instance (Foldable f, Foldable g) => Foldable (Sum f g) where
    foldMap f (InL x) = foldMap f x
    foldMap f (InR y) = foldMap f y

-- | @since 4.9.0.0
instance (Traversable f, Traversable g) => Traversable (Sum f g) where
    traverse f (InL x) = InL <$> traverse f x
    traverse f (InR y) = InR <$> traverse f y
