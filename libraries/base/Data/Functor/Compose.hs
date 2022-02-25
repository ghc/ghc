{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Compose
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Composition of functors.
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

module Data.Functor.Compose (
    Compose(..),
  ) where

import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

infixr 9 `Compose`

-- | Right-to-left composition of functors.
-- The composition of applicative functors is always applicative,
-- but the composition of monads is not always a monad.
newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving ( Data     -- ^ @since 4.9.0.0
           , Generic  -- ^ @since 4.9.0.0
           , Generic1 -- ^ @since 4.9.0.0
           , Semigroup -- ^ @since 4.16.0.0
           , Monoid    -- ^ @since 4.16.0.0
           )

-- Instances of lifted Prelude classes

-- | @since 4.9.0.0
instance (Eq1 f, Eq1 g) => Eq1 (Compose f g) where
    liftEq eq (Compose x) (Compose y) = liftEq (liftEq eq) x y

-- | @since 4.9.0.0
instance (Ord1 f, Ord1 g) => Ord1 (Compose f g) where
    liftCompare comp (Compose x) (Compose y) =
        liftCompare (liftCompare comp) x y

-- | @since 4.9.0.0
instance (Read1 f, Read1 g) => Read1 (Compose f g) where
    liftReadPrec rp rl = readData $
        readUnaryWith (liftReadPrec rp' rl') "Compose" Compose
      where
        rp' = liftReadPrec     rp rl
        rl' = liftReadListPrec rp rl

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance (Show1 f, Show1 g) => Show1 (Compose f g) where
    liftShowsPrec sp sl d (Compose x) =
        showsUnaryWith (liftShowsPrec sp' sl') "Compose" d x
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

-- Instances of Prelude classes

-- | @since 4.9.0.0
instance (Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
    (==) = eq1

-- | @since 4.9.0.0
instance (Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
    compare = compare1

-- | @since 4.9.0.0
instance (Read1 f, Read1 g, Read a) => Read (Compose f g a) where
    readPrec = readPrec1

    readListPrec = readListPrecDefault
    readList     = readListDefault

-- | @since 4.9.0.0
instance (Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showsPrec = showsPrec1

-- Functor instances

-- | @since 4.9.0.0
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)
    a <$ (Compose x) = Compose (fmap (a <$) x)

-- | @since 4.9.0.0
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

-- | @since 4.9.0.0
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

-- | @since 4.9.0.0
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
    liftA2 f (Compose x) (Compose y) =
      Compose (liftA2 (liftA2 f) x y)

-- | @since 4.9.0.0
instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    (<|>) = coerce ((<|>) :: f (g a) -> f (g a) -> f (g a))
      :: forall a . Compose f g a -> Compose f g a -> Compose f g a

-- | The deduction (via generativity) that if @g x :~: g y@ then @x :~: y@.
--
-- @since 4.14.0.0
instance (TestEquality f) => TestEquality (Compose f g) where
  testEquality (Compose x) (Compose y) =
    case testEquality x y of -- :: Maybe (g x :~: g y)
      Just Refl -> Just Refl -- :: Maybe (x :~: y)
      Nothing   -> Nothing
