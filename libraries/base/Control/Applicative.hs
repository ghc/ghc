{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module describes a structure intermediate between a functor and
-- a monad (technically, a strong lax monoidal functor).  Compared with
-- monads, this interface lacks the full power of the binding operation
-- '>>=', but
--
-- * it has more instances.
--
-- * it is sufficient for many uses, e.g. context-free parsing, or the
--   'Data.Traversable.Traversable' class.
--
-- * instances can perform analysis of computations before they are
--   executed, and thus produce shared optimizations.
--
-- This interface was introduced for parsers by Niklas R&#xF6;jemo, because
-- it admits more sharing than the monadic interface.  The names here are
-- mostly based on parsing work by Doaitse Swierstra.
--
-- For more details, see
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects>,
-- by Conor McBride and Ross Paterson.

module Control.Applicative (
    -- * Applicative functors
    Applicative(..),
    -- * Alternatives
    Alternative(..),
    -- * Instances
    Const(..), WrappedMonad(..), WrappedArrow(..), ZipList(..),
    -- * Utility functions
    (<$>), (<$), (<**>),
    liftA, liftA2, liftA3,
    optional,
    ) where

import Control.Category hiding ((.), id)
import Control.Arrow
import Data.Maybe
import Data.Tuple
import Data.Eq
import Data.Ord
import Data.Foldable (Foldable(..))
import Data.Functor ((<$>))

import GHC.Base
import GHC.Generics
import GHC.List (repeat, zipWith)
import GHC.Read (Read)
import GHC.Show (Show)

newtype Const a b = Const { getConst :: a }
                  deriving (Generic, Generic1, Monoid)

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Functor (Const m) where
    fmap _ (Const v) = Const v

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    (<*>) = coerce (mappend :: m -> m -> m)
-- This is pretty much the same as
-- Const f <*> Const v = Const (f `mappend` v)
-- but guarantees that mappend for Const a b will have the same arity
-- as the one for a; it won't create a closure to raise the arity
-- to 2.

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }
                         deriving (Generic, Generic1, Monad)

instance Monad m => Functor (WrappedMonad m) where
    fmap f (WrapMonad v) = WrapMonad (liftM f v)

instance Monad m => Applicative (WrappedMonad m) where
    pure = WrapMonad . return
    WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)

instance MonadPlus m => Alternative (WrappedMonad m) where
    empty = WrapMonad mzero
    WrapMonad u <|> WrapMonad v = WrapMonad (u `mplus` v)

newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }
                           deriving (Generic, Generic1)

instance Arrow a => Functor (WrappedArrow a b) where
    fmap f (WrapArrow a) = WrapArrow (a >>> arr f)

instance Arrow a => Applicative (WrappedArrow a b) where
    pure x = WrapArrow (arr (const x))
    WrapArrow f <*> WrapArrow v = WrapArrow (f &&& v >>> arr (uncurry id))

instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) where
    empty = WrapArrow zeroArrow
    WrapArrow u <|> WrapArrow v = WrapArrow (u <+> v)

-- | Lists, but with an 'Applicative' functor based on zipping, so that
--
-- @f '<$>' 'ZipList' xs1 '<*>' ... '<*>' 'ZipList' xsn = 'ZipList' (zipWithn f xs1 ... xsn)@
--
newtype ZipList a = ZipList { getZipList :: [a] }
                  deriving (Show, Eq, Ord, Read, Functor, Generic, Generic1)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)

-- extra functions

-- | One or none.
optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
