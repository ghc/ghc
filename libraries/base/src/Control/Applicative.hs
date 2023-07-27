{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
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
    liftA, liftA3,
    optional,
    asum,
    ) where

import Control.Category hiding ((.), id)
import Control.Arrow
import Data.Maybe
import Data.Tuple
import Data.Foldable (asum)
import Data.Functor ((<$>))
import Data.Functor.Const (Const(..))
import Data.Typeable (Typeable)
import Data.Data (Data)

import GHC.Base
import GHC.Functor.ZipList (ZipList(..))
import GHC.Generics

-- $setup
-- >>> import Prelude

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }
                         deriving ( Generic  -- ^ @since 4.7.0.0
                                  , Generic1 -- ^ @since 4.7.0.0
                                  , Monad    -- ^ @since 4.7.0.0
                                  )

-- | @since 2.01
instance Monad m => Functor (WrappedMonad m) where
    fmap f (WrapMonad v) = WrapMonad (liftM f v)

-- | @since 2.01
instance Monad m => Applicative (WrappedMonad m) where
    pure = WrapMonad . pure
    WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
    liftA2 f (WrapMonad x) (WrapMonad y) = WrapMonad (liftM2 f x y)

-- | @since 2.01
instance MonadPlus m => Alternative (WrappedMonad m) where
    empty = WrapMonad mzero
    WrapMonad u <|> WrapMonad v = WrapMonad (u `mplus` v)

-- | @since 4.14.0.0
deriving instance (Typeable (m :: Type -> Type), Typeable a, Data (m a))
         => Data (WrappedMonad m a)

newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }
                           deriving ( Generic  -- ^ @since 4.7.0.0
                                    , Generic1 -- ^ @since 4.7.0.0
                                    )

-- | @since 2.01
instance Arrow a => Functor (WrappedArrow a b) where
    fmap f (WrapArrow a) = WrapArrow (a >>> arr f)

-- | @since 2.01
instance Arrow a => Applicative (WrappedArrow a b) where
    pure x = WrapArrow (arr (const x))
    liftA2 f (WrapArrow u) (WrapArrow v) =
      WrapArrow (u &&& v >>> arr (uncurry f))

-- | @since 2.01
instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) where
    empty = WrapArrow zeroArrow
    WrapArrow u <|> WrapArrow v = WrapArrow (u <+> v)

-- | @since 4.14.0.0
deriving instance (Typeable (a :: Type -> Type -> Type), Typeable b, Typeable c,
                   Data (a b c))
         => Data (WrappedArrow a b c)

-- extra functions

-- | One or none.
--
-- It is useful for modelling any computation that is allowed to fail.
--
-- ==== __Examples__
--
-- Using the 'Alternative' instance of "Control.Monad.Except", the following functions:
--
-- >>> import Control.Monad.Except
--
-- >>> canFail = throwError "it failed" :: Except String Int
-- >>> final = return 42                :: Except String Int
--
-- Can be combined by allowing the first function to fail:
--
-- >>> runExcept $ canFail *> final
-- Left "it failed"
--
-- >>> runExcept $ optional canFail *> final
-- Right 42

optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
