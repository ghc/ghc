{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import Data.Eq
import Data.Ord
import Data.Foldable (Foldable(..), asum)
import Data.Functor ((<$>))
import Data.Functor.Const (Const(..))

import GHC.Base
import GHC.Generics
import GHC.List (repeat, zipWith)
import GHC.Read (Read)
import GHC.Show (Show)

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

-- | Lists, but with an 'Applicative' functor based on zipping.
newtype ZipList a = ZipList { getZipList :: [a] }
                  deriving ( Show     -- ^ @since 4.7.0.0
                           , Eq       -- ^ @since 4.7.0.0
                           , Ord      -- ^ @since 4.7.0.0
                           , Read     -- ^ @since 4.7.0.0
                           , Functor  -- ^ @since 2.01
                           , Foldable -- ^ @since 4.9.0.0
                           , Generic  -- ^ @since 4.7.0.0
                           , Generic1 -- ^ @since 4.7.0.0
                           )
-- See Data.Traversable for Traversable instance due to import loops

-- |
-- > f <$> ZipList xs1 <*> ... <*> ZipList xsN
-- >     = ZipList (zipWithN f xs1 ... xsN)
--
-- where @zipWithN@ refers to the @zipWith@ function of the appropriate arity
-- (@zipWith@, @zipWith3@, @zipWith4@, ...). For example:
--
-- > (\a b c -> stimes c [a, b]) <$> ZipList "abcd" <*> ZipList "567" <*> ZipList [1..]
-- >     = ZipList (zipWith3 (\a b c -> stimes c [a, b]) "abcd" "567" [1..])
-- >     = ZipList {getZipList = ["a5","b6b6","c7c7c7"]}
--
-- @since 2.01
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

-- | @since 4.11.0.0
instance Alternative ZipList where
   empty = ZipList []
   ZipList xs0 <|> ZipList ys0 = ZipList $ go xs0 ys0
     where
       go (x:xs) (_:ys) = x : go xs ys
       go    []     ys  = ys
       go    xs      _  = xs

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
-- >>> runExcept $ optional canFail *> final
-- Right 42

optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
