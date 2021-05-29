-- Strict counterparts to common data structures,
-- e.g. tuples, lists, maybes, etc.
--
-- Import this module qualified as Strict.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Data.Strict (
    Maybe(Nothing, Just),
    fromMaybe,
    Pair(And),

    -- Not used at the moment:
    --
    -- Either(Left, Right),
    -- List(Nil, Cons),
  ) where

import GHC.Prelude hiding (Maybe(..), Either(..))
import Control.Applicative
import Data.Semigroup
import Data.Data

data Maybe a = Nothing | Just !a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Data)

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe (Just f) (Just x) = Just (f x)
apMaybe _ _ = Nothing

altMaybe :: Maybe a -> Maybe a -> Maybe a
altMaybe Nothing r = r
altMaybe l _ = l

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance Applicative Maybe where
  pure = Just
  (<*>) = apMaybe

instance Alternative Maybe where
  empty = Nothing
  (<|>) = altMaybe

data Pair a b = !a `And` !b
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Data)

-- The definitions below are commented out because they are
-- not used anywhere in the compiler, but are useful to showcase
-- the intent behind this module (i.e. how it may evolve).
--
-- data Either a b = Left !a | Right !b
--   deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Data)
--
-- data List a = Nil | !a `Cons` !(List a)
--   deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Data)
