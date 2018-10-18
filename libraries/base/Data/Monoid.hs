{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A class for monoids (types with an associative binary operation that
-- has an identity) with various general-purpose instances.
--
-----------------------------------------------------------------------------

module Data.Monoid (
        -- * 'Monoid' typeclass
        Monoid(..),
        (<>),
        Dual(..),
        Endo(..),
        -- * 'Bool' wrappers
        All(..),
        Any(..),
        -- * 'Num' wrappers
        Sum(..),
        Product(..),
        -- * 'Maybe' wrappers
        -- $MaybeExamples
        First(..),
        Last(..),
        -- * 'Alternative' wrapper
        Alt(..),
        -- * 'Applicative' wrapper
        Ap(..)
  ) where

-- Push down the module in the dependency hierarchy.
import GHC.Base hiding (Any)
import GHC.Enum
import GHC.Generics
import GHC.Num
import GHC.Read
import GHC.Show

import Control.Monad.Fail (MonadFail)

import Data.Semigroup.Internal

-- $MaybeExamples
-- To implement @find@ or @findLast@ on any 'Foldable':
--
-- @
-- findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findLast pred = getLast . foldMap (\x -> if pred x
--                                            then Last (Just x)
--                                            else Last Nothing)
-- @
--
-- Much of Data.Map's interface can be implemented with
-- Data.Map.alter. Some of the rest can be implemented with a new
-- @alterA@ function and either 'First' or 'Last':
--
-- > alterA :: (Applicative f, Ord k) =>
-- >           (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
-- >
-- > instance Monoid a => Applicative ((,) a)  -- from Control.Applicative
--
-- @
-- insertLookupWithKey :: Ord k => (k -> v -> v -> v) -> k -> v
--                     -> Map k v -> (Maybe v, Map k v)
-- insertLookupWithKey combine key value =
--   Arrow.first getFirst . alterA doChange key
--   where
--   doChange Nothing = (First Nothing, Just value)
--   doChange (Just oldValue) =
--     (First (Just oldValue),
--      Just (combine key value oldValue))
-- @


-- | Maybe monoid returning the leftmost non-Nothing value.
--
-- @'First' a@ is isomorphic to @'Alt' 'Maybe' a@, but precedes it
-- historically.
--
-- >>> getFirst (First (Just "hello") <> First Nothing <> First (Just "world"))
-- Just "hello"
--
-- Use of this type is discouraged. Note the following equivalence:
--
-- > Data.Monoid.First x === Maybe (Data.Semigroup.First x)
--
-- In addition to being equivalent in the structural sense, the two
-- also have 'Monoid' instances that behave the same. This type will
-- be marked deprecated in GHC 8.8, and removed in GHC 8.10.
-- Users are advised to use the variant from "Data.Semigroup" and wrap
-- it in 'Maybe'.
newtype First a = First { getFirst :: Maybe a }
        deriving ( Eq          -- ^ @since 2.01
                 , Ord         -- ^ @since 2.01
                 , Read        -- ^ @since 2.01
                 , Show        -- ^ @since 2.01
                 , Generic     -- ^ @since 4.7.0.0
                 , Generic1    -- ^ @since 4.7.0.0
                 , Functor     -- ^ @since 4.8.0.0
                 , Applicative -- ^ @since 4.8.0.0
                 , Monad       -- ^ @since 4.8.0.0
                 )

-- | @since 4.9.0.0
instance Semigroup (First a) where
        First Nothing <> b = b
        a             <> _ = a
        stimes = stimesIdempotentMonoid

-- | @since 2.01
instance Monoid (First a) where
        mempty = First Nothing

-- | Maybe monoid returning the rightmost non-Nothing value.
--
-- @'Last' a@ is isomorphic to @'Dual' ('First' a)@, and thus to
-- @'Dual' ('Alt' 'Maybe' a)@
--
-- >>> getLast (Last (Just "hello") <> Last Nothing <> Last (Just "world"))
-- Just "world"
--
-- Use of this type is discouraged. Note the following equivalence:
--
-- > Data.Monoid.Last x === Maybe (Data.Semigroup.Last x)
--
-- In addition to being equivalent in the structural sense, the two
-- also have 'Monoid' instances that behave the same. This type will
-- be marked deprecated in GHC 8.8, and removed in GHC 8.10.
-- Users are advised to use the variant from "Data.Semigroup" and wrap
-- it in 'Maybe'.
newtype Last a = Last { getLast :: Maybe a }
        deriving ( Eq          -- ^ @since 2.01
                 , Ord         -- ^ @since 2.01
                 , Read        -- ^ @since 2.01
                 , Show        -- ^ @since 2.01
                 , Generic     -- ^ @since 4.7.0.0
                 , Generic1    -- ^ @since 4.7.0.0
                 , Functor     -- ^ @since 4.8.0.0
                 , Applicative -- ^ @since 4.8.0.0
                 , Monad       -- ^ @since 4.8.0.0
                 )

-- | @since 4.9.0.0
instance Semigroup (Last a) where
        a <> Last Nothing = a
        _ <> b                   = b
        stimes = stimesIdempotentMonoid

-- | @since 2.01
instance Monoid (Last a) where
        mempty = Last Nothing

-- | This data type witnesses the lifting of a 'Monoid' into an
-- 'Applicative' pointwise.
--
-- @since 4.12.0.0
newtype Ap f a = Ap { getAp :: f a }
        deriving ( Alternative -- ^ @since 4.12.0.0
                 , Applicative -- ^ @since 4.12.0.0
                 , Enum        -- ^ @since 4.12.0.0
                 , Eq          -- ^ @since 4.12.0.0
                 , Functor     -- ^ @since 4.12.0.0
                 , Generic     -- ^ @since 4.12.0.0
                 , Generic1    -- ^ @since 4.12.0.0
                 , Monad       -- ^ @since 4.12.0.0
                 , MonadFail   -- ^ @since 4.12.0.0
                 , MonadPlus   -- ^ @since 4.12.0.0
                 , Ord         -- ^ @since 4.12.0.0
                 , Read        -- ^ @since 4.12.0.0
                 , Show        -- ^ @since 4.12.0.0
                 )

-- | @since 4.12.0.0
instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
        (Ap x) <> (Ap y) = Ap $ liftA2 (<>) x y

-- | @since 4.12.0.0
instance (Applicative f, Monoid a) => Monoid (Ap f a) where
        mempty = Ap $ pure mempty

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

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = oneof [return Nothing, Just `fmap` arbitrary]

prop_mconcatMaybe :: [Maybe [Int]] -> Bool
prop_mconcatMaybe x =
  fromMaybe [] (mconcat x) == mconcat (catMaybes x)

prop_mconcatFirst :: [Maybe Int] -> Bool
prop_mconcatFirst x =
  getFirst (mconcat (map First x)) == listToMaybe (catMaybes x)
prop_mconcatLast :: [Maybe Int] -> Bool
prop_mconcatLast x =
  getLast (mconcat (map Last x)) == listLastToMaybe (catMaybes x)
        where listLastToMaybe [] = Nothing
              listLastToMaybe lst = Just (last lst)
-- -}

-- $setup
-- >>> import Prelude
