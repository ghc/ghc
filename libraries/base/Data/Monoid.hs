{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
        Alt (..)
  ) where

-- Push down the module in the dependency hierarchy.
import GHC.Base hiding (Any)
import GHC.Enum
import GHC.Num
import GHC.Read
import GHC.Show
import GHC.Generics

{-
-- just for testing
import Data.Maybe
import Test.QuickCheck
-- -}

infixr 6 <>

-- | An infix synonym for 'mappend'.
--
-- @since 4.5.0.0
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

-- Monoid instances.

-- | The dual of a 'Monoid', obtained by swapping the arguments of 'mappend'.
--
-- >>> getDual (mappend (Dual "Hello") (Dual "World"))
-- "WorldHello"
newtype Dual a = Dual { getDual :: a }
        deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1)

-- | @since 2.01
instance Monoid a => Monoid (Dual a) where
        mempty = Dual mempty
        Dual x `mappend` Dual y = Dual (y `mappend` x)

-- | @since 4.8.0.0
instance Functor Dual where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Dual where
    pure     = Dual
    (<*>)    = coerce

-- | @since 4.8.0.0
instance Monad Dual where
    m >>= k  = k (getDual m)

-- | The monoid of endomorphisms under composition.
--
-- >>> let computation = Endo ("Hello, " ++) <> Endo (++ "!")
-- >>> appEndo computation "Haskell"
-- "Hello, Haskell!"
newtype Endo a = Endo { appEndo :: a -> a }
               deriving (Generic)

-- | @since 2.01
instance Monoid (Endo a) where
        mempty = Endo id
        Endo f `mappend` Endo g = Endo (f . g)

-- | Boolean monoid under conjunction ('&&').
--
-- >>> getAll (All True <> mempty <> All False)
-- False
--
-- >>> getAll (mconcat (map (\x -> All (even x)) [2,4,6,7,8]))
-- False
newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded, Generic)

-- | @since 2.01
instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)

-- | Boolean monoid under disjunction ('||').
--
-- >>> getAny (Any True <> mempty <> Any False)
-- True
--
-- >>> getAny (mconcat (map (\x -> Any (even x)) [2,4,6,7,8]))
-- True
newtype Any = Any { getAny :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded, Generic)

-- | @since 2.01
instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)

-- | Monoid under addition.
--
-- >>> getSum (Sum 1 <> Sum 2 <> mempty)
-- 3
newtype Sum a = Sum { getSum :: a }
        deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

-- | @since 2.01
instance Num a => Monoid (Sum a) where
        mempty = Sum 0
        mappend = coerce ((+) :: a -> a -> a)
--        Sum x `mappend` Sum y = Sum (x + y)

-- | @since 4.8.0.0
instance Functor Sum where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Sum where
    pure     = Sum
    (<*>)    = coerce

-- | @since 4.8.0.0
instance Monad Sum where
    m >>= k  = k (getSum m)

-- | Monoid under multiplication.
--
-- >>> getProduct (Product 3 <> Product 4 <> mempty)
-- 12
newtype Product a = Product { getProduct :: a }
        deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

-- | @since 2.01
instance Num a => Monoid (Product a) where
        mempty = Product 1
        mappend = coerce ((*) :: a -> a -> a)
--        Product x `mappend` Product y = Product (x * y)

-- | @since 4.8.0.0
instance Functor Product where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Product where
    pure     = Product
    (<*>)    = coerce

-- | @since 4.8.0.0
instance Monad Product where
    m >>= k  = k (getProduct m)

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
newtype First a = First { getFirst :: Maybe a }
        deriving (Eq, Ord, Read, Show, Generic, Generic1,
                  Functor, Applicative, Monad)

-- | @since 2.01
instance Monoid (First a) where
        mempty = First Nothing
        First Nothing `mappend` r = r
        l `mappend` _             = l

-- | Maybe monoid returning the rightmost non-Nothing value.
--
-- @'Last' a@ is isomorphic to @'Dual' ('First' a)@, and thus to
-- @'Dual' ('Alt' 'Maybe' a)@
--
-- >>> getLast (Last (Just "hello") <> Last Nothing <> Last (Just "world"))
-- Just "world"
newtype Last a = Last { getLast :: Maybe a }
        deriving (Eq, Ord, Read, Show, Generic, Generic1,
                  Functor, Applicative, Monad)

-- | @since 2.01
instance Monoid (Last a) where
        mempty = Last Nothing
        l `mappend` Last Nothing = l
        _ `mappend` r            = r

-- | Monoid under '<|>'.
--
-- @since 4.8.0.0
newtype Alt f a = Alt {getAlt :: f a}
  deriving (Generic, Generic1, Read, Show, Eq, Ord, Num, Enum,
            Monad, MonadPlus, Applicative, Alternative, Functor)

-- | @since 4.8.0.0
instance Alternative f => Monoid (Alt f a) where
        mempty = Alt empty
        mappend = coerce ((<|>) :: f a -> f a -> f a)

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
