{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Monoid
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- A type @a@ is a 'Monoid' if it provides an associative function ('<>')
-- that lets you combine any two values of type @a@ into one, and a neutral
-- element (`mempty`) such that
--
-- > a <> mempty == mempty <> a == a
--
-- A 'Monoid' is a 'Semigroup' with the added requirement of a neutral element.
-- Thus any 'Monoid' is a 'Semigroup', but not the other way around.
--
-- ==== __Examples__
--
-- The 'Sum' monoid is defined by the numerical addition operator and `0` as neutral element:
--
-- >>> import Data.Int (Int)
-- >>> mempty :: Sum Int
-- Sum {getSum = 0}
-- >>> Sum 1 <> Sum 2 <> Sum 3 <> Sum 4 :: Sum Int
-- Sum {getSum = 10}
--
-- We can combine multiple values in a list into a single value using the `mconcat` function.
-- Note that we have to specify the type here since 'Int' is a monoid under several different
-- operations:
--
-- >>> mconcat [1,2,3,4] :: Sum Int
-- Sum {getSum = 10}
-- >>> mconcat [] :: Sum Int
-- Sum {getSum = 0}
--
-- Another valid monoid instance of 'Int' is 'Product' It is defined by multiplication
-- and `1` as neutral element:
--
-- >>> Product 1 <> Product 2 <> Product 3 <> Product 4 :: Product Int
-- Product {getProduct = 24}
-- >>> mconcat [1,2,3,4] :: Product Int
-- Product {getProduct = 24}
-- >>> mconcat [] :: Product Int
-- Product {getProduct = 1}
--
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.Monoid (
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

-- $setup
-- >>> import Data.Int

-- Push down the module in the dependency hierarchy.
import GHC.Internal.Base hiding (Any)
import GHC.Internal.Enum
import GHC.Internal.Generics
import GHC.Internal.Num
import GHC.Internal.Read
import GHC.Internal.Show

import GHC.Internal.Control.Monad.Fail (MonadFail)

import GHC.Internal.Data.Semigroup.Internal

-- $MaybeExamples
-- To implement @find@ or @findLast@ on any 'Data.Foldable.Foldable':
--
-- @
-- findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findLast pred = getLast . foldMap (\x -> if pred x
--                                            then Last (Just x)
--                                            else Last Nothing)
-- @
--
-- Much of 'Data.Map.Lazy.Map's interface can be implemented with
-- 'Data.Map.Lazy.alter'. Some of the rest can be implemented with a new
-- 'Data.Map.Lazy.alterF' function and either 'First' or 'Last':
--
-- > alterF :: (Functor f, Ord k) =>
-- >           (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
-- >
-- > instance Monoid a => Functor ((,) a)  -- from GHC.Internal.Data.Functor
--
-- @
-- insertLookupWithKey :: Ord k => (k -> v -> v -> v) -> k -> v
--                     -> Map k v -> (Maybe v, Map k v)
-- insertLookupWithKey combine key value =
--   Arrow.first getFirst . 'Data.Map.Lazy.alterF' doChange key
--   where
--   doChange Nothing = (First Nothing, Just value)
--   doChange (Just oldValue) =
--     (First (Just oldValue),
--      Just (combine key value oldValue))
-- @


-- | Maybe monoid returning the leftmost non-'Nothing' value.
--
-- @'First' a@ is isomorphic to @'Alt' 'Maybe' a@, but precedes it
-- historically.
--
-- Beware that @Data.Monoid.@'First' is different from
-- @Data.Semigroup.@'Data.Semigroup.First'. The former returns the first non-'Nothing',
-- so @Data.Monoid.First Nothing <> x = x@. The latter simply returns the first value,
-- thus @Data.Semigroup.First Nothing <> x = Data.Semigroup.First Nothing@.
--
-- ==== __Examples__
--
-- >>> First (Just "hello") <> First Nothing <> First (Just "world")
-- First {getFirst = Just "hello"}
--
-- >>> First Nothing <> mempty
-- First {getFirst = Nothing}
newtype First a = First { getFirst :: Maybe a }
        deriving ( Eq          -- ^ @since base-2.01
                 , Ord         -- ^ @since base-2.01
                 , Read        -- ^ @since base-2.01
                 , Show        -- ^ @since base-2.01
                 , Generic     -- ^ @since base-4.7.0.0
                 , Generic1    -- ^ @since base-4.7.0.0
                 , Functor     -- ^ @since base-4.8.0.0
                 , Applicative -- ^ @since base-4.8.0.0
                 , Monad       -- ^ @since base-4.8.0.0
                 )

-- | @since base-4.9.0.0
instance Semigroup (First a) where
        First Nothing <> b = b
        a             <> _ = a
        stimes = stimesIdempotentMonoid
        sconcat (first@(First m) :| rest)
          | Nothing <- m = mconcat rest
          | otherwise    = first


-- | @since base-2.01
instance Monoid (First a) where
        mempty = First Nothing

-- | Maybe monoid returning the rightmost non-'Nothing' value.
--
-- @'Last' a@ is isomorphic to @'Dual' ('First' a)@, and thus to
-- @'Dual' ('Alt' 'Maybe' a)@
--
-- @Data.Semigroup.@'Data.Semigroup.Last'. The former returns the last non-'Nothing',
-- so @x <> Data.Monoid.Last Nothing = x@. The latter simply returns the last value,
-- thus @x <> Data.Semigroup.Last Nothing = Data.Semigroup.Last Nothing@.
--
-- ==== __Examples__
--
-- >>> Last (Just "hello") <> Last Nothing <> Last (Just "world")
-- Last {getLast = Just "world"}
--
-- >>> Last Nothing <> mempty
-- Last {getLast = Nothing}
newtype Last a = Last { getLast :: Maybe a }
        deriving ( Eq          -- ^ @since base-2.01
                 , Ord         -- ^ @since base-2.01
                 , Read        -- ^ @since base-2.01
                 , Show        -- ^ @since base-2.01
                 , Generic     -- ^ @since base-4.7.0.0
                 , Generic1    -- ^ @since base-4.7.0.0
                 , Functor     -- ^ @since base-4.8.0.0
                 , Applicative -- ^ @since base-4.8.0.0
                 , Monad       -- ^ @since base-4.8.0.0
                 )

-- | @since base-4.9.0.0
instance Semigroup (Last a) where
        a <> Last Nothing = a
        _ <> b                   = b
        stimes = stimesIdempotentMonoid

-- | @since base-2.01
instance Monoid (Last a) where
        mempty = Last Nothing

-- | This data type witnesses the lifting of a 'Monoid' into an
-- 'Applicative' pointwise.
--
-- ==== __Examples__
--
-- >>> Ap (Just [1, 2, 3]) <> Ap Nothing
-- Ap {getAp = Nothing}
--
-- >>> Ap [Sum 10, Sum 20] <> Ap [Sum 1, Sum 2]
-- Ap {getAp = [Sum {getSum = 11},Sum {getSum = 12},Sum {getSum = 21},Sum {getSum = 22}]}
--
-- @since base-4.12.0.0
newtype Ap f a = Ap { getAp :: f a }
        deriving ( Alternative -- ^ @since base-4.12.0.0
                 , Applicative -- ^ @since base-4.12.0.0
                 , Enum        -- ^ @since base-4.12.0.0
                 , Eq          -- ^ @since base-4.12.0.0
                 , Functor     -- ^ @since base-4.12.0.0
                 , Generic     -- ^ @since base-4.12.0.0
                 , Generic1    -- ^ @since base-4.12.0.0
                 , Monad       -- ^ @since base-4.12.0.0
                 , MonadFail   -- ^ @since base-4.12.0.0
                 , MonadPlus   -- ^ @since base-4.12.0.0
                 , Ord         -- ^ @since base-4.12.0.0
                 , Read        -- ^ @since base-4.12.0.0
                 , Show        -- ^ @since base-4.12.0.0
                 )

-- | @since base-4.12.0.0
instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
        (Ap x) <> (Ap y) = Ap $ liftA2 (<>) x y

-- | @since base-4.12.0.0
instance (Applicative f, Monoid a) => Monoid (Ap f a) where
        mempty = Ap $ pure mempty

-- | @since base-4.12.0.0
instance (Applicative f, Bounded a) => Bounded (Ap f a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- | Note that even if the underlying 'Num' and 'Applicative' instances are
-- lawful, for most 'Applicative's, this instance will not be lawful. If you use
-- this instance with the list 'Applicative', the following customary laws will
-- not hold:
--
-- Commutativity:
--
-- >>> Ap [10,20] + Ap [1,2]
-- Ap {getAp = [11,12,21,22]}
-- >>> Ap [1,2] + Ap [10,20]
-- Ap {getAp = [11,21,12,22]}
--
-- Additive inverse:
--
-- >>> Ap [] + negate (Ap [])
-- Ap {getAp = []}
-- >>> fromInteger 0 :: Ap [] Int
-- Ap {getAp = [0]}
--
-- Distributivity:
--
-- >>> Ap [1,2] * (3 + 4)
-- Ap {getAp = [7,14]}
-- >>> (Ap [1,2] * 3) + (Ap [1,2] * 4)
-- Ap {getAp = [7,11,10,14]}
--
-- @since base-4.12.0.0
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
