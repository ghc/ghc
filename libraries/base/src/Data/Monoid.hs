{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Monoid
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
-- >>> import Data.Int
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

module Data.Monoid
    (-- *  'Monoid' typeclass
     Monoid(..),
     (<>),
     Dual(..),
     Endo(..),
     -- *  'Bool' wrappers
     All(..),
     Any(..),
     -- *  'Num' wrappers
     Sum(..),
     Product(..),
     -- *  'Maybe' wrappers
     -- $MaybeExamples
     First(..),
     Last(..),
     -- *  'Alternative' wrapper
     Alt(..),
     -- *  'Applicative' wrapper
     Ap(..)
     ) where

import GHC.Internal.Data.Monoid

