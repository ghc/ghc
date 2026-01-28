{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Module      :  Data.Ord
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Orderings
--

module Data.Ord
    (Ord(..),
     Ordering(..),
     Down(..),
     comparing,
     clamp
     ) where

import Prelude ( Ord(..), Ordering(..) )
import Data.Ord.Down ( Down(..) )

-- $setup
-- >>> import Prelude

-- |
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... sortBy (comparing fst) ...
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

-- |
-- > clamp (low, high) a = min high (max a low)
--
-- Function for ensuring the value @a@ is within the inclusive bounds given by
-- @low@ and @high@. If it is, @a@ is returned unchanged. The result
-- is otherwise @low@ if @a <= low@, or @high@ if @high <= a@.
--
-- When clamp is used at Double and Float, it has NaN propagating semantics in
-- its second argument. That is, @clamp (l,h) NaN = NaN@, but @clamp (NaN, NaN)
-- x = x@.
--
-- >>> clamp (0, 10) 2
-- 2
--
-- >>> clamp ('a', 'm') 'x'
-- 'm'
--
-- @since base-4.16.0.0
clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)
