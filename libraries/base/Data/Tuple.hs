{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Functions associated with the tuple data types.
--
-----------------------------------------------------------------------------

module Data.Tuple
  ( Solo (..)
  , fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

import GHC.Base ()      -- Note [Depend on GHC.Tuple]
import GHC.Tuple (Solo (..))

default ()              -- Double isn't available yet

-- ---------------------------------------------------------------------------
-- Standard functions over tuples

-- | Extract the first component of a pair.
fst                     :: (a,b) -> a
fst (x,_)               =  x

-- | Extract the second component of a pair.
snd                     :: (a,b) -> b
snd (_,y)               =  y

-- | 'curry' converts an uncurried function to a curried function.
--
-- ==== __Examples__
--
-- >>> curry fst 1 2
-- 1
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
--
-- ==== __Examples__
--
-- >>> uncurry (+) (1,2)
-- 3
--
-- >>> uncurry ($) (show, 1)
-- "1"
--
-- >>> map (uncurry max) [(1,2), (3,4), (6,8)]
-- [2,4,8]
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

-- | Swap the components of a pair.
swap                    :: (a,b) -> (b,a)
swap (a,b)              = (b,a)

-- $setup
-- >>> import Prelude hiding (curry, uncurry, fst, snd)
