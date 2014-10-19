{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The tuple data types, and associated functions.
--
-----------------------------------------------------------------------------

module Data.Tuple
  ( fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

import GHC.Base ()      -- Note [Depend on GHC.Tuple]

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
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

-- | Swap the components of a pair.
swap                    :: (a,b) -> (b,a)
swap (a,b)              = (b,a)
