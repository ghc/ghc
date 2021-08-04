{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Builders
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD-style
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides generic builder functions. These functions construct
-- values of a given type.
-----------------------------------------------------------------------------

module Data.Generics.Builders (empty, constrs) where

import Data.Data
import Data.Generics.Aliases (extB)

-- | Construct the empty value for a datatype. For algebraic datatypes, the
-- leftmost constructor is chosen.
empty :: forall a. Data a => a
empty = general 
      `extB` char 
      `extB` int
      `extB` integer
      `extB` float 
      `extB` double where
  -- Generic case
  general :: Data a => a
  general = fromConstrB empty (indexConstr (dataTypeOf general) 1)
  
  -- Base cases
  char    = '\NUL'
  int     = 0      :: Int
  integer = 0      :: Integer
  float   = 0.0    :: Float
  double  = 0.0    :: Double


-- | Return a list of values of a datatype. Each value is one of the possible
-- constructors of the datatype, populated with 'empty' values.
constrs :: forall a. Data a => [a]
constrs = general
      `extB` char
      `extB` int
      `extB` integer
      `extB` float
      `extB` double where
  -- Generic case
  general :: Data a => [a]
  general = map (fromConstrB empty)
              (dataTypeConstrs (dataTypeOf (unList general))) where
    unList :: Data a => [a] -> a
    unList = undefined

  -- Base cases
  char    = "\NUL"
  int     = [0   :: Int]
  integer = [0   :: Integer]
  float   = [0.0 :: Float]
  double  = [0.0 :: Double]
