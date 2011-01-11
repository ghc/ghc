
{-# LANGUAGE NoImplicitPrelude, CPP, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer.Type
-- Copyright   :  (c) Ian Lynagh 2007-2008
-- License     :  BSD3
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- An simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Integer.Type (
    Integer(..),
    Positive, Positives,
    Digits(..), Digit,
    List(..)
 ) where

import GHC.Prim
import GHC.Types ()

#if !defined(__HADDOCK__)

data Integer = Positive !Positive | Negative !Positive | Naught

-------------------------------------------------------------------
-- The hard work is done on positive numbers

-- Least significant bit is first

-- Positive's have the property that they contain at least one Bit,
-- and their last Bit is One.
type Positive = Digits
type Positives = List Positive

data Digits = Some !Digit !Digits
            | None
type Digit = Word#

-- XXX Could move [] above us
data List a = Nil | Cons a (List a)

#endif

