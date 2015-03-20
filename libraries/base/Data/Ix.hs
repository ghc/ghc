{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ix
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The 'Ix' class is used to map a contiguous subrange of values in
-- type onto integers.  It is used primarily for array indexing
-- (see the array package).  'Ix' uses row-major order.
-- 
-----------------------------------------------------------------------------

module Data.Ix
    (
    -- * The 'Ix' class
        Ix
          ( range
          , index
          , inRange
          , rangeSize
          )
    -- Ix instances:
    --
    --  Ix Char
    --  Ix Int
    --  Ix Integer
    --  Ix Bool
    --  Ix Ordering
    --  Ix ()
    --  (Ix a, Ix b) => Ix (a, b)
    --  ...

    -- * Deriving Instances of 'Ix'
    -- | Derived instance declarations for the class 'Ix' are only possible
    -- for enumerations (i.e. datatypes having only nullary constructors)
    -- and single-constructor datatypes, including arbitrarily large tuples,
    -- whose constituent types are instances of 'Ix'. 
    -- 
    -- * For an enumeration, the nullary constructors are assumed to be
    -- numbered left-to-right with the indices being 0 to n-1 inclusive. This
    -- is the same numbering defined by the 'Enum' class. For example, given
    -- the datatype: 
    -- 
    -- >        data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    -- 
    -- we would have: 
    -- 
    -- >        range   (Yellow,Blue)        ==  [Yellow,Green,Blue]
    -- >        index   (Yellow,Blue) Green  ==  1
    -- >        inRange (Yellow,Blue) Red    ==  False
    -- 
    -- * For single-constructor datatypes, the derived instance declarations
    -- are as shown for tuples in chapter 19, section 2 of the Haskell 2010 report:
    -- <https://www.haskell.org/onlinereport/haskell2010/haskellch19.html>.

    ) where

import GHC.Arr
