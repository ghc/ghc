-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ix
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class of index types.
--
-- The 'Ix' class is used to map a continuous subrange of values in a type onto
-- integers. It is used primarily for array indexing (see Section 6
-- <http://www.haskell.org/onlinelibrary/array.html#arrays>). The 'Ix'
-- class contains the methods range, index, and inRange. The 'index' operation
-- maps a bounding pair, which defines the lower and upper bounds of the range,
-- and a subscript, to an integer. The 'range' operation enumerates all
-- subscripts; the 'inRange' operation tells whether a particular subscript
-- lies in the range defined by a bounding pair.
-- 
-- An implementation is entitled to assume the following laws about these
-- operations: 
-- 
-- >        range (l,u) !! index (l,u) i == i   -- when i is in range
-- 
-- >        inRange (l,u) i == i `elem` range (l,u)
-- 
-----------------------------------------------------------------------------
module Data.Ix
    (
	Ix
	  ( range	-- :: (Ix a) => (a,a) -> [a]
	  , index       -- :: (Ix a) => (a,a) -> a   -> Int
	  , inRange     -- :: (Ix a) => (a,a) -> a   -> Bool
	  , rangeSize	-- :: (Ix a) => (a,a) -> Int
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

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.

    -- * Deriving Instances of Ix
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
    -- >	data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    -- 
    -- we would have: 
    -- 
    -- >	range   (Yellow,Blue)        ==  [Yellow,Green,Blue]
    -- >	index   (Yellow,Blue) Green  ==  1
    -- >	inRange (Yellow,Blue) Red    ==  False
    -- 
    -- * For single-constructor datatypes, the derived instance declarations
    -- are as shown for tuples in Figure 1
    -- <http://www.haskell.org/onlinelibrary/ix.html#prelude-index>.

    ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Arr
#endif

#ifdef __HUGS__
import Hugs.Prelude( Ix(..) )
#endif

#ifdef __NHC__
import Ix (Ix(..))
#endif

