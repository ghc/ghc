{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC Extensions: this is the Approved Way to get at GHC-specific extensions.
--
-----------------------------------------------------------------------------

module GHC.Exts
       (
        -- * Representations of some basic types
        Int(..),Word(..),Float(..),Double(..),
        Char(..),
        Ptr(..), FunPtr(..),

        -- * The maximum tuple size
        maxTupleSize,

        -- * Primitive operations
        module GHC.Prim,
        shiftL#, shiftRL#, iShiftL#, iShiftRA#, iShiftRL#,
        uncheckedShiftL64#, uncheckedShiftRL64#,
        uncheckedIShiftL64#, uncheckedIShiftRA64#,

        -- * Fusion
        build, augment,

        -- * Overloaded string literals
        IsString(..),

        -- * Debugging
        breakpoint, breakpointCond,

        -- * Ids with special behaviour
        lazy, inline,

        -- * Transform comprehensions
        Down(..), groupWith, sortWith, the,

        -- * Event logging
        traceEvent,

        -- * SpecConstr annotations
        SpecConstrAnnotation(..),

        -- * The call stack
        currentCallStack,

        -- * The Constraint kind
        Constraint
       ) where

import Prelude

import GHC.Prim
import GHC.Base
import GHC.Magic
import GHC.Word
import GHC.Int
import GHC.Ptr
import GHC.Stack
import Data.String
import Data.List
import Data.Data
import qualified Debug.Trace

-- XXX This should really be in Data.Tuple, where the definitions are
maxTupleSize :: Int
maxTupleSize = 62

-- | The 'Down' type allows you to reverse sort order conveniently.  A value of type
-- @'Down' a@ contains a value of type @a@ (represented as @'Down' a@).
-- If @a@ has an @'Ord'@ instance associated with it then comparing two
-- values thus wrapped will give you the opposite of their normal sort order.
-- This is particularly useful when sorting in generalised list comprehensions,
-- as in: @then sortWith by 'Down' x@
newtype Down a = Down a deriving (Eq)

instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x

-- | 'the' ensures that all the elements of the list are identical
-- and then returns that unique element
the :: Eq a => [a] -> a
the (x:xs)
  | all (x ==) xs = x
  | otherwise     = error "GHC.Exts.the: non-identical elements"
the []            = error "GHC.Exts.the: empty list"

-- | The 'sortWith' function sorts a list of elements using the
-- user supplied function to project something out of each element
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

-- | The 'groupWith' function uses the user supplied function which
-- projects an element out of every list element in order to to first sort the 
-- input list and then to form groups by equality on these projected elements
{-# INLINE groupWith #-}
groupWith :: Ord b => (a -> b) -> [a] -> [[a]]
groupWith f xs = build (\c n -> groupByFB c n (\x y -> f x == f y) (sortWith f xs))

groupByFB :: ([a] -> lst -> lst) -> lst -> (a -> a -> Bool) -> [a] -> lst
groupByFB c n eq xs0 = groupByFBCore xs0
  where groupByFBCore [] = n
        groupByFBCore (x:xs) = c (x:ys) (groupByFBCore zs)
            where (ys, zs) = span (eq x) xs


-- -----------------------------------------------------------------------------
-- tracing

traceEvent :: String -> IO ()
traceEvent = Debug.Trace.traceEventIO
{-# DEPRECATED traceEvent "Use Debug.Trace.traceEvent or Debug.Trace.traceEventIO" #-}


{- **********************************************************************
*									*
*              SpecConstr annotation                                    *
*									*
********************************************************************** -}

-- Annotating a type with NoSpecConstr will make SpecConstr 
-- not specialise for arguments of that type.

-- This data type is defined here, rather than in the SpecConstr module
-- itself, so that importing it doesn't force stupidly linking the
-- entire ghc package at runtime

data SpecConstrAnnotation = NoSpecConstr | ForceSpecConstr
                deriving( Data, Typeable, Eq )

