{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

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
-- Note: no other base module should import this module.
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
        isTrue#,

        -- * Fusion
        build, augment,

        -- * Overloaded string literals
        IsString(..),

        -- * Debugging
        breakpoint, breakpointCond,

        -- * Ids with special behaviour
        lazy, inline,

        -- * Safe coercions
        GHC.Prim.coerce, GHC.Prim.Coercible,

        -- * Transform comprehensions
        Down(..), groupWith, sortWith, the,

        -- * Event logging
        traceEvent,

        -- * SpecConstr annotations
        SpecConstrAnnotation(..),

        -- * The call stack
        currentCallStack,

        -- * The Constraint kind
        Constraint,

        -- * Overloaded lists
        IsList(..)
       ) where

import Prelude

import GHC.Prim hiding (coerce, Coercible)
import qualified GHC.Prim
import GHC.Base hiding (coerce, Coercible)
import GHC.Word
import GHC.Int
import GHC.Ptr
import GHC.Stack
import Data.String
import Data.List
import Data.Data
import Data.Ord
import qualified Debug.Trace

-- XXX This should really be in Data.Tuple, where the definitions are
maxTupleSize :: Int
maxTupleSize = 62

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
-- projects an element out of every list element in order to first sort the
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
{-# DEPRECATED traceEvent "Use 'Debug.Trace.traceEvent' or 'Debug.Trace.traceEventIO'" #-} -- deprecated in 7.4


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


{- **********************************************************************
*									*
*              The IsList class                                         *
*									*
********************************************************************** -}

-- | The 'IsList' class and its methods are intended to be used in
--   conjunction with the OverloadedLists extension.
--
-- /Since: 4.7.0.0/
class IsList l where
  -- | The 'Item' type function returns the type of items of the structure
  --   @l@.
  type Item l

  -- | The 'fromList' function constructs the structure @l@ from the given
  --   list of @Item l@
  fromList  :: [Item l] -> l

  -- | The 'fromListN' function takes the input list's length as a hint. Its
  --   behaviour should be equivalent to 'fromList'. The hint can be used to
  --   construct the structure @l@ more efficiently compared to 'fromList'. If
  --   the given hint does not equal to the input list's length the behaviour of
  --   'fromListN' is not specified.
  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList

  -- | The 'toList' function extracts a list of @Item l@ from the structure @l@.
  --   It should satisfy fromList . toList = id.
  toList :: l -> [Item l]

instance IsList [a] where
  type (Item [a]) = a
  fromList = id
  toList = id
