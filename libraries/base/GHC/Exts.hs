{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

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
        -- ** Pointer types
        Ptr(..), FunPtr(..),

        -- ** Other primitive types
        module GHC.Types,

        -- ** Legacy interface for arrays of arrays
        module GHC.ArrayArray,

        -- * Primitive operations

        module GHC.Prim,
        module GHC.Prim.Ext,

        -- ** Running 'RealWorld' state thread
        runRW#,

        -- ** Bit shift operations
        shiftL#, shiftRL#, iShiftL#, iShiftRA#, iShiftRL#,

        -- ** Pointer comparison operations
        -- See `Note [Pointer comparison operations]` in primops.txt.pp
        reallyUnsafePtrEquality,
        eqStableName#,
        sameArray#,
        sameMutableArray#,
        sameSmallArray#,
        sameSmallMutableArray#,
        sameByteArray#,
        sameMutableByteArray#,
        sameMVar#,
        sameMutVar#,
        sameTVar#,
        sameIOPort#,

        -- ** Compat wrapper
        atomicModifyMutVar#,

        -- ** Resize functions
        --
        -- | Resizing arrays of boxed elements is currently handled in
        -- library space (rather than being a primop) since there is not
        -- an efficient way to grow arrays. However, resize operations
        -- may become primops in a future release of GHC.
        resizeSmallMutableArray#,

        -- ** Fusion
        build, augment,

        -- * Overloaded lists
        IsList(..),

        -- * Transform comprehensions
        Down(..), groupWith, sortWith, the,

        -- * Strings
        -- ** Overloaded string literals
        IsString(..),

        -- ** CString
        unpackCString#,
        unpackAppendCString#,
        unpackFoldrCString#,
        unpackCStringUtf8#,
        unpackNBytes#,
        cstringLength#,

        -- * Debugging
        -- ** Breakpoints
        breakpoint, breakpointCond,

        -- ** Event logging
        traceEvent,

        -- ** The call stack
        currentCallStack,

        -- * Ids with special behaviour
        inline, noinline, lazy, oneShot, considerAccessible,

        -- * SpecConstr annotations
        SpecConstrAnnotation(..), SPEC (..),

        -- * Coercions
        -- ** Safe coercions
        --
        -- | These are available from the /Trustworthy/ module "Data.Coerce" as well.
        --
        -- @since 4.7.0.0
        Data.Coerce.coerce,

        -- ** Very unsafe coercion
        unsafeCoerce#,

        -- ** Casting class dictionaries with single methods
        WithDict(..),

        -- * The maximum tuple size
        maxTupleSize,
       ) where

import GHC.Prim hiding ( coerce )
import GHC.Types
  hiding ( IO   -- Exported from "GHC.IO"
         , Type -- Exported from "Data.Kind"

           -- GHC's internal representation of 'TyCon's, for 'Typeable'
         , Module, TrName, TyCon, TypeLitSort, KindRep, KindBndr )
import qualified GHC.Prim.Ext
import GHC.ArrayArray
import GHC.Base hiding ( coerce )
import GHC.Ptr
import GHC.Stack
import GHC.IsList (IsList(..)) -- for re-export

import qualified Data.Coerce
import Data.String
import Data.OldList
import Data.Data
import Data.Ord
import qualified Debug.Trace
import Unsafe.Coerce ( unsafeCoerce# ) -- just for re-export

-- XXX This should really be in Data.Tuple, where the definitions are
maxTupleSize :: Int
maxTupleSize = 64

-- | 'the' ensures that all the elements of the list are identical
-- and then returns that unique element
the :: Eq a => [a] -> a
the (x:xs)
  | all (x ==) xs = x
  | otherwise     = errorWithoutStackTrace "GHC.Exts.the: non-identical elements"
the []            = errorWithoutStackTrace "GHC.Exts.the: empty list"

-- | The 'sortWith' function sorts a list of elements using the
-- user supplied function to project something out of each element
--
-- In general if the user supplied function is expensive to compute then
-- you should probably be using 'Data.List.sortOn', as it only needs
-- to compute it once for each element. 'sortWith', on the other hand
-- must compute the mapping function for every comparison that it performs.
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

-- | The 'groupWith' function uses the user supplied function which
-- projects an element out of every list element in order to first sort the
-- input list and then to form groups by equality on these projected elements
{-# INLINE groupWith #-}
groupWith :: Ord b => (a -> b) -> [a] -> [[a]]
groupWith f xs = build (\c n -> groupByFB c n (\x y -> f x == f y) (sortWith f xs))

{-# INLINE [0] groupByFB #-} -- See Note [Inline FB functions] in GHC.List
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
*                                                                       *
*              SpecConstr annotation                                    *
*                                                                       *
********************************************************************** -}

-- Annotating a type with NoSpecConstr will make SpecConstr
-- not specialise for arguments of that type.

-- This data type is defined here, rather than in the SpecConstr module
-- itself, so that importing it doesn't force stupidly linking the
-- entire ghc package at runtime

data SpecConstrAnnotation = NoSpecConstr | ForceSpecConstr
                deriving ( Data -- ^ @since 4.3.0.0
                         , Eq   -- ^ @since 4.3.0.0
                         )



-- | An implementation of the old @atomicModifyMutVar#@ primop in
-- terms of the new 'atomicModifyMutVar2#' primop, for backwards
-- compatibility. The type of this function is a bit bogus. It's
-- best to think of it as having type
--
-- @
-- atomicModifyMutVar#
--   :: MutVar# s a
--   -> (a -> (a, b))
--   -> State# s
--   -> (# State# s, b #)
-- @
--
-- but there may be code that uses this with other two-field record
-- types.
atomicModifyMutVar#
  :: MutVar# s a
  -> (a -> b)
  -> State# s
  -> (# State# s, c #)
atomicModifyMutVar# mv f s =
  case unsafeCoerce# (atomicModifyMutVar2# mv f s) of
    (# s', _, ~(_, res) #) -> (# s', res #)

-- | Resize a mutable array to new specified size. The returned
-- 'SmallMutableArray#' is either the original 'SmallMutableArray#'
-- resized in-place or, if not possible, a newly allocated
-- 'SmallMutableArray#' with the original content copied over.
--
-- To avoid undefined behaviour, the original 'SmallMutableArray#' shall
-- not be accessed anymore after a 'resizeSmallMutableArray#' has been
-- performed. Moreover, no reference to the old one should be kept in order
-- to allow garbage collection of the original 'SmallMutableArray#'  in
-- case a new 'SmallMutableArray#' had to be allocated.
--
-- @since 4.14.0.0
resizeSmallMutableArray#
  :: SmallMutableArray# s a -- ^ Array to resize
  -> Int# -- ^ New size of array
  -> a
     -- ^ Newly created slots initialized to this element.
     -- Only used when array is grown.
  -> State# s
  -> (# State# s, SmallMutableArray# s a #)
resizeSmallMutableArray# arr0 szNew a s0 =
  case getSizeofSmallMutableArray# arr0 s0 of
    (# s1, szOld #) -> if isTrue# (szNew <# szOld)
      then case shrinkSmallMutableArray# arr0 szNew s1 of
        s2 -> (# s2, arr0 #)
      else if isTrue# (szNew ># szOld)
        then case newSmallArray# szNew a s1 of
          (# s2, arr1 #) -> case copySmallMutableArray# arr0 0# arr1 0# szOld s2 of
            s3 -> (# s3, arr1 #)
        else (# s1, arr0 #)

-- | Semantically, @considerAccessible = True@. But it has special meaning
-- to the pattern-match checker, which will never flag the clause in which
-- 'considerAccessible' occurs as a guard as redundant or inaccessible.
-- Example:
--
-- > case (x, x) of
-- >   (True,  True)  -> 1
-- >   (False, False) -> 2
-- >   (True,  False) -> 3 -- Warning: redundant
--
-- The pattern-match checker will warn here that the third clause is redundant.
-- It will stop doing so if the clause is adorned with 'considerAccessible':
--
-- > case (x, x) of
-- >   (True,  True)  -> 1
-- >   (False, False) -> 2
-- >   (True,  False) | considerAccessible -> 3 -- No warning
--
-- Put 'considerAccessible' as the last statement of the guard to avoid get
-- confusing results from the pattern-match checker, which takes \"consider
-- accessible\" by word.
considerAccessible :: Bool
considerAccessible = True
