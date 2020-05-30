{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash, UnboxedTuples, TypeFamilies, DeriveDataTypeable,
             MultiParamTypeClasses, FlexibleInstances, NoImplicitPrelude #-}

{-# OPTIONS_HADDOCK not-home #-}

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
        module GHC.Prim.Ext,
        shiftL#, shiftRL#, iShiftL#, iShiftRA#, iShiftRL#,
        uncheckedShiftL64#, uncheckedShiftRL64#,
        uncheckedIShiftL64#, uncheckedIShiftRA64#,
        isTrue#,

        -- * Compat wrapper
        atomicModifyMutVar#,

        -- * Resize functions
        --
        -- | Resizing arrays of boxed elements is currently handled in
        -- library space (rather than being a primop) since there is not
        -- an efficient way to grow arrays. However, resize operations
        -- may become primops in a future release of GHC.
        resizeSmallMutableArray#,

        -- * Fusion
        build, augment,

        -- * Overloaded string literals
        IsString(..),

        -- * CString
        unpackCString#,
        unpackAppendCString#,
        unpackFoldrCString#,
        unpackCStringUtf8#,
        unpackNBytes#,
        cstringLength#,

        -- * Debugging
        breakpoint, breakpointCond,

        -- * Ids with special behaviour
        inline, noinline, lazy, oneShot,

        -- * Running 'RealWorld' state thread
        runRW#,

        -- * Safe coercions
        --
        -- | These are available from the /Trustworthy/ module "Data.Coerce" as well
        --
        -- @since 4.7.0.0
        Data.Coerce.coerce, Data.Coerce.Coercible,

        -- * Very unsafe coercion
        unsafeCoerce#,

        -- * Equality
        type (~~),

        -- * Representation polymorphism
        GHC.Prim.TYPE, RuntimeRep(..), VecCount(..), VecElem(..),

        -- * Transform comprehensions
        Down(..), groupWith, sortWith, the,

        -- * Event logging
        traceEvent,

        -- * The call stack
        currentCallStack,

        -- * The Constraint kind
        Constraint,

        -- * The Any type
        Any,

        -- * Overloaded lists
        IsList(..)
       ) where

import GHC.Prim hiding ( coerce, TYPE )
import qualified GHC.Prim
import qualified GHC.Prim.Ext
import GHC.Base hiding ( coerce )
import GHC.Word
import GHC.Int
import GHC.Ptr
import GHC.Stack

import qualified Data.Coerce
import Data.String
import Data.OldList
import Data.Ord
import Data.Version ( Version(..), makeVersion )
import qualified Debug.Trace
import Unsafe.Coerce ( unsafeCoerce# ) -- just for re-export

import Control.Applicative (ZipList(..))

-- XXX This should really be in Data.Tuple, where the definitions are
maxTupleSize :: Int
maxTupleSize = 62

-- | 'the' ensures that all the elements of the list are identical
-- and then returns that unique element
the :: Eq a => [a] -> a
the (x:xs)
  | all (x ==) xs = x
  | otherwise     = errorWithoutStackTrace "GHC.Exts.the: non-identical elements"
the []            = errorWithoutStackTrace "GHC.Exts.the: empty list"

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
*              The IsList class                                         *
*                                                                       *
********************************************************************** -}

-- | The 'IsList' class and its methods are intended to be used in
--   conjunction with the OverloadedLists extension.
--
-- @since 4.7.0.0
class IsList l where
  -- | The 'Item' type function returns the type of items of the structure
  --   @l@.
  type Item l

  -- | The 'fromList' function constructs the structure @l@ from the given
  --   list of @Item l@
  fromList  :: [Item l] -> l

  -- | The 'fromListN' function takes the input list's length and potentially
  --   uses it to construct the structure @l@ more efficiently compared to 
  --   'fromList'. If the given number does not equal to the input list's length 
  --   the behaviour of 'fromListN' is not specified.
  --
  --   prop> fromListN (length xs) xs == fromList xs
  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList

  -- | The 'toList' function extracts a list of @Item l@ from the structure @l@.
  --   It should satisfy fromList . toList = id.
  toList :: l -> [Item l]

-- | @since 4.7.0.0
instance IsList [a] where
  type (Item [a]) = a
  fromList = id
  toList = id

-- | @since 4.15.0.0
instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList = ZipList
  toList = getZipList

-- | @since 4.9.0.0
instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a

  fromList (a:as) = a :| as
  fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"

  toList ~(a :| as) = a : as

-- | @since 4.8.0.0
instance IsList Version where
  type (Item Version) = Int
  fromList = makeVersion
  toList = versionBranch

-- | Be aware that 'fromList . toList = id' only for unfrozen 'CallStack's,
-- since 'toList' removes frozenness information.
--
-- @since 4.9.0.0
instance IsList CallStack where
  type (Item CallStack) = (String, SrcLoc)
  fromList = fromCallSiteList
  toList   = getCallStack

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
