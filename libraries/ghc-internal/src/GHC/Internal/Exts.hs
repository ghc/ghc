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
-- Module      :  GHC.Internal.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC Extensions: this is the Approved Way to get at GHC-specific extensions.
--
-- Note: no other base module should import this module.
-----------------------------------------------------------------------------

module GHC.Internal.Exts
       (
        -- ** Pointer types
        Ptr(..), FunPtr(..),

        -- ** Other primitive types
        module GHC.Types,

        -- ** Legacy interface for arrays of arrays
        module GHC.Internal.ArrayArray,

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
        unsafePtrEquality#,
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
        samePromptTag#,

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
        currentCallStackIds,
        CostCentreId,

        -- * Ids with special behaviour
        inline, noinline, lazy, oneShot, considerAccessible, seq#,

        -- * SpecConstr annotations
        SpecConstrAnnotation(..), SPEC (..),

        -- * Coercions
        -- ** Safe coercions
        --
        -- | These are available from the /Trustworthy/ module "Data.Coerce" as well.
        --
        -- @since base-4.7.0.0
        GHC.Internal.Data.Coerce.coerce,

        -- ** Very unsafe coercion
        unsafeCoerce#,

        -- ** Casting class dictionaries with single methods
        --
        --   @since base-4.17.0.0
        WithDict(..),

        -- * Converting ADTs to constructor tags
        DataToTag(..),

        -- * The maximum tuple size
        maxTupleSize,
       ) where

import GHC.Prim hiding ( coerce, dataToTagSmall#, dataToTagLarge#, whereFrom# )
  -- Hide dataToTagLarge# because it is expected to break for
  -- GHC-internal reasons in the near future, and shouldn't
  -- be exposed from base (not even GHC.Exts)
  -- whereFrom# is similarly internal.

import GHC.Types
  hiding ( IO   -- Exported from "GHC.IO"
         , Type -- Exported from "Data.Kind"

           -- GHC's internal representation of 'TyCon's, for 'Typeable'
         , Module, TrName, TyCon, TypeLitSort, KindRep, KindBndr,
         Unit#,
         Solo#,
         Tuple0#,
         Tuple1#,
         Tuple2#,
         Tuple3#,
         Tuple4#,
         Tuple5#,
         Tuple6#,
         Tuple7#,
         Tuple8#,
         Tuple9#,
         Tuple10#,
         Tuple11#,
         Tuple12#,
         Tuple13#,
         Tuple14#,
         Tuple15#,
         Tuple16#,
         Tuple17#,
         Tuple18#,
         Tuple19#,
         Tuple20#,
         Tuple21#,
         Tuple22#,
         Tuple23#,
         Tuple24#,
         Tuple25#,
         Tuple26#,
         Tuple27#,
         Tuple28#,
         Tuple29#,
         Tuple30#,
         Tuple31#,
         Tuple32#,
         Tuple33#,
         Tuple34#,
         Tuple35#,
         Tuple36#,
         Tuple37#,
         Tuple38#,
         Tuple39#,
         Tuple40#,
         Tuple41#,
         Tuple42#,
         Tuple43#,
         Tuple44#,
         Tuple45#,
         Tuple46#,
         Tuple47#,
         Tuple48#,
         Tuple49#,
         Tuple50#,
         Tuple51#,
         Tuple52#,
         Tuple53#,
         Tuple54#,
         Tuple55#,
         Tuple56#,
         Tuple57#,
         Tuple58#,
         Tuple59#,
         Tuple60#,
         Tuple61#,
         Tuple62#,
         Tuple63#,
         Tuple64#,
         Sum2#,
         Sum3#,
         Sum4#,
         Sum5#,
         Sum6#,
         Sum7#,
         Sum8#,
         Sum9#,
         Sum10#,
         Sum11#,
         Sum12#,
         Sum13#,
         Sum14#,
         Sum15#,
         Sum16#,
         Sum17#,
         Sum18#,
         Sum19#,
         Sum20#,
         Sum21#,
         Sum22#,
         Sum23#,
         Sum24#,
         Sum25#,
         Sum26#,
         Sum27#,
         Sum28#,
         Sum29#,
         Sum30#,
         Sum31#,
         Sum32#,
         Sum33#,
         Sum34#,
         Sum35#,
         Sum36#,
         Sum37#,
         Sum38#,
         Sum39#,
         Sum40#,
         Sum41#,
         Sum42#,
         Sum43#,
         Sum44#,
         Sum45#,
         Sum46#,
         Sum47#,
         Sum48#,
         Sum49#,
         Sum50#,
         Sum51#,
         Sum52#,
         Sum53#,
         Sum54#,
         Sum55#,
         Sum56#,
         Sum57#,
         Sum58#,
         Sum59#,
         Sum60#,
         Sum61#,
         Sum62#,
         Sum63#,
  )
import qualified GHC.Prim.Ext
import GHC.Internal.ArrayArray
import GHC.Internal.Base hiding ( coerce )
import GHC.Internal.IO (seq#)
import GHC.Internal.Ptr
import GHC.Internal.Stack
import GHC.Internal.IsList (IsList(..)) -- for re-export

import qualified GHC.Internal.Data.Coerce
import GHC.Internal.Data.String
import GHC.Internal.Data.OldList
import GHC.Internal.Data.Data
import GHC.Internal.Data.Ord
import qualified GHC.Internal.Debug.Trace
import GHC.Internal.Unsafe.Coerce ( unsafeCoerce# ) -- just for re-export

-- XXX This should really be in Data.Tuple, where the definitions are
maxTupleSize :: Int
maxTupleSize = 64

-- | 'the' ensures that all the elements of the list are identical
-- and then returns that unique element
the :: Eq a => [a] -> a
the (x:xs)
  | all (x ==) xs = x
  | otherwise     = errorWithoutStackTrace "GHC.Internal.Exts.the: non-identical elements"
the []            = errorWithoutStackTrace "GHC.Internal.Exts.the: empty list"

-- | The 'sortWith' function sorts a list of elements using the
-- user supplied function to project something out of each element
--
-- In general if the user supplied function is expensive to compute then
-- you should probably be using 'GHC.Internal.Data.List.sortOn', as it only needs
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

{-# INLINE [0] groupByFB #-} -- See Note [Inline FB functions] in GHC.Internal.List
groupByFB :: ([a] -> lst -> lst) -> lst -> (a -> a -> Bool) -> [a] -> lst
groupByFB c n eq xs0 = groupByFBCore xs0
  where groupByFBCore [] = n
        groupByFBCore (x:xs) = c (x:ys) (groupByFBCore zs)
            where (ys, zs) = span (eq x) xs


-- -----------------------------------------------------------------------------
-- tracing

traceEvent :: String -> IO ()
traceEvent = GHC.Internal.Debug.Trace.traceEventIO
{-# DEPRECATED traceEvent "Use 'GHC.Internal.Debug.Trace.traceEvent' or 'GHC.Internal.Debug.Trace.traceEventIO'" #-} -- deprecated in 7.4


{- **********************************************************************
*                                                                       *
*              SpecConstr annotation                                    *
*                                                                       *
********************************************************************** -}

-- | Deprecated, use 'SPEC' directly instead.
--
-- Annotating a type with 'NoSpecConstr' will make @SpecConstr@
-- not specialise for arguments of that type,
-- e. g., @{-# ANN type SPEC ForceSpecConstr #-}@.

-- This data type is defined here, rather than in the SpecConstr module
-- itself, so that importing it doesn't force stupidly linking the
-- entire ghc package at runtime

data SpecConstrAnnotation = NoSpecConstr | ForceSpecConstr
                deriving ( Data -- ^ @since base-4.3.0.0
                         , Eq   -- ^ @since base-4.3.0.0
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
-- @since base-4.14.0.0
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
