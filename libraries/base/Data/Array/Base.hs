{-# OPTIONS -monly-3-regs #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Array.Base
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Base.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Basis for IArray and MArray.  Not intended for external consumption;
-- use IArray or MArray instead.
--
-----------------------------------------------------------------------------

module Data.Array.Base where

import Prelude

import Data.Ix		( Ix, range, index, rangeSize )

#ifdef __GLASGOW_HASKELL__
import GHC.Arr		( STArray, unsafeIndex )
import qualified GHC.Arr
import GHC.ST		( ST(..), runST )
import GHC.Base
import GHC.Word		( Word(..) )
import GHC.Ptr		( Ptr(..), FunPtr(..) )
import GHC.Float	( Float(..), Double(..) )
import GHC.Stable	( StablePtr(..) )
import GHC.Int		( Int8(..),  Int16(..),  Int32(..),  Int64(..) )
import GHC.Word		( Word8(..), Word16(..), Word32(..), Word64(..) )
#endif

import Data.Dynamic
#include "Dynamic.h"

-----------------------------------------------------------------------------
-- Class of immutable arrays

class HasBounds a where
    bounds :: Ix i => a i e -> (i,i)

class HasBounds a => IArray a e where
    unsafeArray      :: Ix i => (i,i) -> [(Int, e)] -> a i e
    unsafeAt         :: Ix i => a i e -> Int -> e
    unsafeReplace    :: Ix i => a i e -> [(Int, e)] -> a i e
    unsafeAccum      :: Ix i => (e -> e' -> e) -> a i e -> [(Int, e')] -> a i e
    unsafeAccumArray :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> a i e

    unsafeReplace arr ies = runST (unsafeReplaceST arr ies >>= unsafeFreeze)
    unsafeAccum f arr ies = runST (unsafeAccumST f arr ies >>= unsafeFreeze)
    unsafeAccumArray f e lu ies = runST (unsafeAccumArrayST f e lu ies >>= unsafeFreeze)

{-# INLINE unsafeReplaceST #-}
unsafeReplaceST :: (IArray a e, Ix i) => a i e -> [(Int, e)] -> ST s (STArray s i e)
unsafeReplaceST arr ies = do
    marr <- thaw arr
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    return marr

{-# INLINE unsafeAccumST #-}
unsafeAccumST :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumST f arr ies = do
    marr <- thaw arr
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    return marr

{-# INLINE unsafeAccumArrayST #-}
unsafeAccumArrayST :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumArrayST f e (l,u) ies = do
    marr <- newArray (l,u) e
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    return marr

{-# INLINE array #-}
array :: (IArray a e, Ix i) => (i,i) -> [(i, e)] -> a i e
array (l,u) ies = unsafeArray (l,u) [(index (l,u) i, e) | (i, e) <- ies]

-- Since unsafeFreeze is not guaranteed to be only a cast, we will
-- use unsafeArray and zip instead of a specialized loop to implement
-- listArray, unlike Array.listArray, even though it generates some
-- unnecessary heap allocation. Will use the loop only when we have
-- fast unsafeFreeze, namely for Array and UArray (well, they cover
-- almost all cases).

{-# INLINE listArray #-}
listArray :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
listArray (l,u) es = unsafeArray (l,u) (zip [0 .. rangeSize (l,u) - 1] es)

{-# INLINE listArrayST #-}
listArrayST :: Ix i => (i,i) -> [e] -> ST s (STArray s i e)
listArrayST (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

{-# RULES
"listArray/Array" listArray =
    \lu es -> runST (listArrayST lu es >>= GHC.Arr.unsafeFreezeSTArray)
    #-}

{-# INLINE listUArrayST #-}
listUArrayST :: (MArray (STUArray s) e (ST s), Ix i)
             => (i,i) -> [e] -> ST s (STUArray s i e)
listUArrayST (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

-- I don't know how to write a single rule for listUArrayST, because
-- the type looks like constrained over 's', which runST doesn't
-- like. In fact all MArray (STUArray s) instances are polymorphic
-- wrt. 's', but runST can't know that.

-- I would like to write a rule for listUArrayST (or listArray or
-- whatever) applied to unpackCString#. Unfortunately unpackCString#
-- calls seem to be floated out, then floated back into the middle
-- of listUArrayST, so I was not able to do this.

{-# RULES
"listArray/UArray/Bool"      listArray = \lu (es :: [Bool])        ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Char"      listArray = \lu (es :: [Char])        ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Int"       listArray = \lu (es :: [Int])         ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Word"      listArray = \lu (es :: [Word])        ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Ptr"       listArray = \lu (es :: [Ptr a])       ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/FunPtr"    listArray = \lu (es :: [FunPtr a])    ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Float"     listArray = \lu (es :: [Float])       ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Double"    listArray = \lu (es :: [Double])      ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/StablePtr" listArray = \lu (es :: [StablePtr a]) ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Int8"      listArray = \lu (es :: [Int8])        ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Int16"     listArray = \lu (es :: [Int16])       ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Int32"     listArray = \lu (es :: [Int32])       ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Int64"     listArray = \lu (es :: [Int64])       ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Word8"     listArray = \lu (es :: [Word8])       ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Word16"    listArray = \lu (es :: [Word16])      ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Word32"    listArray = \lu (es :: [Word32])      ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
"listArray/UArray/Word64"    listArray = \lu (es :: [Word64])      ->
    runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
    #-}

{-# INLINE (!) #-}
(!) :: (IArray a e, Ix i) => a i e -> i -> e
arr ! i | (l,u) <- bounds arr = unsafeAt arr (index (l,u) i)

{-# INLINE indices #-}
indices :: (HasBounds a, Ix i) => a i e -> [i]
indices arr | (l,u) <- bounds arr = range (l,u)

{-# INLINE elems #-}
elems :: (IArray a e, Ix i) => a i e -> [e]
elems arr | (l,u) <- bounds arr =
    [unsafeAt arr i | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE assocs #-}
assocs :: (IArray a e, Ix i) => a i e -> [(i, e)]
assocs arr | (l,u) <- bounds arr =
    [(i, unsafeAt arr (unsafeIndex (l,u) i)) | i <- range (l,u)]

{-# INLINE accumArray #-}
accumArray :: (IArray a e, Ix i) => (e -> e' -> e) -> e -> (i,i) -> [(i, e')] -> a i e
accumArray f init (l,u) ies =
    unsafeAccumArray f init (l,u) [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE (//) #-}
(//) :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e
arr // ies | (l,u) <- bounds arr =
    unsafeReplace arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE accum #-}
accum :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e
accum f arr ies | (l,u) <- bounds arr =
    unsafeAccum f arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE amap #-}
amap :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
amap f arr | (l,u) <- bounds arr =
    unsafeArray (l,u) [(i, f (unsafeAt arr i)) | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE ixmap #-}
ixmap :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
ixmap (l,u) f arr =
    unsafeArray (l,u) [(unsafeIndex (l,u) i, arr ! f i) | i <- range (l,u)]

-----------------------------------------------------------------------------
-- Normal polymorphic arrays

instance HasBounds GHC.Arr.Array where
    {-# INLINE bounds #-}
    bounds = GHC.Arr.bounds

instance IArray GHC.Arr.Array e where
    {-# INLINE unsafeArray #-}
    unsafeArray      = GHC.Arr.unsafeArray
    {-# INLINE unsafeAt #-}
    unsafeAt         = GHC.Arr.unsafeAt
    {-# INLINE unsafeReplace #-}
    unsafeReplace    = GHC.Arr.unsafeReplace
    {-# INLINE unsafeAccum #-}
    unsafeAccum      = GHC.Arr.unsafeAccum
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray = GHC.Arr.unsafeAccumArray

-----------------------------------------------------------------------------
-- Flat unboxed arrays

data UArray i e = UArray !i !i ByteArray#

INSTANCE_TYPEABLE2(UArray,uArrayTc,"UArray")

instance HasBounds UArray where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)

{-# INLINE unsafeArrayUArray #-}
unsafeArrayUArray :: (MArray (STUArray s) e (ST s), Ix i)
                  => (i,i) -> [(Int, e)] -> ST s (UArray i e)
unsafeArrayUArray (l,u) ies = do
    marr <- newArray_ (l,u)
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE unsafeFreezeSTUArray #-}
unsafeFreezeSTUArray :: STUArray s i e -> ST s (UArray i e)
unsafeFreezeSTUArray (STUArray l u marr#) = ST $ \s1# ->
    case unsafeFreezeByteArray# marr# s1# of { (# s2#, arr# #) ->
    (# s2#, UArray l u arr# #) }

{-# INLINE unsafeReplaceUArray #-}
unsafeReplaceUArray :: (MArray (STUArray s) e (ST s), Ix i)
                    => UArray i e -> [(Int, e)] -> ST s (UArray i e)
unsafeReplaceUArray arr ies = do
    marr <- thawSTUArray arr
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE unsafeAccumUArray #-}
unsafeAccumUArray :: (MArray (STUArray s) e (ST s), Ix i)
                  => (e -> e' -> e) -> UArray i e -> [(Int, e')] -> ST s (UArray i e)
unsafeAccumUArray f arr ies = do
    marr <- thawSTUArray arr
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE unsafeAccumArrayUArray #-}
unsafeAccumArrayUArray :: (MArray (STUArray s) e (ST s), Ix i)
                       => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> ST s (UArray i e)
unsafeAccumArrayUArray f init (l,u) ies = do
    marr <- newArray (l,u) init
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE eqUArray #-}
eqUArray :: (IArray UArray e, Ix i, Eq e) => UArray i e -> UArray i e -> Bool
eqUArray arr1@(UArray l1 u1 _) arr2@(UArray l2 u2 _) =
    if rangeSize (l1,u1) == 0 then rangeSize (l2,u2) == 0 else
    l1 == l2 && u1 == u2 &&
    and [unsafeAt arr1 i == unsafeAt arr2 i | i <- [0 .. rangeSize (l1,u1) - 1]]

{-# INLINE cmpUArray #-}
cmpUArray :: (IArray UArray e, Ix i, Ord e) => UArray i e -> UArray i e -> Ordering
cmpUArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntUArray #-}
cmpIntUArray :: (IArray UArray e, Ord e) => UArray Int e -> UArray Int e -> Ordering
cmpIntUArray arr1@(UArray l1 u1 _) arr2@(UArray l2 u2 _) =
    if rangeSize (l1,u1) == 0 then if rangeSize (l2,u2) == 0 then EQ else LT else
    if rangeSize (l2,u2) == 0 then GT else
    case compare l1 l2 of
        EQ    -> foldr cmp (compare u1 u2) [0 .. rangeSize (l1, min u1 u2) - 1]
        other -> other
    where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpUArray/Int" cmpUArray = cmpIntUArray #-}

showsUArray :: (IArray UArray e, Ix i, Show i, Show e)
            => Int -> UArray i e -> ShowS
showsUArray p a =
    showParen (p > 9) $
    showString "array " .
    shows (bounds a) .
    showChar ' ' .
    shows (assocs a)

-----------------------------------------------------------------------------
-- Flat unboxed arrays: instances

instance IArray UArray Bool where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) =
        (indexWordArray# arr# (bOOL_INDEX i#) `and#` bOOL_BIT i#)
        `neWord#` int2Word# 0#
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Char where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = C# (indexWideCharArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = I# (indexIntArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = W# (indexWordArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray (Ptr a) where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = Ptr (indexAddrArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray (FunPtr a) where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = FunPtr (indexAddrArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Float where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = F# (indexFloatArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Double where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = D# (indexDoubleArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray (StablePtr a) where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = StablePtr (indexStablePtrArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int8 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = I8# (indexInt8Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int16 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = I16# (indexInt16Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int32 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = I32# (indexInt32Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int64 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = I64# (indexInt64Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word8 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = W8# (indexWord8Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word16 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = W16# (indexWord16Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word32 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = W32# (indexWord32Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word64 where
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies)
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ arr#) (I# i#) = W64# (indexWord64Array# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance Ix ix => Eq (UArray ix Bool) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Char) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Int) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Word) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix (Ptr a)) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix (FunPtr a)) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Float) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Double) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix (StablePtr a)) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Int8) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Int16) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Int32) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Int64) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Word8) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Word16) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Word32) where
    (==) = eqUArray

instance Ix ix => Eq (UArray ix Word64) where
    (==) = eqUArray

instance Ix ix => Ord (UArray ix Bool) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Char) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Int) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Word) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix (Ptr a)) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix (FunPtr a)) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Float) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Double) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Int8) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Int16) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Int32) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Int64) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Word8) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Word16) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Word32) where
    compare = cmpUArray

instance Ix ix => Ord (UArray ix Word64) where
    compare = cmpUArray

instance (Ix ix, Show ix) => Show (UArray ix Bool) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Char) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Int) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Word) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Float) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Double) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Int8) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Int16) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Int32) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Int64) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Word8) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Word16) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Word32) where
    showsPrec = showsUArray

instance (Ix ix, Show ix) => Show (UArray ix Word64) where
    showsPrec = showsUArray

-----------------------------------------------------------------------------
-- Mutable arrays

{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "MArray: undefined array element"

class (HasBounds a, Monad m) => MArray a e m where
    newArray    :: Ix i => (i,i) -> e -> m (a i e)
    newArray_   :: Ix i => (i,i) -> m (a i e)
    unsafeRead  :: Ix i => a i e -> Int -> m e
    unsafeWrite :: Ix i => a i e -> Int -> e -> m ()

    newArray (l,u) init = do
        marr <- newArray_ (l,u)
        sequence_ [unsafeWrite marr i init | i <- [0 .. rangeSize (l,u) - 1]]
        return marr

    newArray_ (l,u) = newArray (l,u) arrEleBottom

    -- newArray takes an initialiser which all elements of
    -- the newly created array are initialised to.  newArray_ takes
    -- no initialiser, it is assumed that the array is initialised with
    -- "undefined" values.

    -- why not omit newArray_?  Because in the unboxed array case we would
    -- like to omit the initialisation altogether if possible.  We can't do
    -- this for boxed arrays, because the elements must all have valid values
    -- at all times in case of garbage collection.

    -- why not omit newArray?  Because in the boxed case, we can omit the
    -- default initialisation with undefined values if we *do* know the
    -- initial value and it is constant for all elements.

{-# INLINE newListArray #-}
newListArray :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)
newListArray (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

{-# INLINE readArray #-}
readArray :: (MArray a e m, Ix i) => a i e -> i -> m e
readArray marr i | (l,u) <- bounds marr =
    unsafeRead marr (index (l,u) i)

{-# INLINE writeArray #-}
writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
writeArray marr i e | (l,u) <- bounds marr =
    unsafeWrite marr (index (l,u) i) e

{-# INLINE getElems #-}
getElems :: (MArray a e m, Ix i) => a i e -> m [e]
getElems marr | (l,u) <- bounds marr =
    sequence [unsafeRead marr i | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE getAssocs #-}
getAssocs :: (MArray a e m, Ix i) => a i e -> m [(i, e)]
getAssocs marr | (l,u) <- bounds marr =
    sequence [do e <- unsafeRead marr (index (l,u) i); return (i,e)
              | i <- range (l,u)]

{-# INLINE mapArray #-}
mapArray :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
mapArray f marr | (l,u) <- bounds marr = do
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- unsafeRead marr i
        unsafeWrite marr' i (f e)
        | i <- [0 .. rangeSize (l,u) - 1]]
    return marr'

{-# INLINE mapIndices #-}
mapIndices :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)
mapIndices (l,u) f marr = do
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- readArray marr (f i)
        unsafeWrite marr' (unsafeIndex (l,u) i) e
        | i <- range (l,u)]
    return marr'

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (ST monad)

instance HasBounds (STArray s) where
    {-# INLINE bounds #-}
    bounds = GHC.Arr.boundsSTArray

instance MArray (STArray s) e (ST s) where
    {-# INLINE newArray #-}
    newArray    = GHC.Arr.newSTArray
    {-# INLINE unsafeRead #-}
    unsafeRead  = GHC.Arr.unsafeReadSTArray
    {-# INLINE unsafeWrite #-}
    unsafeWrite = GHC.Arr.unsafeWriteSTArray

-----------------------------------------------------------------------------
-- Typeable instance for STArray

sTArrayTc :: TyCon
sTArrayTc = mkTyCon "STArray"

instance (Typeable a, Typeable b, Typeable c) => Typeable (STArray a b c) where
  typeOf a = mkAppTy sTArrayTc [typeOf ((undefined :: STArray a b c -> a) a),
				typeOf ((undefined :: STArray a b c -> b) a),
				typeOf ((undefined :: STArray a b c -> c) a)]

-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (ST monad)

data STUArray s i a = STUArray !i !i (MutableByteArray# s)

INSTANCE_TYPEABLE3(STUArray,stUArrayTc,"STUArray")

instance HasBounds (STUArray s) where
    {-# INLINE bounds #-}
    bounds (STUArray l u _) = (l,u)

instance MArray (STUArray s) Bool (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (bOOL_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWordArray# marr# (bOOL_INDEX i#) s1# of { (# s2#, e# #) ->
        (# s2#, (e# `and#` bOOL_BIT i#) `neWord#` int2Word# 0# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) e = ST $ \s1# ->
        case bOOL_INDEX i#              of { j# ->
        case readWordArray# marr# j# s1# of { (# s2#, old# #) ->
        case if e then old# `or#` bOOL_BIT i#
             else old# `and#` bOOL_NOT_BIT i# of { e# ->
        case writeWordArray# marr# j# e# s2# of { s3# ->
        (# s3#, () #) }}}}

instance MArray (STUArray s) Char (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 4#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWideCharArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, C# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (C# e#) = ST $ \s1# ->
        case writeWideCharArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Int (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (wORD_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readIntArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (I# e#) = ST $ \s1# ->
        case writeIntArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Word (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (wORD_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWordArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (W# e#) = ST $ \s1# ->
        case writeWordArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) (Ptr a) (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (wORD_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readAddrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, Ptr e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (Ptr e#) = ST $ \s1# ->
        case writeAddrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) (FunPtr a) (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (wORD_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readAddrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, FunPtr e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (FunPtr e#) = ST $ \s1# ->
        case writeAddrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Float (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (fLOAT_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readFloatArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, F# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (F# e#) = ST $ \s1# ->
        case writeFloatArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Double (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (dOUBLE_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readDoubleArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, D# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (D# e#) = ST $ \s1# ->
        case writeDoubleArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) (StablePtr a) (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (wORD_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readStablePtrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2# , StablePtr e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (StablePtr e#) = ST $ \s1# ->
        case writeStablePtrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Int8 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# n# s1#       of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readInt8Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I8# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (I8# e#) = ST $ \s1# ->
        case writeInt8Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Int16 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 2#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readInt16Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I16# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (I16# e#) = ST $ \s1# ->
        case writeInt16Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Int32 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 4#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readInt32Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I32# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (I32# e#) = ST $ \s1# ->
        case writeInt32Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Int64 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 8#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readInt64Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I64# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (I64# e#) = ST $ \s1# ->
        case writeInt64Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Word8 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# n# s1#       of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWord8Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W8# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (W8# e#) = ST $ \s1# ->
        case writeWord8Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Word16 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 2#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWord16Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W16# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (W16# e#) = ST $ \s1# ->
        case writeWord16Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Word32 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 4#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWord32Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W32# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (W32# e#) = ST $ \s1# ->
        case writeWord32Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray (STUArray s) Word64 (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (n# *# 8#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWord64Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W64# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (W64# e#) = ST $ \s1# ->
        case writeWord64Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

-----------------------------------------------------------------------------
-- Translation between elements and bytes

#include "config.h"

bOOL_SCALE, wORD_SCALE, dOUBLE_SCALE, fLOAT_SCALE :: Int# -> Int#
bOOL_SCALE   n# = bOOL_INDEX (n# +# last#) where I# last# = SIZEOF_VOID_P - 1
wORD_SCALE   n# = scale# *# n# where I# scale# = SIZEOF_VOID_P
dOUBLE_SCALE n# = scale# *# n# where I# scale# = SIZEOF_DOUBLE
fLOAT_SCALE  n# = scale# *# n# where I# scale# = SIZEOF_FLOAT

bOOL_INDEX :: Int# -> Int#
#if SIZEOF_VOID_P == 4
bOOL_INDEX i# = i# `iShiftRA#` 5#
#else
bOOL_INDEX i# = i# `iShiftRA#` 6#
#endif

bOOL_BIT, bOOL_NOT_BIT :: Int# -> Word#
bOOL_BIT     n# = int2Word# 1# `shiftL#` (word2Int# (int2Word# n# `and#` mask#))
    where W# mask# = SIZEOF_VOID_P * 8 - 1
bOOL_NOT_BIT n# = bOOL_BIT n# `xor#` mb# where W# mb# = maxBound

-----------------------------------------------------------------------------
-- Freezing

freeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freeze marr | (l,u) <- bounds marr = do
    ies <- sequence [do e <- unsafeRead marr i; return (i,e)
                     | i <- [0 .. rangeSize (l,u) - 1]]
    return (unsafeArray (l,u) ies)

freezeSTUArray :: Ix i => STUArray s i e -> ST s (UArray i e)
freezeSTUArray (STUArray l u marr#) = ST $ \s1# ->
    case sizeofMutableByteArray# marr#  of { n# ->
    case newByteArray# n# s1#           of { (# s2#, marr'# #) ->
    case unsafeCoerce# memcpy marr'# marr# n# s2# of { (# s3#, () #) ->
    case unsafeFreezeByteArray# marr'# s3# of { (# s4#, arr# #) ->
    (# s4#, UArray l u arr# #) }}}}

{-# RULES
"freeze/STArray"  freeze = GHC.Arr.freezeSTArray
"freeze/STUArray" freeze = freezeSTUArray
    #-}

-- In-place conversion of mutable arrays to immutable ones places
-- a proof obligation on the user: no other parts of your code can
-- have a reference to the array at the point where you unsafely
-- freeze it (and, subsequently mutate it, I suspect).

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

{-# RULES
"unsafeFreeze/STArray"  unsafeFreeze = GHC.Arr.unsafeFreezeSTArray
"unsafeFreeze/STUArray" unsafeFreeze = unsafeFreezeSTUArray
    #-}

-----------------------------------------------------------------------------
-- Thawing

thaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thaw arr | (l,u) <- bounds arr = do
    marr <- newArray_ (l,u)
    sequence_ [unsafeWrite marr i (unsafeAt arr i)
               | i <- [0 .. rangeSize (l,u) - 1]]
    return marr

thawSTUArray :: Ix i => UArray i e -> ST s (STUArray s i e)
thawSTUArray (UArray l u arr#) = ST $ \s1# ->
    case sizeofByteArray# arr#          of { n# ->
    case newByteArray# n# s1#           of { (# s2#, marr# #) ->
    case unsafeCoerce# memcpy marr# arr# n# s2# of { (# s3#, () #) ->
    (# s3#, STUArray l u marr# #) }}}

foreign import "memcpy" unsafe
    memcpy :: MutableByteArray# RealWorld -> ByteArray# -> Int# -> IO ()

{-# RULES
"thaw/STArray"  thaw = GHC.Arr.thawSTArray
"thaw/STUArray" thaw = thawSTUArray
    #-}

-- In-place conversion of immutable arrays to mutable ones places
-- a proof obligation on the user: no other parts of your code can
-- have a reference to the array at the point where you unsafely
-- thaw it (and, subsequently mutate it, I suspect).

{-# INLINE unsafeThaw #-}
unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = thaw

{-# INLINE unsafeThawSTUArray #-}
unsafeThawSTUArray :: Ix i => UArray i e -> ST s (STUArray s i e)
unsafeThawSTUArray (UArray l u marr#) =
    return (STUArray l u (unsafeCoerce# marr#))

{-# RULES
"unsafeThaw/STArray"    unsafeThaw = GHC.Arr.unsafeThawSTArray
"unsafeThaw/STUArray"   unsafeThaw = unsafeThawSTUArray
    #-}
