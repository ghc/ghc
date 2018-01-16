{-# LANGUAGE CPP, BangPatterns #-}

#include "fusion-phases.h"

-- | Define the closures for the array combinators the vectoriser uses.
--   The closures themselves use the *PD primitives defined in
--   dph-common:Data.Array.Parallel.Lifted.Combinators
--
--   For each combinator:
--    The *PA_v version is the "vectorised" version that has had its 
--    parameters closure converted. See zipWithPA_v for an example.
--
--    The *PA_l version is the "lifted" version that also works
--    on arrays of arrays.
--
--    The *PA version contains both of these wrapped up into a closure.
--    The output of the vectoriser uses these *PA versions directly, 
--    with applications being performed by the liftedApply function 
--    from "Data.Array.Parallel.Lifted.Closure"
--     
--    TODO: combine2PA_l isn't implemented and will just `error` if you
--          try to use it. None of our benchmarks do yet...
--
module Data.Array.Parallel.Lifted.Combinators (
  lengthPA, replicatePA, singletonPA, mapPA, crossMapPA,
  zipPA, zip3PA, zip4PA, zipWithPA, zipWith3PA, unzipPA, unzip3PA, unzip4PA ,
  zipWith4PA,
  packPA, filterPA, combine2PA, indexPA, concatPA, appPA, enumFromToPA_Int,
  indexedPA, slicePA, updatePA, bpermutePA,

  -- * Functions re-exported by Data.Array.Parallel.PArray
  lengthPA_v, replicatePA_v, singletonPA_v, zipPA_v,    unzipPA_v,
  packPA_v,   concatPA_v,    indexedPA_v,   updatePA_v, bpermutePA_v,
  slicePA_v,  indexPA_v,     appPA_v,       enumFromToPA_v
) where
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.PArray.PReprInstances
import Data.Array.Parallel.PArray.PDataInstances
import Data.Array.Parallel.PArray.ScalarInstances

import qualified Data.Array.Parallel.Unlifted   as U
import Data.Array.Parallel.Base                 (Tag)

import GHC.Exts                                 (Int(..), (+#))


-- length ---------------------------------------------------------------------
-- | Take the number of elements in an array.
lengthPA :: PA a => PArray a :-> Int
{-# INLINE lengthPA #-}
lengthPA = closure1 lengthPA_v lengthPA_l

lengthPA_v :: PA a => PArray a -> Int
{-# INLINE_PA lengthPA_v #-}
lengthPA_v xs = I# (lengthPA# xs)

lengthPA_l :: PA a => PArray (PArray a) -> PArray Int
{-# INLINE_PA lengthPA_l #-}
lengthPA_l xss = fromUArray' (U.elementsSegd segd) (U.lengthsSegd segd)
  where
    segd = segdPA# xss


-- replicate ------------------------------------------------------------------
-- | Produce a new array by replicating a single element the given number of times.
replicatePA :: PA a => Int :-> a :-> PArray a
{-# INLINE replicatePA #-}
replicatePA = closure2 replicatePA_v replicatePA_l

replicatePA_v :: PA a => Int -> a -> PArray a
{-# INLINE_PA replicatePA_v #-}
replicatePA_v (I# n#) x = replicatePA# n# x

replicatePA_l :: PA a => PArray Int -> PArray a -> PArray (PArray a)
{-# INLINE_PA replicatePA_l #-}
replicatePA_l (PArray n# (PInt ns)) (PArray _ xs)
  = PArray n# (PNested segd (replicatelPD segd xs))
  where
    segd = U.lengthsToSegd ns


-- singleton ------------------------------------------------------------------
-- | Produce an array containing a single element.
singletonPA :: PA a => a :-> PArray a
{-# INLINE singletonPA #-}
singletonPA = closure1 singletonPA_v singletonPA_l

singletonPA_v :: PA a => a -> PArray a
{-# INLINE_PA singletonPA_v #-}
singletonPA_v x = replicatePA_v 1 x

singletonPA_l :: PA a => PArray a -> PArray (PArray a)
{-# INLINE_PA singletonPA_l #-}
singletonPA_l (PArray n# xs)
  = PArray n# (PNested (U.mkSegd (U.replicate (I# n#) 1)
                                 (U.enumFromStepLen 0 1 (I# n#))
                                 (I# n#))
                       xs)


-- map ------------------------------------------------------------------------
-- | Apply a worker function to each element of an array, yielding a new array.
mapPA :: (PA a, PA b) => (a :-> b) :-> PArray a :-> PArray b
{-# INLINE mapPA #-}
mapPA = closure2 mapPA_v mapPA_l

-- | When performing a map we `replicate` the function into an array, then use
--   lifted-application to apply each function to its corresponding argument.
--
--   Note that this is a virtual replicate only, meaning that we can use
--   the same vectorised and lifted worker functions, provided we replicate
--   the environment part of the closure. The instance for repliatePA# in
--   PRepr class does exactly this, and it's defined in 
--   "Data.Array.Parallel.Lifted.Closure".
--
mapPA_v :: (PA a, PA b) => (a :-> b) -> PArray a -> PArray b
{-# INLINE_PA mapPA_v #-}
mapPA_v f as = replicatePA# (lengthPA# as) f $:^ as

mapPA_l :: (PA a, PA b)
        => PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
{-# INLINE_PA mapPA_l #-}
mapPA_l (PArray n# clo) (PArray _ xss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    PNested segd
  $ liftedApply (case U.elementsSegd segd of { I# k# -> k# })
                (replicatelPD segd clo)
                xs }
  

-- crossMap -------------------------------------------------------------------
-- TODO: What does this do?
crossMapPA :: (PA a, PA b) => (PArray a :-> (a :-> PArray b) :-> PArray (a,b))
{-# INLINE crossMapPA #-}
crossMapPA = closure2 crossMapPA_v crossMapPA_l

crossMapPA_v :: (PA a, PA b) => PArray a -> (a :-> PArray b) -> PArray (a,b)
{-# INLINE_PA crossMapPA_v #-}
crossMapPA_v as f
  = zipPA# (replicatelPA# (segdPA# bss) as) (concatPA# bss)
  where
    bss = mapPA_v f as

crossMapPA_l :: (PA a, PA b)
             => PArray (PArray a)
             -> PArray (a :-> PArray b)
             -> PArray (PArray (a,b))
{-# INLINE_PA crossMapPA_l #-}
crossMapPA_l ass fs = copySegdPA# bss (zipPA# as' (concatPA# bss))
  where
    bsss = mapPA_l fs ass
    bss  = concatPA_l bsss
    as' = replicatelPA# (segdPA# (concatPA# bsss)) (concatPA# ass)


-- zip ------------------------------------------------------------------------
-- |Turns a tuple of arrays into an array of the corresponding tuples.
--
--  If one array is short, excess elements of the longer array are discarded.

zipPA :: (PA a, PA b) => PArray a :-> PArray b :-> PArray (a, b)
{-# INLINE zipPA #-}
zipPA = closure2 zipPA_v zipPA_l

zipPA_v :: (PA a, PA b) => PArray a -> PArray b -> PArray (a, b)
{-# INLINE_PA zipPA_v #-}
zipPA_v xs ys = zipPA# xs ys

zipPA_l :: (PA a, PA b)
        => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
{-# INLINE_PA zipPA_l #-}
zipPA_l (PArray n# (PNested segd xs)) (PArray _ (PNested _ ys))
  = PArray n# (PNested segd (P_2 xs ys))


zip3PA :: (PA a, PA b, PA c) => PArray a :-> PArray b :-> PArray c :-> PArray (a, b, c)
{-# INLINE zip3PA #-}
zip3PA = closure3 zip3PA_v zip3PA_l

zip3PA_v :: (PA a, PA b, PA c) => PArray a -> PArray b -> PArray c -> PArray (a, b, c)
{-# INLINE_PA zip3PA_v #-}
zip3PA_v xs ys = zip3PA# xs ys

zip3PA_l :: (PA a, PA b, PA c)
         => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray c) -> PArray (PArray (a, b, c))
{-# INLINE_PA zip3PA_l #-}
zip3PA_l (PArray n# (PNested segd xs)) (PArray _ (PNested _ ys)) (PArray _ (PNested _ zs))
  = PArray n# (PNested segd (P_3 xs ys zs))



zip4PA :: (PA a, PA b, PA c, PA d) => PArray a :-> PArray b :-> PArray c :-> PArray d :-> PArray (a, b, c, d)
{-# INLINE zip4PA #-}
zip4PA = closure4 zip4PA_v zip4PA_l

zip4PA_v :: (PA a, PA b, PA c, PA d) => PArray a -> PArray b -> PArray c -> PArray d -> PArray (a, b, c, d)
{-# INLINE_PA zip4PA_v #-}
zip4PA_v xs ys = zip4PA# xs ys

zip4PA_l :: (PA a, PA b, PA c, PA d)
         => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray c) -> PArray (PArray d) -> PArray (PArray (a, b, c, d))
{-# INLINE_PA zip4PA_l #-}
zip4PA_l (PArray n# (PNested segd ws)) (PArray _ (PNested _ xs)) (PArray _ (PNested _ ys)) (PArray _ (PNested _ zs))
  = PArray n# (PNested segd (P_4 ws xs ys zs))


-- zipWith --------------------------------------------------------------------
-- |Map a function over multiple arrays at once.

zipWithPA :: (PA a, PA b, PA c)
          => (a :-> b :-> c) :-> PArray a :-> PArray b :-> PArray c
{-# INLINE zipWithPA #-}
zipWithPA = closure3 zipWithPA_v zipWithPA_l

zipWithPA_v :: (PA a, PA b, PA c)
            => (a :-> b :-> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA zipWithPA_v #-}
zipWithPA_v f as bs = replicatePA# (lengthPA# as) f $:^ as $:^ bs

zipWithPA_l :: (PA a, PA b, PA c)
            => PArray (a :-> b :-> c) -> PArray (PArray a) -> PArray (PArray b)
            -> PArray (PArray c)
{-# INLINE_PA zipWithPA_l #-}
zipWithPA_l fs ass bss
  = copySegdPA# ass
      (replicatelPA# (segdPA# ass) fs $:^ concatPA# ass $:^ concatPA# bss)


zipWith3PA :: (PA a, PA b, PA c, PA d)
           => (a :-> b :-> c :-> d) :-> PArray a :-> PArray b :-> PArray c :-> PArray d
{-# INLINE zipWith3PA #-}
zipWith3PA = closure4 zipWith3PA_v zipWith3PA_l

zipWith3PA_v :: (PA a, PA b, PA c, PA d)
             => (a :-> b :-> c :-> d) -> PArray a -> PArray b -> PArray c -> PArray d
{-# INLINE_PA zipWith3PA_v #-}
zipWith3PA_v f as bs cs = replicatePA# (lengthPA# as) f $:^ as $:^ bs $:^ cs

zipWith3PA_l :: (PA a, PA b, PA c, PA d)
             => PArray (a :-> b :-> c :-> d) 
             -> PArray (PArray a) -> PArray (PArray b) -> PArray (PArray c)
             -> PArray (PArray d)
{-# INLINE_PA zipWith3PA_l #-}
zipWith3PA_l fs ass bss css
  = copySegdPA# ass
      (replicatelPA# (segdPA# ass) fs $:^ concatPA# ass $:^ concatPA# bss $:^ concatPA# css)


zipWith4PA :: (PA a, PA b, PA c, PA d, PA e)
           => (a :-> b :-> c :-> d :-> e) :-> PArray a :-> PArray b :-> PArray c :-> PArray d :-> PArray e
{-# INLINE zipWith4PA #-}
zipWith4PA = closure5 zipWith4PA_v zipWith4PA_l

zipWith4PA_v :: (PA a, PA b, PA c, PA d, PA e)
             => (a :-> b :-> c :-> d :-> e) -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e
{-# INLINE_PA zipWith4PA_v #-}
zipWith4PA_v f as bs cs ds = replicatePA# (lengthPA# as) f $:^ as $:^ bs $:^ cs $:^ ds

zipWith4PA_l :: (PA a, PA b, PA c, PA d, PA e)
             => PArray (a :-> b :-> c :-> d :-> e) 
             -> PArray (PArray a) -> PArray (PArray b) -> PArray (PArray c)
             -> PArray (PArray d) -> PArray (PArray e)
{-# INLINE_PA zipWith4PA_l #-}
zipWith4PA_l fs ass bss css dss
  = copySegdPA# ass
      (replicatelPA# (segdPA# ass) fs $:^ concatPA# ass $:^ concatPA# bss $:^ concatPA# css $:^ concatPA# dss)


-- unzip ----------------------------------------------------------------------
-- |Transform an array of tuples into a tuple of arrays.

unzipPA :: (PA a, PA b) => PArray (a, b) :-> (PArray a, PArray b)
{-# INLINE unzipPA #-}
unzipPA = closure1 unzipPA_v unzipPA_l

unzipPA_v :: (PA a, PA b) => PArray (a, b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA_v #-}
unzipPA_v abs' = unzipPA# abs'

unzipPA_l :: (PA a, PA b) => PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
{-# INLINE_PA unzipPA_l #-}
unzipPA_l xyss = zipPA# (copySegdPA# xyss xs) (copySegdPA# xyss ys)
  where
    (xs, ys) = unzipPA# (concatPA# xyss)

unzip3PA :: (PA a, PA b, PA c) => PArray (a, b, c) :-> (PArray a, PArray b, PArray c)
{-# INLINE unzip3PA #-}
unzip3PA = closure1 unzip3PA_v unzip3PA_l

unzip3PA_v :: (PA a, PA b, PA c) => PArray (a, b, c) -> (PArray a, PArray b, PArray c)
{-# INLINE_PA unzip3PA_v #-}
unzip3PA_v abs' = unzip3PA# abs'

unzip3PA_l :: (PA a, PA b) => PArray (PArray (a, b, c)) -> PArray (PArray a, PArray b, PArray c)
{-# INLINE_PA unzip3PA_l #-}
unzip3PA_l xyzss = zip3PA# (copySegdPA# xyzss xs) (copySegdPA# xyzss ys) (copySegdPA# xyzss zs)
  where
    (xs, ys, zs) = unzip3PA# (concatPA# xyzss)


unzip4PA :: (PA a, PA b, PA c, PA d) => PArray (a, b, c, d) :-> (PArray a, PArray b, PArray c, PArray d)
{-# INLINE unzip4PA #-}
unzip4PA = closure1 unzip4PA_v unzip4PA_l

unzip4PA_v :: (PA a, PA b, PA c, PA d) => PArray (a, b, c, d) -> (PArray a, PArray b, PArray c, PArray d)
{-# INLINE_PA unzip4PA_v #-}
unzip4PA_v abs' = unzip4PA# abs'

unzip4PA_l :: (PA a, PA b, PA c) => PArray (PArray (a, b, c, d)) -> PArray (PArray a, PArray b, PArray c, PArray d)
{-# INLINE_PA unzip4PA_l #-}
unzip4PA_l wxyzss = zip4PA# (copySegdPA# wxyzss ws) (copySegdPA# wxyzss xs) (copySegdPA# wxyzss ys) (copySegdPA# wxyzss zs) 
  where
    (ws, xs, ys, zs) = unzip4PA# (concatPA# wxyzss) 


-- packPA ---------------------------------------------------------------------
-- | Select the elements of an array that have their tag set as True.
--   
-- @
-- packPA [12, 24, 42, 93] [True, False, False, True]
--  = [24, 42]
-- @
--
packPA :: PA a => PArray a :-> PArray Bool :-> PArray a
{-# INLINE packPA #-}
packPA = closure2 packPA_v packPA_l

packPA_v :: PA a => PArray a -> PArray Bool -> PArray a
{-# INLINE_PA packPA_v #-}
packPA_v xs bs
  = packByTagPA# xs (elementsSel2_1# sel) (U.tagsSel2 sel) 1#
  where
    sel = boolSel bs

packPA_l :: PA a
         => PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
{-# INLINE_PA packPA_l #-}
packPA_l (PArray n# xss) (PArray _ bss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    case bss of { PNested _ (PBool sel) ->
    PNested (U.lengthsToSegd $ U.count_s segd (U.tagsSel2 sel) 1)
  $ packByTagPD  xs (elementsSel2_1# sel) (U.tagsSel2 sel) 1# }}

boolSel :: PArray Bool -> U.Sel2
{-# INLINE boolSel #-}
boolSel (PArray _ (PBool sel)) = sel


-- combine --------------------------------------------------------------------
-- | Combine two arrays, using a tag array to tell us where to get each element from.
--
--   @combine2 [1,2,3] [4,5,6] [T,F,F,T,T,F] = [1,4,5,2,3,6]@
--
--   TODO: should the selector be a boolean array?
--
combine2PA:: PA a => PArray a :-> PArray a :-> PArray Tag :-> PArray a
{-# INLINE_PA combine2PA #-}
combine2PA = closure3 combine2PA_v combine2PA_l

combine2PA_v:: PA a => PArray a -> PArray a -> PArray Tag -> PArray a
{-# INLINE_PA combine2PA_v #-}
combine2PA_v xs ys bs
  = combine2PA# (lengthPA# xs +# lengthPA# ys)
                (U.tagsToSel2 (toUArray bs))
                xs ys

combine2PA_l
        :: PA a
        => PArray (PArray a) -> PArray (PArray a)
        -> PArray (PArray Tag)
        -> PArray (PArray a)
{-# INLINE_PA combine2PA_l #-}
combine2PA_l _ _ _ 
        = error "dph-common:Data.Array.Parallel.Lifted.Combinators: combinePA_l isn't implemented"


-- filter ---------------------------------------------------------------------
-- | Extract the elements from an array that match the given predicate.
filterPA :: PA a => (a :-> Bool) :-> PArray a :-> PArray a
{-# INLINE filterPA #-}
filterPA = closure2 filterPA_v filterPA_l

filterPA_v :: PA a => (a :-> Bool) -> PArray a -> PArray a
{-# INLINE_PA filterPA_v #-}
filterPA_v p xs = packPA_v xs (mapPA_v p xs)

filterPA_l :: PA a
           => PArray (a :-> Bool) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE_PA filterPA_l #-}
filterPA_l ps xss = packPA_l xss (mapPA_l ps xss)


-- index ----------------------------------------------------------------------
-- | Retrieve the array element with the given index.
indexPA :: PA a => PArray a :-> Int :-> a
{-# INLINE indexPA #-}
indexPA = closure2 indexPA_v indexPA_l

indexPA_v :: PA a => PArray a -> Int -> a
{-# INLINE_PA indexPA_v #-}
indexPA_v xs (I# i#) = indexPA# xs i#

indexPA_l :: PA a => PArray (PArray a) -> PArray Int -> PArray a
{-# INLINE_PA indexPA_l #-}
indexPA_l (PArray _ (PNested segd xs)) (PArray n# is)
  = PArray n#
  $ bpermutePD xs n#
                  (U.zipWith (+) (U.indicesSegd segd)
                                 (fromScalarPData is))


-- concat ---------------------------------------------------------------------
-- | Concatenate an array of arrays into a single array.
concatPA :: PA a => PArray (PArray a) :-> PArray a
{-# INLINE concatPA #-}
concatPA = closure1 concatPA_v concatPA_l

concatPA_v :: PA a => PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA_v #-}
concatPA_v xss = concatPA# xss

concatPA_l :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
{-# INLINE_PA concatPA_l #-}
concatPA_l (PArray m# (PNested segd1 (PNested segd2 xs)))
  = PArray m#
      (PNested (U.mkSegd (U.sum_s segd1 (U.lengthsSegd segd2))
                         (U.bpermute (U.indicesSegd segd2) (U.indicesSegd segd1))
                         (U.elementsSegd segd2))
               xs)


-- app (append) ---------------------------------------------------------------
-- | Append two arrays.
appPA :: PA a => PArray a :-> PArray a :-> PArray a
{-# INLINE appPA #-}
appPA = closure2 appPA_v appPA_l

appPA_v :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA_v #-}
appPA_v xs ys = appPA# xs ys

appPA_l :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE_PA appPA_l #-}
appPA_l (PArray m# pxss) (PArray n# pyss)
  = PArray (m# +# n#)
  $ case pxss of { PNested xsegd xs ->
    case pyss of { PNested ysegd ys ->
    let
      segd = U.plusSegd xsegd ysegd
    in
    PNested segd (applPD segd xsegd xs ysegd ys) }}


-- enumFromTo -----------------------------------------------------------------
-- | Produce a range of integers.
enumFromToPA_Int :: Int :-> Int :-> PArray Int
{-# INLINE enumFromToPA_Int #-}
enumFromToPA_Int = closure2 enumFromToPA_v enumFromToPA_l

enumFromToPA_v :: Int -> Int -> PArray Int
{-# INLINE_PA enumFromToPA_v #-}
enumFromToPA_v m n = fromUArray' (distance m n) (U.enumFromTo m n)

distance :: Int -> Int -> Int
{-# INLINE_STREAM distance #-}
distance m n = max 0 (n - m + 1)

enumFromToPA_l :: PArray Int -> PArray Int -> PArray (PArray Int)
{-# INLINE_PA enumFromToPA_l #-}
enumFromToPA_l (PArray m# ms) (PArray _ ns)
  = PArray m#
  $ PNested segd
  $ toScalarPData
  $ U.enumFromStepLenEach (U.elementsSegd segd)
        (fromScalarPData ms) (U.replicate (U.elementsSegd segd) 1) lens
  where
    lens = U.zipWith distance (fromScalarPData ms) (fromScalarPData ns)
    segd = U.lengthsToSegd lens


-- indexed --------------------------------------------------------------------
-- | Tag each element of an array with its index.
--
--   @indexed [42, 93, 13] = [(0, 42), (1, 93), (2, 13)]@ 
--
indexedPA :: PA a => PArray a :-> PArray (Int,a)
{-# INLINE indexedPA #-}
indexedPA = closure1 indexedPA_v indexedPA_l

indexedPA_v :: PA a => PArray a -> PArray (Int,a)
{-# INLINE indexedPA_v #-}
indexedPA_v (PArray n# xs)
  = PArray n# (P_2 (toScalarPData $ U.enumFromStepLen 0 1 (I# n#)) xs)

indexedPA_l :: PA a => PArray (PArray a) -> PArray (PArray (Int,a))
{-# INLINE indexedPA_l #-}
indexedPA_l (PArray n# xss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    PNested segd (P_2 (toScalarPData $ U.indices_s segd) xs) }


-- slice ----------------------------------------------------------------------
-- | Extract a subrange of elements from an array.
--   The first argument is the starting index, while the second is the 
--   length of the slice.
--  
slicePA :: PA a => Int :-> Int :-> PArray a :-> PArray a
{-# INLINE slicePA #-}
slicePA = closure3 slicePA_v slicePA_l

slicePA_v :: PA a => Int -> Int -> PArray a -> PArray a
{-# INLINE slicePA_v #-}
slicePA_v (I# from) (I# len) xs 
  = extractPA# xs from len 

-- TODO: Can we define this in terms of extractPA?
slicePA_l :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE slicePA_l #-}
slicePA_l (PArray n# is) (PArray _ lens) (PArray _ xss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    PNested segd'
  $ bpermutePD xs (elementsSegd# segd')
                  (U.zipWith (+) (U.indices_s segd')
                                 (U.replicate_s segd'
                                    (U.zipWith (+) (fromScalarPData is)
                                                   (U.indicesSegd segd)))) }
  where
    segd' = U.lengthsToSegd (fromScalarPData lens)



-- update ---------------------------------------------------------------------
-- | Copy the source array in the destination, using new values for the given indices.
updatePA :: PA a => PArray a :-> PArray (Int,a) :-> PArray a
{-# INLINE updatePA #-}
updatePA = closure2 updatePA_v updatePA_l

updatePA_v :: PA a => PArray a -> PArray (Int,a) -> PArray a
{-# INLINE_PA updatePA_v #-}
updatePA_v xs (PArray n# (P_2 is ys))
  = updatePA# xs (fromScalarPData is) (PArray n# ys)

updatePA_l
  :: PA a => PArray (PArray a) -> PArray (PArray (Int,a)) -> PArray (PArray a)
{-# INLINE_PA updatePA_l #-}
updatePA_l (PArray m# xss) (PArray _ pss)
  = PArray m#
  $ case xss of { PNested segd  xs ->
    case pss of { PNested segd' (P_2 is ys) ->
    PNested segd
  $ updatePD xs (U.zipWith (+) (fromScalarPData is)
                                (U.replicate_s segd' (U.indicesSegd segd)))
                ys }}


-- bpermute -------------------------------------------------------------------
-- | Backwards permutation of array elements.
--
--   @bpermute [50, 60, 20, 30] [0, 3, 2]  = [50, 30, 20]@
--
bpermutePA :: PA a => PArray a :-> PArray Int :-> PArray a
{-# INLINE bpermutePA #-}
bpermutePA = closure2 bpermutePA_v bpermutePA_l

bpermutePA_v :: PA a => PArray a -> PArray Int -> PArray a
{-# INLINE_PA bpermutePA_v #-}
bpermutePA_v xs (PArray n# is) = bpermutePA# xs n# (fromScalarPData is)

bpermutePA_l :: PA a => PArray (PArray a) -> PArray (PArray Int) -> PArray (PArray a)
{-# INLINE_PA bpermutePA_l #-}
bpermutePA_l (PArray _ xss) (PArray n# iss)
  = PArray n#
  $ case xss of { PNested segd  xs ->
    case iss of { PNested isegd is ->
    PNested isegd
  $ bpermutePD xs (elementsSegd# isegd)
                  (U.zipWith (+) (fromScalarPData is)
                                 (U.replicate_s isegd (U.indicesSegd segd))) }}


