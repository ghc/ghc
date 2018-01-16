{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Unboxed (
  Segd, elementsSegd#, mkSegd#,
  Sel2, elementsSel2_0#, elementsSel2_1#, replicateSel2#, {-pickSel2#,-} tagsSel2,

  PArray_Int#,
  lengthPA_Int#, emptyPA_Int#,
  replicatePA_Int#, replicatelPA_Int#, repeatPA_Int#,
  indexPA_Int#, extractPA_Int#, bpermutePA_Int#,
  appPA_Int#, applPA_Int#,
  packPA_Int#, pack'PA_Int#, combine2PA_Int#, combine2'PA_Int#,
  fromListPA_Int#,
  upToPA_Int#, enumFromToPA_Int#, enumFromThenToPA_Int#,
  enumFromStepLenPA_Int#,
  selectPA_Int#, selectorToIndices2PA#,
  -- lengthSegdPA#, lengthsSegdPA#, indicesSegdPA#, elementsSegdPA#,
  -- lengthsToSegdPA#, mkSegdPA#,
  sumPA_Int#, sumPAs_Int#,
  unsafe_mapPA_Int#, unsafe_zipWithPA_Int#, unsafe_foldPA_Int#,
  unsafe_scanPA_Int#,

  PArray_Word8#,
  lengthPA_Word8#, emptyPA_Word8#,
  replicatePA_Word8#, replicatelPA_Word8#, repeatPA_Word8#,
  indexPA_Word8#, extractPA_Word8#, bpermutePA_Word8#,
  appPA_Word8#, applPA_Word8#,
  packPA_Word8#, pack'PA_Word8#, combine2PA_Word8#, combine2'PA_Word8#,
  fromListPA_Word8#,
  unsafe_zipWithPA_Word8#, unsafe_foldPA_Word8#, unsafe_fold1PA_Word8#,
  unsafe_foldPAs_Word8#,

  PArray_Double#,
  lengthPA_Double#, emptyPA_Double#,
  replicatePA_Double#, replicatelPA_Double#, repeatPA_Double#,
  indexPA_Double#, extractPA_Double#, bpermutePA_Double#,
  appPA_Double#, applPA_Double#,
  packPA_Double#, pack'PA_Double#, combine2PA_Double#, combine2'PA_Double#,
  fromListPA_Double#,
  unsafe_zipWithPA_Double#, unsafe_foldPA_Double#, unsafe_fold1PA_Double#,
  unsafe_foldPAs_Double#,

  PArray_Bool#,
  lengthPA_Bool#, replicatelPA_Bool#,
  packPA_Bool#, truesPA_Bool#, truesPAs_Bool#,

  fromBoolPA#, toBoolPA#
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base (
  Tag, fromBool, toBool, intToTag, tagToInt )

import GHC.Exts ( Int#, Int(..), Word#,
                  Double#, Double(..) )
import GHC.Word ( Word8(..) )

type Segd = U.Segd
type Sel2 = U.Sel2

elementsSegd# :: Segd -> Int#
elementsSegd# segd = case U.elementsSegd segd of { I# n# -> n# }
{-# INLINE_PA elementsSegd# #-}

mkSegd# :: U.Array Int -> U.Array Int -> Int# -> Segd
mkSegd# ns is n# = U.mkSegd ns is (I# n#)
{-# INLINE_PA mkSegd# #-}

{-# RULES

"mkSegd#" forall ns is n#.
  mkSegd# ns is n# = U.mkSegd ns is (I# n#)

  #-}

replicateSel2# :: Int# -> Int# -> Sel2
{-# INLINE replicateSel2# #-}
replicateSel2# n# tag# = U.mkSel2 (U.replicate n (intToTag tag))
                                  (U.enumFromStepLen 0 1 n)
                                  (if tag == 0 then n else 0)
                                  (if tag == 0 then 0 else n)
                                  (U.mkSelRep2 (U.replicate n (intToTag tag)))
  where
    n = I# n#
    tag = I# tag#

-- unused
-- pickSel2# :: Sel2 -> Int# -> U.Array Bool
-- {-# INLINE pickSel2# #-}
-- pickSel2# sel tag# = U.pick (U.tagsSel2 sel) (intToTag (I# tag#))

tagsSel2 :: Sel2 -> U.Array Tag
{-# INLINE tagsSel2 #-}
tagsSel2 = U.tagsSel2

elementsSel2_0# :: Sel2 -> Int#
elementsSel2_0# sel = case U.elementsSel2_0 sel of { I# n# -> n# }
{-# INLINE_PA elementsSel2_0# #-}

elementsSel2_1# :: Sel2 -> Int#
elementsSel2_1# sel = case U.elementsSel2_1 sel of { I# n# -> n# }
{-# INLINE_PA elementsSel2_1# #-}

type PArray_Int# = U.Array Int

lengthPA_Int# :: PArray_Int# -> Int#
lengthPA_Int# arr = case U.length arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Int# #-}

emptyPA_Int# :: PArray_Int#
emptyPA_Int# = U.empty
{-# INLINE_PA emptyPA_Int# #-}

replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)
{-# INLINE_PA replicatePA_Int# #-}

{-# RULES

"replicatePA_Int#" forall n# i#.
  replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)

 #-}

replicatelPA_Int# :: Segd -> PArray_Int# -> PArray_Int#
replicatelPA_Int# segd is = U.replicate_s segd is
{-# INLINE_PA replicatelPA_Int# #-}

{-# RULES

"replicatelPA_Int#" forall segd is.
  replicatelPA_Int# segd is = U.replicate_s segd is

 #-}

repeatPA_Int# :: Int# -> Int# -> PArray_Int# -> PArray_Int#
repeatPA_Int# n# len# is = U.repeat (I# n#) (I# len#) is
{-# INLINE_PA repeatPA_Int# #-}

indexPA_Int# :: PArray_Int# -> Int# -> Int#
indexPA_Int# ns i#      = case U.index "indexPA_Int#" ns (I# i#) of { I# n# -> n# }
{-# INLINE_PA indexPA_Int# #-}

extractPA_Int# :: PArray_Int# -> Int# -> Int# -> PArray_Int#
extractPA_Int# xs i# n# = U.extract xs (I# i#) (I# n#)
{-# INLINE_PA extractPA_Int# #-}

bpermutePA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
bpermutePA_Int# ns is = U.bpermute ns is
{-# INLINE_PA bpermutePA_Int# #-}

appPA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
appPA_Int# ms ns = ms U.+:+ ns
{-# INLINE_PA appPA_Int# #-}

applPA_Int# :: Segd -> Segd -> PArray_Int# -> Segd -> PArray_Int# -> PArray_Int#
applPA_Int# segd is xs js ys = U.append_s segd is xs js ys
{-# INLINE_PA applPA_Int# #-}

pack'PA_Int# :: PArray_Int# -> PArray_Bool# -> PArray_Int#
pack'PA_Int# ns bs = U.pack ns bs
{-# INLINE_PA pack'PA_Int# #-}

packPA_Int# :: PArray_Int# -> Int# -> PArray_Bool# -> PArray_Int#
packPA_Int# ns _ bs = pack'PA_Int# ns bs
{-# INLINE_PA packPA_Int# #-}

combine2'PA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2'PA_Int# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Int# #-}

combine2PA_Int# :: Int# -> PArray_Int# -> PArray_Int#
                -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2PA_Int# _ sel _ xs ys = combine2'PA_Int# sel xs ys
{-# INLINE_PA combine2PA_Int# #-}

fromListPA_Int# :: Int# -> [Int] -> PArray_Int#
fromListPA_Int# _ xs = U.fromList xs
{-# INLINE_PA fromListPA_Int# #-}

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = U.enumFromTo 0 (I# n# - 1)
{-# INLINE_PA upToPA_Int# #-}

enumFromToPA_Int# :: Int# -> Int# -> PArray_Int#
enumFromToPA_Int# m# n# = U.enumFromTo (I# m#) (I# n#)
{-# INLINE_PA enumFromToPA_Int# #-}

enumFromThenToPA_Int# :: Int# -> Int# -> Int# -> PArray_Int#
enumFromThenToPA_Int# k# m# n# = U.enumFromThenTo (I# k#) (I# m#) (I# n#)
{-# INLINE_PA enumFromThenToPA_Int# #-}

enumFromStepLenPA_Int# :: Int# -> Int# -> Int# -> PArray_Int#
enumFromStepLenPA_Int# k# m# n# = U.enumFromStepLen (I# k#) (I# m#) (I# n#)
{-# INLINE_PA enumFromStepLenPA_Int# #-}

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# ns i# = U.map (\n -> n == I# i#) ns
{-# INLINE_PA selectPA_Int# #-}


selectorToIndices2PA# :: PArray_Int# -> PArray_Int#
selectorToIndices2PA# sel
  = U.zipWith pick sel
  . U.scan index (0,0)
  $ U.map init' sel
  where
    init' 0 = (1,0)
    init' _ = (0,1)

    index (i1,j1) (i2,j2) = (i1+i2,j1+j2)

    pick 0 (i,_) = i
    pick _ (_,j) = j
{-# INLINE_PA selectorToIndices2PA# #-}

sumPA_Int# :: PArray_Int# -> Int#
sumPA_Int# ns = case U.sum ns of I# n# -> n#
{-# INLINE_PA sumPA_Int# #-}

sumPAs_Int# :: Segd -> PArray_Int# -> PArray_Int#
sumPAs_Int# segd ds = U.sum_s segd ds
{-# INLINE_PA sumPAs_Int# #-}

unsafe_mapPA_Int# :: (Int -> Int) -> PArray_Int# -> PArray_Int#
unsafe_mapPA_Int# f ns = U.map f ns
{-# INLINE_PA unsafe_mapPA_Int# #-}

unsafe_zipWithPA_Int# :: (Int -> Int -> Int)
                      -> PArray_Int# -> PArray_Int# -> PArray_Int#
unsafe_zipWithPA_Int# f ms ns = U.zipWith f ms ns
{-# INLINE_PA unsafe_zipWithPA_Int# #-}

unsafe_foldPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> Int
unsafe_foldPA_Int# f z ns = U.fold f z ns
{-# INLINE_PA unsafe_foldPA_Int# #-}

unsafe_scanPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> PArray_Int#
unsafe_scanPA_Int# f z ns = U.scan f z ns
{-# INLINE_PA unsafe_scanPA_Int# #-}

type PArray_Word8# = U.Array Word8

lengthPA_Word8# :: PArray_Word8# -> Int#
lengthPA_Word8# arr = case U.length arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Word8# #-}

emptyPA_Word8# :: PArray_Word8#
emptyPA_Word8# = U.empty
{-# INLINE_PA emptyPA_Word8# #-}

replicatePA_Word8# :: Int# -> Word# -> PArray_Word8#
replicatePA_Word8# n# d# = U.replicate (I# n#) (W8# d#)
{-# INLINE_PA replicatePA_Word8# #-}

replicatelPA_Word8# :: Segd -> PArray_Word8# -> PArray_Word8#
replicatelPA_Word8# segd ds = U.replicate_s segd ds
{-# INLINE_PA replicatelPA_Word8# #-}

repeatPA_Word8# :: Int# -> Int# -> PArray_Word8# -> PArray_Word8#
repeatPA_Word8# n# len# ds = U.repeat (I# n#) (I# len#) ds
{-# INLINE_PA repeatPA_Word8# #-}

indexPA_Word8# :: PArray_Word8# -> Int# -> Word#
indexPA_Word8# ds i# = case U.index "indexPA_Word8#" ds (I# i#) of { W8# d# -> d# }
{-# INLINE_PA indexPA_Word8# #-}

extractPA_Word8# :: PArray_Word8# -> Int# -> Int# -> PArray_Word8#
extractPA_Word8# xs i# n# = U.extract xs (I# i#) (I# n#)
{-# INLINE_PA extractPA_Word8# #-}

bpermutePA_Word8# :: PArray_Word8# -> PArray_Int# -> PArray_Word8#
bpermutePA_Word8# ds is = U.bpermute ds is
{-# INLINE_PA bpermutePA_Word8# #-}

appPA_Word8# :: PArray_Word8# -> PArray_Word8# -> PArray_Word8#
appPA_Word8# ms ns = ms U.+:+ ns
{-# INLINE_PA appPA_Word8# #-}

applPA_Word8# :: Segd -> Segd -> PArray_Word8# -> Segd -> PArray_Word8#
               -> PArray_Word8#
applPA_Word8# segd is xs js ys = U.append_s segd is xs js ys
{-# INLINE_PA applPA_Word8# #-}

pack'PA_Word8# :: PArray_Word8# -> PArray_Bool# -> PArray_Word8#
pack'PA_Word8# ns bs = U.pack ns bs
{-# INLINE_PA pack'PA_Word8# #-}

packPA_Word8# :: PArray_Word8# -> Int# -> PArray_Bool# -> PArray_Word8#
packPA_Word8# ns _ bs = pack'PA_Word8# ns bs
{-# INLINE_PA packPA_Word8# #-}

combine2'PA_Word8# :: PArray_Int#
                    -> PArray_Word8# -> PArray_Word8# -> PArray_Word8#
combine2'PA_Word8# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Word8# #-}

combine2PA_Word8# :: Int# -> PArray_Int# -> PArray_Int#
                   -> PArray_Word8# -> PArray_Word8# -> PArray_Word8#
combine2PA_Word8# _ sel _ xs ys = combine2'PA_Word8# sel xs ys
{-# INLINE_PA combine2PA_Word8# #-}

fromListPA_Word8# :: Int# -> [Word8] -> PArray_Word8#
fromListPA_Word8# _ xs = U.fromList xs
{-# INLINE_PA fromListPA_Word8# #-}

unsafe_zipWithPA_Word8# :: (Word8 -> Word8 -> Word8)
                         -> PArray_Word8# -> PArray_Word8# -> PArray_Word8#
unsafe_zipWithPA_Word8# f ms ns = U.zipWith f ms ns
{-# INLINE_PA unsafe_zipWithPA_Word8# #-}

unsafe_foldPA_Word8# :: (Word8 -> Word8 -> Word8)
                    -> Word8 -> PArray_Word8# -> Word8
unsafe_foldPA_Word8# f z ns = U.fold f z ns
{-# INLINE_PA unsafe_foldPA_Word8# #-}

unsafe_fold1PA_Word8#
  :: (Word8 -> Word8 -> Word8) -> PArray_Word8# -> Word8
unsafe_fold1PA_Word8# f ns = U.fold1 f ns
{-# INLINE_PA unsafe_fold1PA_Word8# #-}

unsafe_foldPAs_Word8# :: (Word8 -> Word8 -> Word8) -> Word8
                      -> Segd -> PArray_Word8# -> PArray_Word8#
unsafe_foldPAs_Word8# f z segd ds = U.fold_s f z segd ds
{-# INLINE_PA unsafe_foldPAs_Word8# #-}

type PArray_Double# = U.Array Double

lengthPA_Double# :: PArray_Double# -> Int#
lengthPA_Double# arr = case U.length arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Double# #-}

emptyPA_Double# :: PArray_Double#
emptyPA_Double# = U.empty
{-# INLINE_PA emptyPA_Double# #-}

replicatePA_Double# :: Int# -> Double# -> PArray_Double#
replicatePA_Double# n# d# = U.replicate (I# n#) (D# d#)
{-# INLINE_PA replicatePA_Double# #-}

replicatelPA_Double# :: Segd -> PArray_Double# -> PArray_Double#
replicatelPA_Double# segd ds = U.replicate_s segd ds
{-# INLINE_PA replicatelPA_Double# #-}

repeatPA_Double# :: Int# -> Int# -> PArray_Double# -> PArray_Double#
repeatPA_Double# n# len# ds = U.repeat (I# n#) (I# len#) ds
{-# INLINE_PA repeatPA_Double# #-}

{-# RULES

"repeatPA_Double#" forall n# len# ds.
  repeatPA_Double# n# len# ds = U.repeat (I# n#) (I# len#) ds

 #-}

indexPA_Double# :: PArray_Double# -> Int# -> Double#
indexPA_Double# ds i# = case U.index "indexPA_Double#" ds (I# i#) of { D# d# -> d# }
{-# INLINE_PA indexPA_Double# #-}

extractPA_Double# :: PArray_Double# -> Int# -> Int# -> PArray_Double#
extractPA_Double# xs i# n# = U.extract xs (I# i#) (I# n#)
{-# INLINE_PA extractPA_Double# #-}

bpermutePA_Double# :: PArray_Double# -> PArray_Int# -> PArray_Double#
bpermutePA_Double# ds is = U.bpermute ds is
{-# INLINE_PA bpermutePA_Double# #-}

appPA_Double# :: PArray_Double# -> PArray_Double# -> PArray_Double#
appPA_Double# ms ns = ms U.+:+ ns
{-# INLINE_PA appPA_Double# #-}

applPA_Double# :: Segd -> Segd -> PArray_Double# -> Segd -> PArray_Double#
               -> PArray_Double#
applPA_Double# segd is xs js ys = U.append_s segd is xs js ys
{-# INLINE_PA applPA_Double# #-}

pack'PA_Double# :: PArray_Double# -> PArray_Bool# -> PArray_Double#
pack'PA_Double# ns bs = U.pack ns bs
{-# INLINE_PA pack'PA_Double# #-}

packPA_Double# :: PArray_Double# -> Int# -> PArray_Bool# -> PArray_Double#
packPA_Double# ns _ bs = pack'PA_Double# ns bs
{-# INLINE_PA packPA_Double# #-}

combine2'PA_Double# :: PArray_Int#
                    -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2'PA_Double# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Double# #-}

combine2PA_Double# :: Int# -> PArray_Int# -> PArray_Int#
                   -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2PA_Double# _ sel _ xs ys = combine2'PA_Double# sel xs ys
{-# INLINE_PA combine2PA_Double# #-}

fromListPA_Double# :: Int# -> [Double] -> PArray_Double#
fromListPA_Double# _ xs = U.fromList xs
{-# INLINE_PA fromListPA_Double# #-}

unsafe_zipWithPA_Double# :: (Double -> Double -> Double)
                         -> PArray_Double# -> PArray_Double# -> PArray_Double#
unsafe_zipWithPA_Double# f ms ns = U.zipWith f ms ns
{-# INLINE_PA unsafe_zipWithPA_Double# #-}

unsafe_foldPA_Double# :: (Double -> Double -> Double)
                    -> Double -> PArray_Double# -> Double
unsafe_foldPA_Double# f z ns = U.fold f z ns
{-# INLINE_PA unsafe_foldPA_Double# #-}

unsafe_fold1PA_Double#
  :: (Double -> Double -> Double) -> PArray_Double# -> Double
unsafe_fold1PA_Double# f ns = U.fold1 f ns
{-# INLINE_PA unsafe_fold1PA_Double# #-}

unsafe_foldPAs_Double# :: (Double -> Double -> Double) -> Double
                       -> Segd -> PArray_Double# -> PArray_Double#
unsafe_foldPAs_Double# f z segd ds = U.fold_s f z segd ds
{-# INLINE_PA unsafe_foldPAs_Double# #-}
               
type PArray_Bool# = U.Array Bool

lengthPA_Bool# :: PArray_Bool# -> Int#
lengthPA_Bool# arr = case U.length arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Bool# #-}

replicatelPA_Bool# :: Segd -> PArray_Bool# -> PArray_Bool#
replicatelPA_Bool# segd ds = U.replicate_s segd ds
{-# INLINE_PA replicatelPA_Bool# #-}

packPA_Bool# :: PArray_Bool# -> Int# -> PArray_Bool# -> PArray_Bool#
packPA_Bool# ns _ bs = U.pack ns bs
{-# INLINE_PA packPA_Bool# #-}

truesPA_Bool# :: PArray_Bool# -> Int#
truesPA_Bool# bs = sumPA_Int# (fromBoolPA# bs)
{-# INLINE_PA truesPA_Bool# #-}

truesPAs_Bool# :: Segd -> PArray_Bool# -> PArray_Int#
truesPAs_Bool# segd = sumPAs_Int# segd . fromBoolPA#
{-# INLINE truesPAs_Bool# #-}

fromBoolPA# :: PArray_Bool# -> PArray_Int#
fromBoolPA# = U.map (tagToInt . fromBool)
{-# INLINE_PA fromBoolPA# #-}

toBoolPA# :: PArray_Int# -> PArray_Bool#
toBoolPA# = U.map (toBool . intToTag)
{-# INLINE_PA toBoolPA# #-}

