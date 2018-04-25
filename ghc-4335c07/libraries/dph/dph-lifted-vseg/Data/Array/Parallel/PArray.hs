{-# OPTIONS -fno-spec-constr #-}
{-# LANGUAGE CPP, UndecidableInstances #-}
#include "fusion-phases.h"

-- | Unvectorised parallel arrays.
--
--   * These operators may be used directly by unvectorised client programs.
--
--   * They are also used by the "Data.Array.Parallel.Lifted.Combinators"
--     module to define the closure converted versions that vectorised code
--     uses.
--
--   * In general, the operators here are all unsafe and don't do bounds checks.
--     The lifted versions also don't check that each of the argument arrays
--     have the same length.

--   TODO:
--   Export unsafe versions from Data.Array.Parallel.PArray.Unsafe, and ensure
--   this module exports safe wrappers. We want to use the unsafe versions in
--   D.A.P.Lifted.Combinators for performance reasons, but the user facing PArray
--   functions should all be safe. In particular, the vectoriser guarantees
--   that all arrays passed to lifted functions will have the same length, but
--   the user may not obey this restriction.
--
module Data.Array.Parallel.PArray
        ( PArray, PA
        , valid
        , nf
        , typeRep

        -- * Constructors
        , empty
        , singleton,    singletonl
        , replicate,    replicatel,     replicates,     replicates'
        , append,       appendl
        , concat,       concatl
        , unconcat
        , nestUSegd

        -- * Projections
        , length,       lengthl         -- length from D.A.P.PArray.PData.Base
        , index,        indexl
        , extract,      extracts,       extracts'
        , slice,        slicel
        , takeUSegd

        -- * Pack and Combine
        , pack,         packl
        , packByTag
        , combine2

        -- * Enumerations
        , enumFromTo,   enumFromTol     -- from D.A.P.PArray.Scalar

        -- * Tuples
        , zip,          zipl
        , zip3
        , zip4
        , zip5
        , unzip,        unzipl
        , unzip3
        , unzip4
        , unzip5

        -- * Conversions
        , fromVector,   toVector
        , fromList,     toList
        , fromUArray,   toUArray        -- from D.A.P.PArray.Scalar
	, fromUArray2)                  -- from D.A.P.PArray.Scalar
where
import Data.Array.Parallel.Trace
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import Data.Array.Parallel.PArray.Reference.Convert
import GHC.Exts (Int(I#), (==#), (+#))
import GHC.Base ( isTrue# )
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Pretty			as T
import qualified Data.Array.Parallel.Array			as A
import qualified Data.Array.Parallel.Unlifted			as U
import qualified Data.Vector					as V
import qualified Data.Array.Parallel.PArray.Reference		as R
import qualified Data.Array.Parallel.PArray.Reference.Convert	as R
import Data.Typeable hiding ( typeRep )

import qualified Prelude					as P
import Prelude hiding
        ( length, replicate, concat
        , enumFromTo
        , zip, zip3, unzip, unzip3)


-- Pretty ---------------------------------------------------------------------
instance PA a => T.PprPhysical (PArray a) where
 pprp (PArray n# pdata)
        =     ( T.text "PArray " T.<+> T.int (I# n#))
        T.$+$ ( T.nest 4
              $ pprpDataPA pdata)

instance PA a => Similar a where
 similar        = similarPA

instance PA a => R.PprPhysical1 a where
 pprp1          = pprpPA


-- Array -----------------------------------------------------------------------
--  Generic interface to PArrays.
--
--  NOTE:
--  The toVector conversion is defined by looking up every index instead of
--  using the bulk fromVectorPA function.
--  We do this to convert arrays of type (PArray Void) properly, as although a
--  (PArray Void) has an intrinsic length, a (PData Void) does not. If we try
--  to se the fromVectorPA function at this type we'll just get an `error`.
--  Arrays of type PArray Void aren't visible in the user API, but during
--  debugging we need to be able to print them out with the implied length.
--
instance PA e => A.Array PArray e where
 valid          = valid
 singleton      = singleton
 append         = append

 length         = length
 index (PArray _ pdata) ix
        = indexPA pdata ix

 toVector arr   = V.map (A.index arr) $ V.enumFromTo 0 (A.length arr - 1)
 fromVector     = fromVector

-- Operators ==================================================================
-- Each of these operators is wrapped in withRef functions so that we can
-- compare their outputs to the reference implementation.
-- See D.A.P.Reference for details.


-- Basics ---------------------------------------------------------------------
instance (Eq a, PA a)  => Eq (PArray a) where
 (==) (PArray _ xs) (PArray _ ys) = toVectorPA xs == toVectorPA ys
 (/=) (PArray _ xs) (PArray _ ys) = toVectorPA xs /= toVectorPA ys


-- | Check that an array has a valid internal representation.
valid :: PA a => PArray a -> Bool
valid (PArray n# darr1)
 = validPA  darr1
 && coversPA True darr1 (I# n#)
{-# INLINE_PA valid #-}


-- | Force an array to normal form.
nf :: PA a => PArray a -> ()
nf (PArray _ d)
 = nfPA d
{-# INLINE_PA nf #-}


-- | Get the type of a thing.
typeRep :: PA a => a -> TypeRep
typeRep x = typeRepPA x


-- Empty ---------------------------------------------------------------------
-- | O(1). An empty array.
empty :: PA a => PArray a
empty
 = withRef1 "empty" R.empty
 $ PArray 0# emptyPA
{-# INLINE_PA empty #-}


-- Singleton ------------------------------------------------------------------
-- | O(1). Produce an array containing a single element.
singleton :: PA a => a -> PArray a
singleton x
 = traceOp (OpSingleton (typeRepPA x))
 $ withRef1 "singleton" (R.singleton x)
 $ PArray 1# (replicatePA 1 x)
{-# INLINE_PA singleton #-}


-- | O(n). Produce an array of singleton arrays.
singletonl :: PA a => PArray a -> PArray (PArray a)
singletonl arr@(PArray n# pdata)
 = traceOp (OpSingletonL (typeRepDataPA pdata) (I# n#))
 $ withRef2 "singletonl" (R.singletonl (toRef1 arr))
 $ replicatel_ (replicate_ (length arr) 1) arr
{-# INLINE_PA singletonl #-}


-- Replicate ------------------------------------------------------------------
-- | O(n). Define an array of the given size, that maps all elements to the same value.
--   We require the replication count to be > 0 so that it's easier to maintain
--   the validPR invariants for nested arrays.
replicate :: PA a => Int -> a -> PArray a
replicate n x
 = traceOp (OpReplicate (typeRepPA x) n)
 $ withRef1 "replicate" (R.replicate n x)
 $ replicate_ n x
{-# INLINE_PA replicate #-}

replicate_ :: PA a => Int -> a -> PArray a
replicate_ (I# n#) x
 = PArray n# (replicatePA (I# n#) x)
{-# INLINE_PA replicate_ #-}


-- | O(sum lengths). Lifted replicate.
replicatel :: PA a => PArray Int -> PArray a -> PArray (PArray a)
replicatel reps arr@(PArray n# pdata)
 = traceOp (OpReplicateL (typeRepDataPA pdata)
                         (I# n#))
 $ withRef2 "replicatel" (R.replicatel (toRef1 reps) (toRef1 arr))
 $ replicatel_ reps arr

replicatel_ :: PA a => PArray Int -> PArray a -> PArray (PArray a)
replicatel_ (PArray n# (PInt lens)) (PArray _ pdata)
 = if isTrue# (n# ==# 0#) then empty else
    let !segd    = U.lengthsToSegd lens
        !vsegd   = U.promoteSegdToVSegd segd
        !pdata'  = replicatesPA segd pdata
        !pdatas' = singletondPA pdata'
     in PArray n# $ mkPNestedPA vsegd pdatas' segd pdata'
{-# INLINE_PA replicatel_ #-}


-- | O(sum lengths). Segmented replicate.
replicates :: PA a => U.Segd -> PArray a -> PArray a
replicates segd arr@(PArray _ pdata)
 = traceOp (OpReplicateS (typeRepDataPA pdata)
                         (U.elementsSegd segd))
 $ withRef1 "replicates" (R.replicates segd (toRef1 arr))
 $ let  !(I# n#) = U.elementsSegd segd
   in   PArray n# $ replicatesPA segd pdata
{-# INLINE_PA replicates #-}


-- | O(sum lengths). Wrapper for segmented replicate that takes replication counts
--  and uses them to build the `U.Segd`.
replicates' :: PA a => PArray Int -> PArray a -> PArray a
replicates' (PArray _ (PInt reps)) arr
 = replicates (U.lengthsToSegd reps) arr
{-# INLINE_PA replicates' #-}


-- Append ---------------------------------------------------------------------
-- | Append two arrays.
append :: PA a => PArray a -> PArray a -> PArray a
append arr1@(PArray n1# pdata1) arr2@(PArray n2# pdata2)
 = traceOp (OpAppend    (typeRepDataPA pdata1)
                        (I# n1#) (I# n2#) (I# (n1# +# n2#)))
 $ withRef1 "append"    (R.append (toRef1 arr1) (toRef1 arr2))
 $ PArray (n1# +# n2#)  (appendPA pdata1 pdata2)
{-# INLINE_PA append #-}


-- | Lifted append.
--   Both arrays must have the same length
appendl :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl arr1@(PArray n# pdata1) arr2@(PArray _ pdata2)
 = traceOp (OpAppendL   (typeRepDataPA pdata1)
                        (I# n#))
 $ withRef2 "appendl"   (R.appendl (toRef2 arr1) (toRef2 arr2))
 $ PArray n# $ appendlPA pdata1 pdata2
{-# INLINE_PA appendl #-}


-- Concat ---------------------------------------------------------------------
-- | Concatenate a nested array.
concat :: PA a => PArray (PArray a) -> PArray a
concat arr@(PArray n# pdata)
 = let  pdata'          = concatPA pdata
        !(I# n2#)       = lengthPA pdata'
   in   traceOp (OpConcat    (typeRepDataPA pdata)
                             (I# n#) (I# n2#))
      $ withRef1 "concat"    (R.concat (toRef2 arr))
      $ PArray  n2# pdata'
{-# INLINE_PA concat #-}


-- | Lifted concat.
concatl :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatl arr@(PArray n# pdata)
 = traceOp (OpConcatL   (typeRepDataPA pdata)
                        (I# n#))
 $ withRef2 "concatl"   (R.concatl (toRef3 arr))
 $ PArray n# $ concatlPA pdata
{-# INLINE_PA concatl #-}


-- | Impose a nesting structure on a flat array
unconcat :: (PA a, PA b) => PArray (PArray a) -> PArray b -> PArray (PArray b)
unconcat (PArray n# pdata1) (PArray _ pdata2)
 = traceOp (OpUnconcat  (typeRepDataPA pdata1) (I# n#))
 $ PArray n# $ unconcatPA pdata1 pdata2
{-# INLINE_PA unconcat #-}


-- | Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
nestUSegd :: PA a => U.Segd -> PArray a -> PArray (PArray a)
nestUSegd segd (PArray n# pdata)
        | U.elementsSegd segd     == I# n#
        , I# n2#                <- U.lengthSegd segd
        = PArray n2#
	$ PNested (U.promoteSegdToVSegd segd) (singletondPA pdata) segd pdata

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestUSegd: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ show (U.elementsSegd segd)
                , " length of data array        = " ++ show (I# n#) ]
{-# INLINE_PA nestUSegd #-}


-- Projections  ---------------------------------------------------------------
-- | Take the length of some arrays.
lengthl :: PA a => PArray (PArray a) -> PArray Int
lengthl arr@(PArray n# (PNested vsegd _ _ _))
 = traceOp (OpLengthL (I# n#))
 $ withRef1 "lengthl" (R.lengthl (toRef2 arr))
 $ PArray n# $ PInt $ U.takeLengthsOfVSegd vsegd
{-# INLINE_PA lengthl #-}


-- | O(1). Lookup a single element from the source array.
index    :: PA a => PArray a -> Int -> a
index (PArray _ arr) ix
 = traceOp (OpIndex)
 $ indexPA arr ix
{-# INLINE_PA index #-}


-- | O(len indices). Lookup a several elements from several source arrays
indexl    :: PA a => PArray (PArray a) -> PArray Int -> PArray a
indexl (PArray n# darr) (PArray _ ixs)
 = traceOp (OpIndexL (I# n#))
 $ PArray n# (indexlPA darr ixs)
{-# INLINE_PA indexl #-}


-- | Extract a range of elements from an array.
extract  :: PA a => PArray a -> Int -> Int -> PArray a
extract (PArray _ arr) start len@(I# len#)
 = traceOp (OpExtract (I# len#))
 $ PArray len# (extractPA arr start len)
{-# INLINE_PA extract #-}


-- | Segmented extract.
extracts :: PA a => Vector (PArray a) -> U.SSegd -> PArray a
extracts arrs ssegd
 = traceOp (OpExtractS (U.sum $ U.lengthsOfSSegd ssegd))
 $ let  pdatas          = fromVectordPA $ V.map (\(PArray _ vec) -> vec) arrs
        !(I# n#)        = (U.sum $ U.lengthsOfSSegd ssegd)
   in   PArray   n#
                (extractssPA pdatas ssegd)
{-# INLINE_PA extracts #-}


-- | Wrapper for `extracts` that takes arrays of sources, starts and lengths of
--   the segments, and uses these to build the `U.SSegd`.
--   TODO: The lengths of the sources, starts and lengths arrays must be the same,
--         but this is not checked.
--         All sourceids must point to valid data arrays.
--         Segments must be within their corresponding source array.
extracts'
        :: PA a
        => Vector (PArray a)
        -> PArray Int           -- ^ id of source array for each segment.
        -> PArray Int           -- ^ starting index of each segment in its source array.
        -> PArray Int           -- ^ length of each segment.
        -> PArray a
extracts' arrs (PArray _ (PInt sources)) (PArray _ (PInt starts)) (PArray _ (PInt lengths))
 = let segd    = U.lengthsToSegd lengths
       ssegd   = U.mkSSegd starts sources segd
   in  extracts arrs ssegd
{-# INLINE_PA extracts' #-}


-- | Extract a range of elements from an arrary.
--   Like `extract` but with the parameters in a different order.
slice :: PA a => Int -> Int -> PArray a -> PArray a
slice start len@(I# len#) (PArray _ darr)
 = traceOp (OpSlice len)
 $ PArray len# (extractPA darr start len)
{-# INLINE_PA slice #-}


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
slicel :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicel (PArray n# sliceStarts) (PArray _ sliceLens) (PArray _ darr)
 = traceOp (OpSliceL (I# n#))
 $ PArray n# (slicelPA sliceStarts sliceLens darr)
{-# INLINE_PA slicel #-}


-- | Take the segment descriptor from a nested array and demote it to a
--   plain Segd. This is unsafe because it can cause index space overflow.
takeUSegd :: PArray (PArray a) -> U.Segd
takeUSegd (PArray _ pdata)
 = takeSegdPD pdata
{-# INLINE_PA takeUSegd #-}


-- Pack and Combine -----------------------------------------------------------
-- | Select the elements of an array that have their tag set to True.
pack :: PA a => PArray a -> PArray Bool -> PArray a
pack arr@(PArray _ xs) flags@(PArray len# (PBool sel2))
 = traceOp (OpPack (I# len#))
 $ withRef1 "pack" (R.pack (toRef1 arr) (toRef1 flags))
 $ let  darr'           = packByTagPA xs (U.tagsSel2 sel2) 1

        -- The selector knows how many elements are set to '1',
        -- so we can use this for the length of the resulting array.
        !(I# m#)        = U.elementsSel2_1 sel2

    in  PArray m# darr'
{-# INLINE_PA pack #-}


-- | Lifted pack.
packl :: PA a => PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
packl xss@(PArray n# xdata@(PNested _ _ segd _))
      fss@(PArray _  fdata)
 = traceOp (OpPackL (I# n#))
 $ withRef2 "packl" (R.packl (toRef2 xss) (toRef2 fss))
 $ let
        -- Concatenate both arrays to get the flat data.
        --   Although the virtual segmentation should be the same,
        --   the physical segmentation of both arrays may be different.
        xdata_flat      = concatPA xdata
        PBool sel       = concatPA fdata
        tags            = U.tagsSel2 sel

        -- Count how many elements go into each segment.
        segd'           = U.lengthsToSegd $ U.count_s segd tags 1

        -- Build the result array
        vsegd'          = U.promoteSegdToVSegd segd'
        flat'           = packByTagPA xdata_flat tags 1
        pdatas'         = singletondPA flat'

   in   PArray n# (PNested vsegd' pdatas' segd' flat')
{-# INLINE_PA packl #-}


-- | Filter an array based on some tags.
packByTag :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTag arr@(PArray n# darr) tags tag
 = traceOp (OpPackByTag (I# n#))
 $ withRef1 "packByTag" (R.packByTag (toRef1 arr) tags tag)
 $ let  darr'           = packByTagPA darr tags tag
        !(I# n2#)        = lengthPA darr'
   in   PArray  n2# darr'

{-# INLINE_PA packByTag #-}


-- | Combine two arrays based on a selector.
combine2  :: forall a. PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 sel arr1@(PArray _ darr1) arr2@(PArray _ darr2)
 = traceOp (OpCombine2 (U.elementsSel2_0 sel + U.elementsSel2_1 sel))
 $ withRef1 "combine2" (R.combine2 sel (toRef1 arr1) (toRef1 arr2))
 $ let  darr'           = combine2PA sel darr1 darr2
        !(I# n#)        = lengthPA darr'
   in   PArray  n# darr'
{-# INLINE_PA combine2 #-}


-- Tuples ---------------------------------------------------------------------
-- | O(1). Zip a pair of arrays into an array of pairs.
--   The two arrays must have the same length, else `error`.
zip :: PArray a -> PArray b -> PArray (a, b)
zip (PArray n# pdata1) (PArray _ pdata2)
 = traceOp (OpZip (I# n#))
 $ PArray n# $ zipPD pdata1 pdata2
{-# INLINE_PA zip #-}


-- | Lifted zip.
zipl    :: (PA a, PA b)
        => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
zipl (PArray n# xs) (PArray _ ys)
 = traceOp (OpZipL (I# n#))
 $ PArray n# $ ziplPA xs ys
{-# INLINE_PA zipl #-}


-- | O(1). Zip three arrays.
--   All arrays must have the same length, else `error`.
zip3 :: PArray a -> PArray b -> PArray c -> PArray (a, b, c)
zip3 (PArray n# pdata1) (PArray _ pdata2) (PArray _ pdata3)
 = PArray n# $ zip3PD pdata1 pdata2 pdata3
{-# INLINE_PA zip3 #-}


-- | O(1). Zip four arrays.
--   All arrays must have the same length, else `error`.
zip4 :: PArray a -> PArray b -> PArray c -> PArray d -> PArray (a, b, c, d)
zip4 (PArray n# pdata1) (PArray _ pdata2) (PArray _ pdata3) (PArray _ pdata4)
 = PArray n# $ zip4PD pdata1 pdata2 pdata3 pdata4
{-# INLINE_PA zip4 #-}


-- | O(1). Zip five arrays.
--   All arrays must have the same length, else `error`.
zip5 :: PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray (a, b, c, d, e)
zip5 (PArray n# pdata1) (PArray _ pdata2) (PArray _ pdata3) (PArray _ pdata4) (PArray _ pdata5)
 = PArray n# $ zip5PD pdata1 pdata2 pdata3 pdata4 pdata5
{-# INLINE_PA zip5 #-}


-- | O(1). Unzip an array of pairs into a pair of arrays.
unzip :: PArray (a, b) -> (PArray a, PArray b)
unzip (PArray n# (PTuple2 xs ys))
 = (PArray n# xs, PArray n# ys)
{-# INLINE_PA unzip #-}

-- | O(1). Unzip an array of triples into a triple of arrays.
unzip3 :: PArray (a, b, c) -> (PArray a, PArray b, PArray c)
unzip3 (PArray n# (PTuple3 xs ys zs))
 = (PArray n# xs, PArray n# ys, PArray n# zs)
{-# INLINE_PA unzip3 #-}

-- | O(1). Unzip an array of triples into a triple of arrays.
unzip4 :: PArray (a, b, c, d) -> (PArray a, PArray b, PArray c, PArray d)
unzip4 (PArray n# (PTuple4 ws xs ys zs))
 = (PArray n# ws, PArray n# xs, PArray n# ys, PArray n# zs)
{-# INLINE_PA unzip4 #-}

-- | O(1). Unzip an array of triples into a triple of arrays.
unzip5 :: PArray (a, b, c, d, e) -> (PArray a, PArray b, PArray c, PArray d, PArray e)
unzip5 (PArray n# (PTuple5 vs ws xs ys zs))
 = (PArray n# vs, PArray n# ws, PArray n# xs, PArray n# ys, PArray n# zs)
{-# INLINE_PA unzip5 #-}


-- | Lifted unzip
unzipl :: PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
unzipl (PArray n# pdata)
 = traceOp (OpZipL (I# n#))
 $ PArray n# $ unziplPD pdata
{-# INLINE_PA unzipl #-}


-- Conversions ----------------------------------------------------------------
-- | Convert a `Vector` to a `PArray`
fromVector :: PA a => Vector a -> PArray a
fromVector vec
 = let !(I# n#) = V.length vec
   in  PArray n#  (fromVectorPA vec)
{-# INLINE_PA fromVector #-}


-- | Convert a `PArray` to a `Vector`
toVector   :: PA a => PArray a -> Vector a
toVector (PArray _ arr)
 = toVectorPA arr
{-# INLINE_PA toVector #-}


-- | Convert a list to a `PArray`.
fromList :: PA a => [a] -> PArray a
fromList xx
 = let  !(I# n#) = P.length xx
   in   PArray n# (fromVectorPA $ V.fromList xx)
{-# INLINE_PA fromList #-}


-- | Convert a `PArray` to a list.
toList   :: PA a => PArray a -> [a]
toList (PArray _ arr)
 = V.toList $ toVectorPA arr
{-# INLINE_PA toList #-}

