{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Scalar
where
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.PArray.PReprInstances
import Data.Array.Parallel.PArray.PDataInstances
import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base (fromBool, toBool)
import GHC.Exts (Int(..))


-- Pretend Bools are scalars --------------------------------------------------
instance Scalar Bool where
  {-# INLINE toScalarPData #-}
  toScalarPData bs
    = PBool (U.tagsToSel2 (U.map fromBool bs))

  {-# INLINE fromScalarPData #-}
  fromScalarPData (PBool sel) = U.map toBool (U.tagsSel2 sel)


-- Projections ----------------------------------------------------------------
prim_lengthPA :: Scalar a => PArray a -> Int
{-# INLINE prim_lengthPA #-}
prim_lengthPA xs = I# (lengthPA# xs)


-- Conversion -----------------------------------------------------------------
-- | Create a PArray out of a scalar U.Array, 
--   the first argument is the array length.
--
--   TODO: ditch this version, just use fromUArrPA'
--
fromUArray :: Scalar a => U.Array a -> PArray a
{-# INLINE fromUArray #-}
fromUArray xs
 = let  !(I# n#) = U.length xs
   in   PArray n# (toScalarPData xs)

-- TODO: Why do we want this version that takes the length explicitly?
--       Is there some fusion issue that requires this?
fromUArray' :: Scalar a => Int -> U.Array a -> PArray a
{-# INLINE fromUArray' #-}
fromUArray' (I# n#) xs
        = PArray n# (toScalarPData xs)


-- | Convert a PArray back to a plain U.Array.
toUArray :: Scalar a => PArray a -> U.Array a
{-# INLINE toUArray #-}
toUArray (PArray _ xs) = fromScalarPData xs


-- Tuple Conversions ----------------------------------------------------------
-- | Convert an U.Array of pairs to a PArray.
fromUArray2
        :: (Scalar a, Scalar b)
        => U.Array (a,b) -> PArray (a,b)
{-# INLINE fromUArray2 #-}
fromUArray2 ps
 = let  !(I# n#) = U.length ps
        (xs, ys) = U.unzip ps
   in   PArray n# (P_2 (toScalarPData xs) (toScalarPData  ys))


-- | Convert a U.Array of triples to a PArray.
fromUArray3
        :: (Scalar a, Scalar b, Scalar c)
        => U.Array ((a,b),c) -> PArray (a,b,c)
{-# INLINE fromUArray3 #-}
fromUArray3 ps
 = let  !(I# n#) = U.length ps
        (qs,zs) = U.unzip ps
        (xs,ys) = U.unzip qs
   in   PArray n# (P_3  (toScalarPData xs)
                        (toScalarPData ys)
                        (toScalarPData zs))


-- Nesting arrays -------------------------------------------------------------
-- | O(1). Create a nested array.
nestUSegd
        :: U.Segd               -- ^ segment descriptor
        -> PArray a             -- ^ array of data elements.
        -> PArray (PArray a)

{-# INLINE nestUSegd #-}
nestUSegd segd (PArray _ xs)
 = let  !(I# n#) = U.lengthSegd segd 
   in   PArray n# (PNested segd xs)


-- Scalar Operators -----------------------------------------------------------
-- These work on PArrays of scalar elements.
-- TODO: Why do we need these versions as well as the standard ones?

-- | Apply a worker function to every element of an array, yielding a new array.
scalar_map 
        :: (Scalar a, Scalar b) 
        => (a -> b) -> PArray a -> PArray b

{-# INLINE_PA scalar_map #-}
scalar_map f xs 
        = fromUArray' (prim_lengthPA xs)
        . U.map f
        $ toUArray xs


-- | Zip two arrays, yielding a new array.
scalar_zipWith
        :: (Scalar a, Scalar b, Scalar c)
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c

{-# INLINE_PA scalar_zipWith #-}
scalar_zipWith f xs ys
        = fromUArray' (prim_lengthPA xs)
        $ U.zipWith f (toUArray xs) (toUArray ys)


-- | Zip three arrays, yielding a new array.
scalar_zipWith3
        :: (Scalar a, Scalar b, Scalar c, Scalar d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d

{-# INLINE_PA scalar_zipWith3 #-}
scalar_zipWith3 f xs ys zs
        = fromUArray' (prim_lengthPA xs)
        $ U.zipWith3 f (toUArray xs) (toUArray ys) (toUArray zs)




-- | Zip four arrays, yielding a new array.
scalar_zipWith4
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e)
        => (a -> b -> c -> d -> e) -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e

{-# INLINE_PA scalar_zipWith4 #-}
scalar_zipWith4 f ws xs ys zs 
        = fromUArray' (prim_lengthPA ws)
        $ U.zipWith4 f (toUArray ws) (toUArray xs) (toUArray ys) (toUArray zs)


-- | Zip five arrays, yielding a new array.
scalar_zipWith5
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f)
        => (a -> b -> c -> d -> e -> f) -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f

{-# INLINE_PA scalar_zipWith5 #-}
scalar_zipWith5 f vs ws xs ys zs  
        = fromUArray' (prim_lengthPA vs)
        $ U.zipWith5 f (toUArray vs) (toUArray ws) (toUArray xs) (toUArray ys) (toUArray zs)
        
        
-- | Zip six arrays, yielding a new array.
scalar_zipWith6
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g)
        => (a -> b -> c -> d -> e -> f -> g) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray  f-> PArray g

{-# INLINE_PA scalar_zipWith6 #-}
scalar_zipWith6 f us vs ws xs ys zs 
        = fromUArray' (prim_lengthPA us)
        $ U.zipWith6 f (toUArray us) (toUArray vs) (toUArray ws) (toUArray xs) (toUArray ys) (toUArray zs)
        
-- | Zip seven arrays, yielding a new array.
scalar_zipWith7
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g, Scalar h)
        => (a -> b -> c -> d -> e -> f -> g -> h) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray  f-> PArray g -> PArray h

{-# INLINE_PA scalar_zipWith7 #-}
scalar_zipWith7 f ts us vs ws xs ys zs 
        = fromUArray' (prim_lengthPA us)
        $ U.zipWith7 f (toUArray ts) (toUArray us) (toUArray vs) (toUArray ws) (toUArray xs) (toUArray ys) (toUArray zs)
                        

-- | Zip eight arrays, yielding a new array.
scalar_zipWith8
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g, Scalar h, Scalar i)
        => (a -> b -> c -> d -> e -> f -> g -> h -> i) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray  f-> PArray g -> PArray h -> PArray i

{-# INLINE_PA scalar_zipWith8 #-}
scalar_zipWith8 f ss ts us vs ws xs ys zs 
        = fromUArray' (prim_lengthPA ss)
        $ U.zipWith8 f (toUArray ss)  (toUArray ts) (toUArray us) (toUArray vs) (toUArray ws) (toUArray xs) (toUArray ys) (toUArray zs)
                        
-- | Left fold over an array.
scalar_fold 
        :: Scalar a
        => (a -> a -> a) -> a -> PArray a -> a

{-# INLINE_PA scalar_fold #-}
scalar_fold f z
        = U.fold f z . toUArray


-- | Left fold over an array, using the first element to initialise the state.
scalar_fold1 
        :: Scalar a
        => (a -> a -> a) -> PArray a -> a

{-# INLINE_PA scalar_fold1 #-}
scalar_fold1 f
        = U.fold1 f . toUArray


-- | Segmented fold of an array of arrays.
--   Each segment is folded individually, yielding an array of the fold results.
scalar_folds 
        :: Scalar a
        => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a

{-# INLINE_PA scalar_folds #-}
scalar_folds f z xss
        = fromUArray' (prim_lengthPA (concatPA# xss))
        . U.fold_s f z (segdPA# xss)
        . toUArray
        $ concatPA# xss


-- | Segmented fold of an array of arrays, using the first element of each
--   segment to initialse the state for that segment.
--   Each segment is folded individually, yielding an array of all the fold results.
scalar_fold1s
        :: Scalar a
        => (a -> a -> a) -> PArray (PArray a) -> PArray a

{-# INLINE_PA scalar_fold1s #-}
scalar_fold1s f xss
        = fromUArray' (prim_lengthPA (concatPA# xss))
        . U.fold1_s f (segdPA# xss)
        . toUArray
         $ concatPA# xss


-- | Left fold over an array, also passing the index of each element
--   to the parameter function.
scalar_fold1Index
        :: Scalar a
        => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int

{-# INLINE_PA scalar_fold1Index #-}
scalar_fold1Index f
        = fst . U.fold1 f . U.indexed . toUArray


-- | Segmented fold over an array, also passing the index of each 
--   element to the parameter function.
scalar_fold1sIndex
        :: Scalar a
        => ((Int, a) -> (Int, a) -> (Int, a))
        -> PArray (PArray a) -> PArray Int

{-# INLINE_PA scalar_fold1sIndex #-}
scalar_fold1sIndex f (PArray m# (PNested segd xs))
        = PArray m#
        $ toScalarPData
        $ U.fsts
        $ U.fold1_s f segd
        $ U.zip (U.indices_s segd)
        $ fromScalarPData xs

