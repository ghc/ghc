{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | Functions that work on parallel arrays of scalar elements.
--   Unlike the functions defined in D.A.P.PArray, these only need
--   Scalar dictionaries, instead of PR or PA dictionaries. 
--
--   They are used when defining vectorised Prelude functions, 
--    eg in D.A.P.Prelude.Int and D.A.P.Prelude.Double.
--
--   The map and zipWith functions are also used by the vectoriser when
--   vectorising uses of scalar operators like (+).
--
module Data.Array.Parallel.PArray.Scalar 
        ( Scalar(..)

        -- * Conversions
        , toUArray,   fromUArray
        , fromUArray2

        -- * Maps and Zips
        , map
        , zipWith
        , zipWith3
        , zipWith4
        , zipWith5
        , zipWith6
        , zipWith7
        , zipWith8
        
        -- * Folds
        , fold,         folds
        , fold1,        fold1s
        , fold1Index,   fold1sIndex
        
        -- * Enumerations
        , enumFromTo, enumFromTol)
where
import Data.Array.Parallel.PArray.PData.Void
import Data.Array.Parallel.PArray.PData.Word8
import Data.Array.Parallel.PArray.PData.Double
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.Base
import Data.Word
import GHC.Exts
import qualified Data.Array.Parallel.Unlifted           as U
import Prelude hiding ( map, zipWith, zipWith3, enumFromTo)


-- | Class of Scalar data that can be converted to and from single unboxed
--   vectors.
class (PA a, U.Elt a) => Scalar a where
  fromScalarPData  :: PData  a    -> U.Array a
  toScalarPData    :: U.Array a   -> PData a
  
  fromScalarPDatas :: PDatas a    -> U.Arrays a
  toScalarPDatas   :: U.Arrays a  -> PDatas a


-- Shorthands for the above methods used in this module only.
from    :: Scalar a => PData a -> U.Array a
from    = fromScalarPData

to      :: Scalar a => U.Array a -> PData a
to      = toScalarPData


-- Instances --------------------------------------------------------------
instance Scalar Bool where
  {-# INLINE toScalarPData #-}
  toScalarPData bs
    = PBool (U.tagsToSel2 (U.map fromBool bs))

  {-# INLINE fromScalarPData #-}
  fromScalarPData (PBool sel)
    = U.map toBool (U.tagsSel2 sel)

  -- NOTE: There is no Arrays instance for Bool, 
  --       but we don't need it yet because the PDatas Sel2s instance
  --       just uses a boxed vector of Sel2s.
  {-# NOINLINE fromScalarPDatas #-}
  fromScalarPDatas _
    = error "Data.Array.Parallel.PArray.Lifted.Scalar: no Arrays instance for Bool."

  {-# NOINLINE toScalarPDatas #-}
  toScalarPDatas _
    = error "Data.Array.Parallel.PArray.Lifted.Scalar: no Arrays instance for Bool."

instance U.Elt Ordering

instance Scalar Ordering where
  {-# INLINE toScalarPData #-}
  toScalarPData
    = POrdering . U.map toPRepr

  {-# INLINE fromScalarPData #-}
  fromScalarPData (POrdering w8s)
    = U.map fromPRepr w8s

    -- FIXME: no idea whether these are used; should be possible to convert, though
  {-# INLINE toScalarPDatas #-}
  toScalarPDatas _
    = error "Data.Array.Parallel.PArray.Lifted.Scalar: no 'Arrays' instance for 'Ordering'."

  {-# INLINE fromScalarPDatas #-}
  fromScalarPDatas _
    = error "Data.Array.Parallel.PArray.Lifted.Scalar: no 'Arrays' instance for 'Ordering'."

-- FIXME: this is a fake instance to enable us to vectorise 'Num'
type instance PRepr  Integer = Void
data instance PData  Integer = PInteger
data instance PDatas Integer = PIntegers
instance PA Integer
instance U.Elt Integer
instance Scalar Integer where
  toScalarPData = fakeScalarInteger
  fromScalarPData = fakeScalarInteger
  toScalarPDatas = fakeScalarInteger
  fromScalarPDatas = fakeScalarInteger
fakeScalarInteger :: a
fakeScalarInteger = error "D.A.P.PArray.Scalar: fake instance 'Scalar Integer'"

-- See Note: Seqs in fromScalar
instance Scalar Int where
  fromScalarPData  (PInt  xs)     = xs  `seq` xs
  fromScalarPDatas (PInts xss)    = xss `seq` xss
  toScalarPData                   = PInt
  toScalarPDatas                  = PInts

instance Scalar Word8 where
  fromScalarPData  (PWord8  xs)   = xs  `seq` xs
  fromScalarPDatas (PWord8s xss)  = xss `seq` xss
  toScalarPData                   = PWord8
  toScalarPDatas                  = PWord8s

instance Scalar Double where
  fromScalarPData  (PDouble xs)   = xs  `seq` xs
  fromScalarPDatas (PDoubles xss) = xss `seq` xss
  toScalarPData                   = PDouble
  toScalarPDatas                  = PDoubles


-- [Note: Seqs in fromScalar]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As we expect the result of fromScalarPData to always be demanded by the 
-- consuming function, we seq on it to force the demand. This helps to avoid
-- fusion problems when GHC can't see that the consumer actually demands the
-- data. This shows up in SMVM where removing the `seq in the Doubles instance
-- prevents the fold_vs/promoteSegdToVSegd rule from firing.
        
-- Conversions ----------------------------------------------------------------
{-# INLINE_PA fromUArray #-}
fromUArray  :: Scalar a => U.Array a -> PArray a
fromUArray uarr
 = let  !(I# n#) = U.length uarr
   in   PArray n# (toScalarPData uarr) 
 
 
{-# INLINE_PA toUArray #-}
toUArray    :: Scalar a => PArray a -> U.Array a
toUArray (PArray _ pdata)
        = fromScalarPData pdata
 

-- Tuple Conversions ----------------------------------------------------------
-- | Convert an U.Array of pairs to a PArray.
{-# INLINE fromUArray2 #-}
fromUArray2
        :: (Scalar a, Scalar b)
        => U.Array (a, b) -> PArray (a, b)
fromUArray2 ps
 = let  !(I# n#) = U.length ps
        (xs,ys)  = U.unzip ps
    in  PArray n# (PTuple2 (toScalarPData xs) (toScalarPData  ys))
    

-- Maps and Zips --------------------------------------------------------------
-- | Apply a worker function to every element of an array, yielding a new array.
{-# INLINE_PA map #-}
map     :: (Scalar a, Scalar b) 
        => (a -> b) -> PArray a -> PArray b

map f (PArray len xs)
        = PArray len $ to $ U.map f (from xs)


-- | Zip two arrays, yielding a new array.
{-# INLINE_PA zipWith #-}
zipWith :: (Scalar a, Scalar b, Scalar c)
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c

zipWith f (PArray len xs) (PArray _ ys)
        = PArray len $ to $ U.zipWith f (from xs) (from ys)


-- | Zip three arrays, yielding a new array.
{-# INLINE_PA zipWith3 #-}
zipWith3
        :: (Scalar a, Scalar b, Scalar c, Scalar d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d

zipWith3 f (PArray len xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith3 f (from xs) (from ys) (from zs)

-- | Zip four arrays, yielding a new array.
{-# INLINE_PA zipWith4 #-}
zipWith4
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e)
        => (a -> b -> c -> d -> e) -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e

zipWith4 f (PArray len ws) (PArray _ xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith4 f (from ws) (from xs) (from ys) (from zs)

-- | Zip five arrays, yielding a new array.
{-# INLINE_PA zipWith5 #-}
zipWith5
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f)
        => (a -> b -> c -> d -> e -> f) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f

zipWith5 f (PArray len vs)  (PArray _ ws) (PArray _ xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith5 f (from vs) (from ws) (from xs) (from ys) (from zs)


-- | Zip six arrays, yielding a new array.
{-# INLINE_PA zipWith6 #-}
zipWith6
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g)
        => (a -> b -> c -> d -> e -> f -> g) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g

zipWith6 f (PArray len us)  (PArray _ vs)(PArray _ ws) (PArray _ xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith6 f (from us) (from vs) (from ws) (from xs) (from ys) (from zs)


-- | Zip seven arrays, yielding a new array.
{-# INLINE_PA zipWith7 #-}
zipWith7
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g, Scalar h)
        => (a -> b -> c -> d -> e -> f -> g -> h) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g
        -> PArray h
        
zipWith7 f (PArray len ts) (PArray _ us)  (PArray _ vs)(PArray _ ws) (PArray _ xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith7 f (from ts) (from us) (from vs) (from ws) (from xs) (from ys) (from zs)


-- | Eight seven arrays, yielding a new array.
{-# INLINE_PA zipWith8 #-}
zipWith8
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g, Scalar h, Scalar i)
        => (a -> b -> c -> d -> e -> f -> g -> h -> i) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g
        -> PArray h -> PArray i
        
zipWith8 f (PArray len ss) (PArray _ ts) (PArray _ us)  (PArray _ vs)(PArray _ ws) (PArray _ xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith8 f (from ss) (from ts) (from us) (from vs) (from ws) (from xs) (from ys) (from zs)


                

-- Folds ----------------------------------------------------------------------
-- | Left fold over an array.
{-# INLINE_PA fold #-}
fold    :: Scalar a 
        => (a -> a -> a) -> a -> PArray a -> a

fold f !z (PArray _ pdata)
        = U.fold f z $ from pdata


-- | Left fold over an array, using the first element to initialise the state.
{-# INLINE_PA fold1 #-}
fold1   :: Scalar a
        => (a -> a -> a) -> PArray a -> a

fold1 f (PArray _ pdata)
        = U.fold1 f $ from pdata


-- | Segmented fold of an array of arrays.
folds   :: (Scalar a, U.Elts a)
        => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a

folds f !z (PArray _ (PNested vsegd pdatas _ _))
  = pdatas `seq`  -- Don't seq on vsegd. See Note: fold/promoteSegd
    fromUArray $ U.fold_vs f z vsegd $ fromScalarPDatas pdatas
{-# INLINE_PA folds #-}
           

-- | Segmented fold of an array of arrays, using the first element of each
--   segment to initialse the state for that segment.
fold1s  :: (Scalar a, U.Elts a)
        => (a -> a -> a) -> PArray (PArray a) -> PArray a

fold1s f (PArray _ (PNested vsegd pdatas _ _))
 = pdatas `seq`  -- Don't seq on vsegd. See Note: fold/promoteSegd
   fromUArray $ U.fold1_vs f vsegd $ fromScalarPDatas pdatas
{-# INLINE_PA fold1s #-}


-- | Left fold over an array, also passing the index of each element
--   to the parameter function.
fold1Index
        :: Scalar a
        => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int

fold1Index f
        = fst . U.fold1 f . U.indexed . toUArray
{-# INLINE_PA fold1Index #-}


-- | Segmented fold over an array, also passing the index of each 
--   element to the parameter function.
--   TODO: fold the psegs then replicate, like in the other folds.
--         this currently has the wrong complexity.
fold1sIndex
        :: Scalar a
        => ((Int, a) -> (Int, a) -> (Int, a))
        -> PArray (PArray a) -> PArray Int

{-# INLINE_PA fold1sIndex #-}
fold1sIndex f (PArray n# pdata)
 = let  segd    = takeSegdPD pdata
        xs      = concatPA pdata
   in   PArray n#
         $ toScalarPData
         $ U.fsts
         $ U.fold1_s f segd
         $ U.zip (U.indices_s segd)
         $ fromScalarPData xs

{- [Note: fold/promoteSegd]
   ~~~~~~~~~~~~~~~~~~~~~~~~
   In the segmented fold functions above, don't seq on the vsegd because we
   we need the vsegd to remain as an argument to the fold function. 
   This ensures that the fold/promoteSegdToVSegd rules from DPH_Interface.h
   will fire, which shows up in SMVM.
-}

-- Enumerations --------------------------------------------------------------
-- | Construct a range of integers.
{-# INLINE_PA enumFromTo #-}
enumFromTo :: Int -> Int -> PArray Int
enumFromTo m n 
        = fromUArray (U.enumFromTo m n)


{-# INLINE_PA enumFromTol #-}
enumFromTol :: PArray Int -> PArray Int -> PArray (PArray Int)
enumFromTol (PArray m# ms) (PArray _ ns)
  = let 
        lens  = U.zipWith distance (fromScalarPData ms) (fromScalarPData ns)
        segd  = U.lengthsToSegd lens

        flat    = toScalarPData
                $ U.enumFromStepLenEach 
                        (U.elementsSegd segd)
                        (fromScalarPData ms)
                        (U.replicate (U.elementsSegd segd) 1) 
                        lens
                        
        vsegd   = U.promoteSegdToVSegd segd
        pdatas  = singletondPA flat
        
    in  PArray m# $ PNested vsegd pdatas segd flat
        
distance :: Int -> Int -> Int
{-# INLINE_STREAM distance #-}
distance m n = max 0 (n - m + 1)

