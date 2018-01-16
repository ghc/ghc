{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for tuples.
module Data.Array.Parallel.PArray.PData.Tuple2
        ( PData(..),    PDatas(..)
        , zipPD
        , ziplPR
        , unzipPD
        , unziplPD)
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import GHC.Exts
import Prelude hiding (zip, unzip)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Typeable                  as T
import qualified Prelude                        as P


-------------------------------------------------------------------------------
data instance PData (a, b)
        = PTuple2  (PData a)  (PData b)

data instance PDatas (a, b)
        = PTuple2s (PDatas a) (PDatas b)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b) => PR (a, b) where

  {-# NOINLINE validPR #-}
  validPR (PTuple2 xs ys)
        = validPR xs && validPR ys


  {-# NOINLINE nfPR #-}
  nfPR (PTuple2 arr1 arr2)
        = nfPR arr1 `seq` nfPR arr2 `seq` ()


  {-# NOINLINE similarPR #-}
  similarPR (x1, y1) (x2, y2)
        =  similarPR x1 x2
        && similarPR y1 y2


  {-# NOINLINE coversPR #-}
  coversPR weak (PTuple2 arr1 arr2) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix

  {-# NOINLINE pprpPR #-}
  pprpPR (x, y)
        = text "Tuple2 " <> vcat [pprpPR x, pprpPR y]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple2 xs ys)
        = text "PTuple2 " <> vcat [pprpDataPR xs, pprpDataPR ys]

  {-# NOINLINE typeRepPR #-}
  typeRepPR x@(a, b)
        = T.typeOf2 x 
                `T.mkAppTy` (typeRepPR a)
                `T.mkAppTy` (typeRepPR b)

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PTuple2 xs ys)
        = T.typeOf2 ((), ())
                `T.mkAppTy` (typeRepDataPR xs)
                `T.mkAppTy` (typeRepDataPR ys)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PTuple2s xs ys)
        = T.typeOf2 ((), ())
                `T.mkAppTy` (typeRepDatasPR xs)
                `T.mkAppTy` (typeRepDatasPR ys)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple2 emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y)
        = PTuple2 (replicatePR len x)
                  (replicatePR len y)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple2 arr1 arr2)
        = PTuple2 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple2 arr11 arr12) (PTuple2 arr21 arr22)
        = PTuple2 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)


  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PTuple2s arrs11 arrs12) segd2 (PTuple2s arrs21 arrs22)
        = PTuple2 (appendvsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendvsPR segdResult segd1 arrs12 segd2 arrs22)


  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple2 arr1 _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple2 arr1 arr2) ix
        = (indexPR arr1 ix, indexPR arr2 ix)

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PTuple2s xs ys) srcixs
        = PTuple2 (indexsPR xs srcixs)
                  (indexsPR ys srcixs)

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PTuple2s xs ys) vsegd srcixs
        = PTuple2 (indexvsPR xs vsegd srcixs)
                  (indexvsPR ys vsegd srcixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple2 arr1 arr2) start len
        = PTuple2 (extractPR arr1 start len) 
                  (extractPR arr2 start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PTuple2s xs ys) ussegd
        = PTuple2 (extractssPR xs ussegd)
                  (extractssPR ys ussegd)

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PTuple2s xs ys) uvsegd
        = PTuple2 (extractvsPR xs uvsegd)
                  (extractvsPR ys uvsegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple2 arr1 arr2) tags tag
        = PTuple2 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple2 xs1 ys1) (PTuple2 xs2 ys2)
        = PTuple2 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys)       = V.unzip vec
     in  PTuple2  (fromVectorPR xs)
                  (fromVectorPR ys)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PTuple2 xs ys)
        = V.zip   (toVectorPR xs)
                  (toVectorPR ys)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple2s emptydPR
                   emptydPR

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple2 x y)
        = PTuple2s (singletondPR x)
                   (singletondPR y)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple2s xs _)
        = lengthdPR xs
   
   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple2s xs ys) i
        = PTuple2  (indexdPR xs i)
                   (indexdPR ys i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple2s xs1 ys1) (PTuple2s xs2 ys2)
        = PTuple2s (appenddPR xs1 xs2)
                   (appenddPR ys1 ys2)
  

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss) = V.unzip $ V.map (\(PTuple2 xs ys) -> (xs, ys)) vec
     in  PTuple2s  (fromVectordPR xss)
                   (fromVectordPR yss)


  {-# NOINLINE toVectordPR #-}
  toVectordPR (PTuple2s pdatas1 pdatas2)
        = V.zipWith PTuple2
                (toVectordPR pdatas1)
                (toVectordPR pdatas2)


-- PD Functions ---------------------------------------------------------------
-- These work on PData arrays of tuples, but don't need a PA or PR dictionary

-- | O(1). Zip a pair of arrays into an array of pairs.
zipPD   :: PData a -> PData b -> PData (a, b)
zipPD   = PTuple2
{-# INLINE_PA zipPD #-}


-- | Lifted zip.
ziplPR   :: (PR a, PR b) => PData (PArray a) -> PData (PArray b) -> PData (PArray (a, b))
ziplPR arr1 arr2
 = let  -- We need to flatten the data here because we can't guarantee
        -- that the vsegds of both arrays have the same form.
        -- One of the arrays may have been created with replicate, and 
        -- thus has internal sharing, while the other does not.
        (segd1, pdata1) = flattenPR arr1
        (_,     pdata2) = flattenPR arr2
        vsegd'          = U.promoteSegdToVSegd segd1

   in   PNested vsegd'
                (PTuple2s (singletondPR pdata1) (singletondPR pdata2))
                segd1
                (PTuple2  pdata1 pdata2)

{-# INLINE_PA ziplPR #-}


-- | O(1). Unzip an array of pairs into a pair of arrays.
unzipPD :: PData (a, b) -> (PData a, PData b)
unzipPD (PTuple2 xs ys) = (xs, ys)
{-# INLINE_PA unzipPD #-}


-- | Lifted unzip.
unziplPD  :: PData (PArray (a, b)) -> PData (PArray a, PArray b)
unziplPD (PNested vsegd (PTuple2s xsdata ysdata) segd (PTuple2 xflat yflat))
 =      PTuple2 (PNested vsegd xsdata segd xflat)
                (PNested vsegd ysdata segd yflat)
{-# INLINE_PA unziplPD #-}


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b)) => Show (PData  (a, b))
deriving instance (Show (PDatas a), Show (PDatas b)) => Show (PDatas (a, b))


instance ( PR a, PR b, Show a, Show b
         , PprVirtual (PData a), PprVirtual (PData b))
        => PprVirtual (PData (a, b)) where
 pprv   (PTuple2 xs ys)
        = text $ show 
        $ P.zip (V.toList $ toVectorPR xs) 
                (V.toList $ toVectorPR ys)

