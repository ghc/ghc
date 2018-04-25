{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for Doubles.
module Data.Array.Parallel.PArray.PData.Double 
        ( PData (..)
        , PDatas(..))
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Typeable                  as T


-------------------------------------------------------------------------------
data instance PData Double
        = PDouble  (U.Array  Double)

data instance PDatas Double
        = PDoubles (U.Arrays Double)


-- PR -------------------------------------------------------------------------
instance PR Double where

  {-# NOINLINE validPR #-}
  validPR _
        = True

  {-# NOINLINE nfPR #-}
  nfPR (PDouble xx)
        = xx `seq` ()

  {-# NOINLINE similarPR #-}
  similarPR  = (==)

  {-# NOINLINE coversPR #-}
  coversPR weak (PDouble uarr) ix
   | weak       = ix <= U.length uarr
   | otherwise  = ix <  U.length uarr

  {-# NOINLINE pprpPR #-}
  pprpPR d
   =    double d

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PDouble vec)
   =   text "PDouble"
   <+> text (show $ U.toList vec)

  {-# NOINLINE typeRepPR #-}
  typeRepPR x           = T.typeOf x

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR _       = T.typeOf (5 :: Double)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR _      = T.typeOf (5 :: Double)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PDouble U.empty

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
        = PDouble $ U.replicate len x

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PDouble arr)
        = PDouble $ U.replicate_s segd arr

  {-# INLINE_PDATA appendPR #-}
  appendPR (PDouble arr1) (PDouble arr2)
        = PDouble $ arr1 U.+:+ arr2

  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PDoubles arr1) segd2 (PDoubles arr2)
        = PDouble $ U.append_vs segdResult segd1 arr1 segd2 arr2


  -- Projections --------------------------------                
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PDouble uarr)
        = U.length uarr

  {-# INLINE_PDATA indexPR #-}
  indexPR (PDouble arr) ix
        = U.index "indexPR[Double]" arr ix

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PDoubles pvecs) srcixs
        = PDouble $ U.map (\(src, ix) -> U.unsafeIndex2s pvecs src ix) srcixs

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PDoubles arrs) vsegd srcixs 
        = PDouble $ U.indexs_avs arrs vsegd srcixs

  {-# INLINE_PDATA extractPR #-}
  extractPR (PDouble arr) start len 
        = PDouble $ U.extract arr start len

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PDoubles arrs) ssegd
        = PDouble $ U.extracts_ass ssegd arrs

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PDoubles arrs) vsegd
        = PDouble $ U.extracts_avs vsegd arrs
                

  -- Pack and Combine ---------------------------
  {-# NOINLINE packByTagPR #-}
  packByTagPR (PDouble arr1) arrTags tag
        = PDouble $ U.packByTag arr1 arrTags tag

  {-# NOINLINE combine2PR #-}
  combine2PR sel (PDouble arr1) (PDouble arr2)
        = PDouble $ U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR xx
        = PDouble (U.fromList $ V.toList xx)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PDouble arr)
        = V.fromList $ U.toList arr


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PDoubles $ U.emptys
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PDouble pdata)
        = PDoubles $ U.singletons pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PDoubles vec)
        = U.lengths vec
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PDoubles vec) ix
        = PDouble $ vec `U.unsafeIndexs` ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PDoubles xs) (PDoubles ys)
        = PDoubles $ xs `U.appends` ys
        
  {-# NOINLINE fromVectordPR #-}
  fromVectordPR pdatas
        = PDoubles 
        $ U.fromVectors 
        $ V.map (\(PDouble vec) -> vec) pdatas
        
  {-# NOINLINE toVectordPR #-}
  toVectordPR (PDoubles vec)
        = V.map PDouble $ U.toVectors vec


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  Double)
deriving instance Show (PDatas Double)

instance PprVirtual (PData Double) where
  pprv (PDouble vec)
   = text (show $ U.toList vec)

