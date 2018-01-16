{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for Ints
module Data.Array.Parallel.PArray.PData.Int () where
import Data.Array.Parallel.PArray.PData.Base
import Data.Typeable                            as T
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Text.PrettyPrint
import Prelude                                  as P
import Data.Array.Parallel.Pretty


-- PR -------------------------------------------------------------------------
instance PR Int where

  {-# NOINLINE validPR #-}
  validPR _
        = True

  {-# NOINLINE nfPR #-}
  nfPR (PInt xx)
        = xx `seq` ()

  {-# NOINLINE similarPR #-}
  similarPR  = (==)

  {-# NOINLINE coversPR #-}
  coversPR weak (PInt uarr) ix
   | weak       = ix <= U.length uarr
   | otherwise  = ix <  U.length uarr

  {-# NOINLINE pprpPR #-}
  pprpPR i
   =    int i

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PInt uarr)
   =    text "PInt" <+> pprp uarr

  {-# NOINLINE typeRepPR #-}
  typeRepPR x           = T.typeOf x

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR _       = T.typeOf (5 :: Int)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR _      = T.typeOf (5 :: Int)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PInt U.empty

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
        = PInt (U.replicate len x)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PInt arr)
        = PInt (U.replicate_s segd arr)
                
  {-# INLINE_PDATA appendPR #-}
  appendPR (PInt arr1) (PInt arr2)
        = PInt $ arr1 U.+:+ arr2

  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PInts arr1) segd2 (PInts arr2)
        = PInt $ U.append_vs segdResult segd1 arr1 segd2 arr2


  -- Projections --------------------------------                
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PInt uarr) 
        = U.length uarr

  {-# INLINE_PDATA indexPR #-}
  indexPR (PInt uarr) ix
        = U.index "indexPR[Int]" uarr ix

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PInts pvecs) srcixs
        = PInt $ U.map (\(src, ix) -> U.unsafeIndex2s pvecs src ix) srcixs

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PInts arrs) vsegd srcixs 
        = PInt $ U.indexs_avs arrs vsegd srcixs

  {-# INLINE_PDATA extractPR #-}
  extractPR (PInt arr) start len 
        = PInt $ U.extract arr start len

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PInts arrs) ssegd
        = PInt $ U.extracts_ass ssegd arrs

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PInts arrs) vsegd
        = PInt $ U.extracts_avs vsegd arrs


  -- Pack and Combine ---------------------------
  {-# NOINLINE packByTagPR #-}
  packByTagPR (PInt arr1) arrTags tag
        = PInt $ U.packByTag arr1 arrTags tag

  {-# NOINLINE combine2PR #-}
  combine2PR sel (PInt arr1) (PInt arr2)
        = PInt $ U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR xx
        = PInt $U.fromList $ V.toList xx

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PInt arr)
        = V.fromList $ U.toList arr


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PInts $ U.emptys
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PInt arr)
        = PInts $ U.singletons arr
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PInts arrs)
        = U.lengths arrs
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PInts arrs) ix
        = PInt $ arrs `U.unsafeIndexs` ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PInts xs) (PInts ys)
        = PInts $ xs `U.appends` ys
        
  {-# NOINLINE fromVectordPR #-}
  fromVectordPR pdatas
        = PInts 
        $ U.fromVectors 
        $ V.map (\(PInt vec) -> vec) pdatas
        
  {-# NOINLINE toVectordPR #-}
  toVectordPR (PInts vec)
        = V.map PInt $ U.toVectors vec


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  Int)
deriving instance Show (PDatas Int)

instance PprPhysical (U.Array Int) where
  pprp uarr 
   =    text (show $ U.toList uarr)

instance PprVirtual (PData Int) where
  pprv (PInt vec)
   = text (show $ U.toList vec)
