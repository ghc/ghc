{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for the Wrap type.
module Data.Array.Parallel.PArray.PData.Wrap where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import qualified Data.Vector                    as V

-------------------------------------------------------------------------------
newtype instance PData (Wrap a)
        = PWrap  (PData a)

newtype instance PDatas (Wrap a)
        = PWraps (PDatas a)


-- PR -------------------------------------------------------------------------
instance PA a => PR (Wrap a) where       

  {-# NOINLINE validPR #-}
  validPR (PWrap pdata)  
        = validPA pdata

  {-# NOINLINE nfPR #-}
  nfPR (PWrap pdata)      
        = nfPA pdata

  {-# NOINLINE similarPR #-}
  similarPR (Wrap x) (Wrap y)
        = similarPA x y

  {-# NOINLINE coversPR #-}
  coversPR weak (PWrap pdata) ix
        = coversPA weak pdata ix

  {-# NOINLINE pprpPR #-}
  pprpPR (Wrap x)
        = pprpPA x

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PWrap pdata)
        = pprpDataPA pdata

  {-# NOINLINE typeRepPR #-}
  typeRepPR  (Wrap x)
        = typeRepPA x

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PWrap pdata)
        = typeRepDataPA pdata

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR (PWraps pdata)
        = typeRepDatasPA pdata


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR               
        = PWrap emptyPA
  
  {-# INLINE_PDATA replicatePR #-}
  replicatePR n (Wrap x)
        = PWrap $ replicatePA n x

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PWrap xs)
        = PWrap $ replicatesPA segd xs

  {-# INLINE_PDATA appendPR #-}
  appendPR (PWrap xs) (PWrap ys)
        = PWrap $ appendPA xs ys
        
  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult segd1 (PWraps xs) segd2 (PWraps ys)
        = PWrap $ appendsPA segdResult segd1 xs segd2 ys
        

  -- Projections --------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PWrap xs)
        = lengthPA xs
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PWrap xs) ix
        = Wrap  $ indexPA xs ix

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PWraps pdatas) srcixs
        = PWrap $ indexsPA pdatas srcixs

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR (PWraps arrs) vsegd srcixs
        = PWrap $ indexvsPA arrs vsegd srcixs

  {-# INLINE_PDATA extractPR #-}
  extractPR (PWrap xs) ix n
        = PWrap $ extractPA xs ix n
        
  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PWraps pdatas) ssegd
        = PWrap $ extractssPA pdatas ssegd

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PWraps pdatas) vsegd
        = PWrap $ extractvsPA pdatas vsegd


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PWrap xs) tags tag
        = PWrap $ packByTagPA xs tags tag

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PWrap xs) (PWrap ys)
        = PWrap $ combine2PA sel xs ys


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec 
        = PWrap $ fromVectorPA $ V.map unWrap vec
        
  {-# NOINLINE toVectorPR #-}
  toVectorPR (PWrap pdata)
        = V.map Wrap $ toVectorPA pdata


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PWraps emptydPA

  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PWrap pdata)
        = PWraps $ singletondPA pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PWraps pdatas)
        = lengthdPA pdatas
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PWraps pdatas) ix
        = PWrap $ indexdPA pdatas ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PWraps xs) (PWraps ys)
        = PWraps $ appenddPA xs ys

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
        = PWraps $ fromVectordPA $ V.map (\(PWrap x) -> x) vec

  {-# NOINLINE toVectordPR #-}
  toVectordPR (PWraps pdatas)
        = V.map PWrap $ toVectordPA pdatas
        


