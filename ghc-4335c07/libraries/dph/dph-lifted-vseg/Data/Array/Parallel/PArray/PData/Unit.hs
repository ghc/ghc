{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for unit.
module Data.Array.Parallel.PArray.PData.Unit where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Pretty
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Typeable                  as T

-------------------------------------------------------------------------------
-- | TODO: For arrays of units, we're currently maintaining their length so
--   that validPR works properly. In future we should ditch the length field
--   and rely on coversPR to check that indices are in bounds, like we do
--   with arrays of type PData Void.
data instance PData ()
        = PUnit  Int

data instance PDatas ()
        = PUnits (U.Array Int)

punit   :: Int -> PData ()
punit   = PUnit


-- PR -------------------------------------------------------------------------
instance PR () where

  {-# NOINLINE validPR #-}
  validPR _
        = True

  {-# NOINLINE nfPR #-}
  nfPR xx
        = xx `seq` ()
  
  {-# NOINLINE similarPR #-}
  similarPR _ _
        = True

  {-# NOINLINE coversPR #-}
  coversPR weak (PUnit n) i
   | weak       = i <= n
   | otherwise  = i <  n

  {-# NOINLINE pprpPR #-}
  pprpPR _
        = text "()"

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR uu
        = text $ show uu

  {-# NOINLINE typeRepPR #-}
  typeRepPR x
        = T.typeOf x

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR _
        = T.typeOf ()

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR _
        = T.typeOf ()


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PUnit 0

  {-# INLINE_PDATA replicatePR #-}
  replicatePR n _
        = PUnit n

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd _
        = PUnit (U.elementsSegd segd)
                
  {-# INLINE_PDATA appendPR #-}
  appendPR (PUnit len1) (PUnit len2)
        = PUnit (len1 + len2)

  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR segdResult _ _ _ _
        = PUnit (U.lengthSegd segdResult)


  -- Projections -------------------------------        
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PUnit n)
        = n

  {-# INLINE_PDATA indexPR #-}
  indexPR _ _
        = ()

  {-# INLINE_PDATA indexsPR #-}
  indexsPR _ srcixs
        = PUnit $ U.length srcixs

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR _ _ srcixs
        = PUnit $ U.length srcixs

  {-# INLINE_PDATA extractPR #-}
  extractPR _ _ len
        = PUnit len
        
  {-# INLINE_PDATA extractssPR #-}
  extractssPR _ ussegd
        = PUnit $ U.sum $ U.lengthsOfSSegd ussegd

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR _ uvsegd
        = PUnit $ U.sum $ U.takeLengthsOfVSegd uvsegd
  

  -- Pack and Combine ---------------------------        
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR _ tags tag
        = PUnit (U.length $ U.filter (== tag) tags)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel2 _ _
        = PUnit ( U.elementsSel2_0 sel2
                + U.elementsSel2_1 sel2)


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec
        = PUnit (V.length vec)

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PUnit len)
        = V.replicate len ()

  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR
        = PUnits $ U.empty

  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PUnit n)
        = PUnits $ U.replicate 1 n

  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PUnits pdatas)
        = U.length pdatas
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PUnits pdatas) ix
        = PUnit $ U.index "indexdPR[Unit]" pdatas ix
        
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PUnits lens1) (PUnits lens2)
        = PUnits $ lens1 U.+:+ lens2

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
        = PUnits $ V.convert $ V.map lengthPR vec
        
  {-# NOINLINE toVectordPR #-}
  toVectordPR (PUnits uvecs)
        = V.map PUnit $ V.convert uvecs


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  ())
deriving instance Show (PDatas ())

instance PprVirtual (PData ()) where
  pprv (PUnit n)
   = text $ "[ () x " ++ show n ++ " ]"

