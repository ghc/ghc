{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | PR instance for the void type.
module Data.Array.Parallel.PArray.PData.Void 
         (Void, void, pvoid, fromVoid, pvoids)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PRepr.Base    ()
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.Pretty
import qualified Data.Typeable                  as T
import qualified Data.Vector                    as V

-------------------------------------------------------------------------------
-- | The Void type is used as a place holder in situations where we don't 
--   want to track a real array.
--  
--   For example:
--    A type like Bool is represented as @Sum2 Void Void@, meaning that we only
--    only care about the tag of the data constructor and not its argumnent.
--
--    We also use it as the to fill empty closures.
--
--   Note that arrays of (PData Void) do not have an intrinsic length, which 
--   is the reason that the PR dictionary only contains a coversPR function
--   was well as a partial lengthPR function.
--
data instance PData Void

-- | PVoids instance counts how many "vectors" of void we have
data instance PDatas Void
        = PVoids Int

pvoid :: PData Void
pvoid   = error "Data.Array.Parallel.PData.Void"

pvoids :: Int -> PDatas Void
pvoids  = PVoids 


-- PR --------------------------------------------------------------------------
nope :: String -> a
nope str    = error $ "Data.Array.Parallel.PData.Void: no PR method for " ++ str

instance PR Void where

  {-# NOINLINE validPR #-}
  validPR _       = True

  {-# NOINLINE nfPR #-}
  nfPR _          = ()

  {-# NOINLINE similarPR #-}
  similarPR _ _   = True
  
  {-# NOINLINE coversPR #-}
  coversPR _ _ _  = True
  
  {-# NOINLINE pprpPR #-}
  pprpPR _        = text "void"
  
  {-# NOINLINE pprpDataPR #-}
  pprpDataPR _    = text "pvoid"

  {-# NOINLINE typeRepPR #-}
  typeRepPR x     = T.typeOf x

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR _
        = T.typeOf (error "typeRepDataPR:  void proxy" :: Void)

  {-# NOINLINE typeRepDatasPR #-}
  typeRepDatasPR _
        = T.typeOf (error "typeRepDatasPR: void proxy" :: Void)

  -- Constructors -------------------------------        
  {-# INLINE_PDATA emptyPR #-}
  emptyPR       = nope "emptyPR"

  {-# INLINE_PDATA replicatePR #-}
  replicatePR   = nope "replicate"

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR  = nope "replicates"

  {-# INLINE_PDATA appendPR #-}
  appendPR      = nope "append"
  
  {-# INLINE_PDATA appendvsPR #-}
  appendvsPR     = nope "appendvs"


  -- Projections --------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR _    = nope "length"

  -- We return the black hole here so that we can construct vectors of type
  -- Vector Void during debugging.
  -- See the (A.Array PArray e) instance in D.A.P.PArray for details.
  {-# INLINE_PDATA indexPR #-}
  indexPR _ _   = void

  {-# INLINE_PDATA indexsPR #-}
  indexsPR      = nope "indexs"

  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR     = nope "indexvs"

  {-# INLINE_PDATA extractPR #-}
  extractPR     = nope "extractl"

  {-# INLINE_PDATA extractssPR #-}
  extractssPR    = nope "extractss"

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR    = nope "extractvs"


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR   = nope "packByTag"

  {-# INLINE_PDATA combine2PR #-}
  combine2PR    = nope "combine2"


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR  = nope "fromVector"

  {-# NOINLINE toVectorPR #-}
  toVectorPR _  = nope "toVector"


  -- PDatas -------------------------------------  
  {-# INLINE_PDATA emptydPR #-}    
  emptydPR      = PVoids 0

  {-# INLINE_PDATA singletondPR #-}    
  singletondPR _
        = PVoids 1

  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PVoids n)
        = n

  {-# INLINE_PDATA indexdPR #-}
  indexdPR _ _
        = pvoid
        
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PVoids n1) (PVoids n2)
        = PVoids (n1 + n2)

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
        = PVoids $ V.length vec

  {-# NOINLINE toVectordPR #-}
  toVectordPR (PVoids n)
        = V.replicate n pvoid


-- Show -----------------------------------------------------------------------
instance Show (PData  Void) where
 show _  = "pvoid"


instance Show (PDatas Void) where
 show _  = "pvoids"
 

instance PprVirtual (PData Void) where
  pprv _ = text "pvoid"

