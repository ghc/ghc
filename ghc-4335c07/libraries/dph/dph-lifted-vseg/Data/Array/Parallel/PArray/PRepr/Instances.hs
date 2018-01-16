{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP, UndecidableInstances #-}
#include "fusion-phases.h"

-- | Simple instances for the PRRepr/PA family and class.
--   This module is kept separate from PRepr.Base to break an import cycle
--   between PRepr.Base PRepr.Instances and PArray.PData.Wrap
--
module Data.Array.Parallel.PArray.PRepr.Instances where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base

import Data.Array.Parallel.PArray.PData.Void
import Data.Array.Parallel.PArray.PData.Sum2    
import Data.Array.Parallel.PArray.PData.Word8
import Data.Array.Parallel.PArray.PData.Wrap    ()
import Data.Array.Parallel.PArray.PData.Unit    ()
import Data.Array.Parallel.PArray.PData.Nested  ()
import Data.Array.Parallel.PArray.PData.Tuple2  ()
import Data.Array.Parallel.PArray.PData.Int     ()
import Data.Array.Parallel.PArray.PData.Double  ()
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Data.Word


-- Void -----------------------------------------------------------------------
type instance PRepr Void = Void

instance PA Void where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Unit -----------------------------------------------------------------------
type instance PRepr () = ()

instance PA () where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int = Int

instance PA Int where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Int ------------------------------------------------------------------------
type instance PRepr Word8 = Word8

instance PA Word8 where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double = Double

instance PA Double where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id
 
  
-- Bool -----------------------------------------------------------------------
-- | We use the `Void` type for both sides because we only care about the tag.
--   The `Void` fields don't use any space at runtime.
type instance PRepr Bool
  = Sum2 Void Void

data instance PData Bool
  = PBool   U.Sel2

data instance PDatas Bool
  = PBools (V.Vector U.Sel2)

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False          = Alt2_1 void
  toPRepr True           = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _)   = False
  fromPRepr (Alt2_2 _)   = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) 
        = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _)
        = PBool sel

  {-# INLINE toArrPReprs #-}
  toArrPReprs (PBools sels)
        = PSum2s sels
                (pvoids $ V.length sels)
                (pvoids $ V.length sels)

  {-# INLINE fromArrPReprs #-}
  fromArrPReprs (PSum2s sels _ _)
        = PBools sels


-- Ordering -------------------------------------------------------------------
type instance PRepr  Ordering
 = Word8

data instance PData Ordering
 = POrdering  (U.Array Word8)
 
data instance PDatas Ordering
 = POrderings (U.Arrays Word8)

instance PA Ordering where
 {-# INLINE toPRepr #-}
 toPRepr LT     = 0
 toPRepr EQ     = 1
 toPRepr GT     = 2
 
 {-# INLINE fromPRepr #-}
 fromPRepr 0    = LT
 fromPRepr 1    = EQ
 fromPRepr 2    = GT
 fromPRepr _    = error "dph-prim-vseg: bad value converting Word8 to Ordering"
 
 {-# INLINE toArrPRepr #-}
 toArrPRepr (POrdering arr)
        = PWord8 arr
        
 {-# INLINE fromArrPRepr #-}
 fromArrPRepr (PWord8 arr)
        = POrdering arr

 {-# INLINE toArrPReprs #-}
 toArrPReprs (POrderings arrs)
        = PWord8s arrs
        
 {-# INLINE fromArrPReprs #-}
 fromArrPReprs (PWord8s arrs)
        = POrderings arrs


-- Either ---------------------------------------------------------------------
type instance PRepr (Either a b)
 = Sum2 a b
 
data instance PData (Either a b)
 = PEither U.Sel2 (PData a) (PData b)

data instance PDatas (Either a b)
 = PEithers (V.Vector U.Sel2) (PDatas a) (PDatas b)

instance (PR a, PR b) => PA (Either a b) where
  {-# INLINE toPRepr #-}
  toPRepr xx
   = case xx of
        Left x    -> Alt2_1 x
        Right y   -> Alt2_2 y

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 x)   = Left x
  fromPRepr (Alt2_2 x)   = Right x

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PEither sel pdata1 pdata2)
        = PSum2 sel pdata1 pdata2
        
  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel pdata1 pdata2)
        = PEither sel pdata1 pdata2

  {-# INLINE toArrPReprs #-}
  toArrPReprs (PEithers sels pdatas1 pdatas2)
        = PSum2s sels pdatas1 pdatas2

  {-# INLINE fromArrPReprs #-}
  fromArrPReprs (PSum2s sels pdatas1 pdatas2)
        = PEithers sels pdatas1 pdatas2

