{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USegd.DT
where
import Data.Array.Parallel.Unlifted.Distributed.Data.Vector             ()
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import Prelude                                                          as P
import Data.Array.Parallel.Pretty
import Control.Monad

instance DT USegd where
  data Dist  USegd   
        = DUSegd  !(Dist (Vector Int))          -- segment lengths
                  !(Dist (Vector Int))          -- segment indices
                  !(Dist Int)                   -- number of elements in this chunk

  data MDist USegd s 
        = MDUSegd !(MDist (Vector Int) s)       -- segment lengths
                  !(MDist (Vector Int) s)       -- segment indices
                  !(MDist Int        s)         -- number of elements in this chunk

  indexD str (DUSegd lens idxs eles) i
   = USegd.mkUSegd
        (indexD (str ++ "/indexD[USegd]") lens i)
        (indexD (str ++ "/indexD[USegd]") idxs i)
        (indexD (str ++ "/indexD[USegd]") eles i)
  {-# INLINE_DIST indexD #-}

  newMD g
   =    liftM3 MDUSegd (newMD g) (newMD g) (newMD g)
  {-# INLINE_DIST newMD #-}

  readMD (MDUSegd lens idxs eles) i
   =    liftM3 USegd.mkUSegd (readMD lens i) (readMD idxs i) (readMD eles i)
  {-# INLINE_DIST readMD #-}

  writeMD (MDUSegd lens idxs eles) i segd
   = do writeMD lens i (USegd.takeLengths  segd)
        writeMD idxs i (USegd.takeIndices  segd)
        writeMD eles i (USegd.takeElements segd)
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDUSegd lens idxs eles)
   =    liftM3 DUSegd (unsafeFreezeMD lens)
                      (unsafeFreezeMD idxs)
                      (unsafeFreezeMD eles)
  {-# INLINE_DIST unsafeFreezeMD #-}

  deepSeqD segd z
   = deepSeqD (USegd.takeLengths  segd)
   $ deepSeqD (USegd.takeIndices  segd)
   $ deepSeqD (USegd.takeElements segd) z
  {-# INLINE_DIST deepSeqD #-}

  sizeD  (DUSegd  _ _ eles) 
   = sizeD eles
  {-# INLINE_DIST sizeD #-}

  sizeMD (MDUSegd _ _ eles) 
   = sizeMD eles
  {-# INLINE_DIST sizeMD #-}

  measureD segd 
   = "Segd " P.++ show (USegd.length segd)
   P.++ " "  P.++ show (USegd.takeElements segd)
  {-# NOINLINE measureD #-}
  --  NOINLINE because this is only used for debugging.


instance PprPhysical (Dist USegd) where
 pprp (DUSegd lens indices elements)
  =  text "DUSegd"
  $$ (nest 7 $ vcat
        [ text "lengths: " <+> pprp lens
        , text "indices: " <+> pprp indices
        , text "elements:" <+> pprp elements])
 {-# NOINLINE pprp #-}
 --  NOINLINE because this is only used for debugging.
