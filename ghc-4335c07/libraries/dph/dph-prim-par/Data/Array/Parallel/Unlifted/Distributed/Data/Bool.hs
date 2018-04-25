{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Distributed.Data.Bool
        ( orD
        , andD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.Scalar.Base        ()
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DPrim
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import qualified Data.Array.Parallel.Unlifted.Distributed.What  as W
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Vector.Unboxed.Mutable                    as MV
import Prelude as P


instance DPrim Bool where
  mkDPrim               = DBool
  unDPrim (DBool a)     = a

  mkMDPrim              = MDBool
  unMDPrim (MDBool a)   = a


instance DT Bool where
  data Dist  Bool       = DBool  !(V.Vector    Bool)
  data MDist Bool s     = MDBool !(MV.STVector s Bool)

  indexD                = primIndexD
  newMD                 = primNewMD
  readMD                = primReadMD
  writeMD               = primWriteMD
  unsafeFreezeMD        = primUnsafeFreezeMD
  sizeD                 = primSizeD
  sizeMD                = primSizeMD


-- | OR together all instances of a distributed 'Bool'.
orD :: Gang -> Dist Bool -> Bool
orD g   = foldD (W.What "orD") g (||)
{-# INLINE_DIST orD #-}


-- | AND together all instances of a distributed 'Bool'.
andD :: Gang -> Dist Bool -> Bool
andD g  = foldD (W.What "andD") g (&&)
{-# INLINE_DIST andD #-}


