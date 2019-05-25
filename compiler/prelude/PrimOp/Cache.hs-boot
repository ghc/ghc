module PrimOp.Cache where

import Platform
import {-# SOURCE #-} PrimOp (PrimOp, PrimOpInfo)

-- fully transparent for UNPACK pragma
data PrimOpCache = PrimOpCache
  { allThePrimOps :: ![PrimOp]
  , primOpInfo :: !(PrimOp -> PrimOpInfo)
  }

mkPrimOpCache :: Platform -> PrimOpCache
