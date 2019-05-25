module PrimOp.Cache where

import Platform
import {-# SOURCE #-} PrimOp (PrimOp, PrimOpInfo)

data PrimOpCache

mkPrimOpCache :: Platform -> PrimOpCache

primOpInfo :: PrimOpCache -> PrimOp -> PrimOpInfo
