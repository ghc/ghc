{-# LANGUAGE CPP, DeriveGeneric, MagicHash #-}

module GHC.Exts.Heap.Ptr.Utils where

import Prelude
import GHC.Ptr
import GHC.Exts

-- | casts a @Ptr@ to an @Int@
ptrToInt :: Ptr a -> Int
ptrToInt (Ptr a#) = I# (addr2Int# a#)
