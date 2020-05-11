module GHC.Exts.Heap.FFIClosures where

#include "Rts.h"

import Prelude
import Foreign
import Foreign.Ptr
import Data.Int

import GHC.Exts.Heap.Closures

peekStgThreadID :: Ptr a -> IO Word64
peekStgThreadID ptr = do
    id <- (#peek struct StgTSO_, id) ptr
    return id

peekAllocLimit :: Ptr a -> IO Int64
peekAllocLimit ptr = do
    alloc_limit <- (#peek struct StgTSO_, alloc_limit) ptr
    return alloc_limit
