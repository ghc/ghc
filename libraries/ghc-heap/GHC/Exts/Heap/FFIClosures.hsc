module GHC.Exts.Heap.FFIClosures where

#include "Rts.h"

import Prelude
import Foreign

peekStgThreadID :: Ptr a -> IO Word64
peekStgThreadID ptr = (#peek struct StgTSO_, id) ptr


peekAllocLimit :: Ptr a -> IO Int64
peekAllocLimit ptr = (#peek struct StgTSO_, alloc_limit) ptr
