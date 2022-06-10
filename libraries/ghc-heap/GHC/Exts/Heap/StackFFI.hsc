module GHC.Exts.Heap.StackFFI where

#include "Rts.h"

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.InfoTable.Types
#if !defined(TABLES_NEXT_TO_CODE)
import GHC.Exts.Heap.Constants
import Data.Maybe
#endif
import Foreign

peekSmallBitmapWord :: Ptr StgInfoTable -> IO Word
peekSmallBitmapWord itbl =
#if !defined(TABLES_NEXT_TO_CODE)
  let ptr = itbl `plusPtr` (negate wORD_SIZE)
#else
  let ptr = itbl
#endif
  in
    (#peek struct StgInfoTable_, layout.bitmap) ptr

-- #define BITMAP_SIZE(bitmap) ((bitmap) & BITMAP_SIZE_MASK)
bitmapSize :: Word -> Word
bitmapSize bitmap = bitmap .&. (#const BITMAP_SIZE_MASK)

-- #define BITMAP_BITS(bitmap) ((bitmap) >> BITMAP_BITS_SHIFT)
bitmapBits :: Word -> Word
bitmapBits bitmap = bitmap `shiftR` (#const BITMAP_BITS_SHIFT)
