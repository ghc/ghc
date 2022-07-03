module GHC.Exts.Heap.StackFFI where

#include "Rts.h"

-- TODO: Check imports: Are all needed?
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

-- TODO: unused
-- #define BITMAP_SIZE(bitmap) ((bitmap) & BITMAP_SIZE_MASK)
bitmapSize :: Word -> Word
bitmapSize b = b .&. (#const BITMAP_SIZE_MASK)

-- TODO: unused
-- #define BITMAP_BITS(bitmap) ((bitmap) >> BITMAP_BITS_SHIFT)
bitmapBits :: Word -> Word
bitmapBits b = b `shiftR` (#const BITMAP_BITS_SHIFT)

data LargeBitmap = LargeBitmap {
  size :: Word,
  bitmap :: [Word]
}

peekStgLargeBitmap :: Ptr StgInfoTable -> IO LargeBitmap
peekStgLargeBitmap itbl = do
#if !defined(TABLES_NEXT_TO_CODE)
  largeBitmapPtr <- (#peek struct StgInfoTable_, layout.large_bitmap) itbl
#else
-- large_bitmap_offset
  offset <- (#peek struct StgInfoTable_, layout.large_bitmap_offset) itbl
  let largeBitmapPtr = plusPtr itbl offset
#endif
  size' <- (#peek StgLargeBitmap, size) largeBitmapPtr
  bitmapArrayPtr <- (#peek StgLargeBitmap, bitmap) largeBitmapPtr
  bitmap' <- peekArray size' bitmapArrayPtr
  pure $ LargeBitmap {
    -- This is safe: ´StgLargeBitmap.size´ is a StgWord in C/RTS
    size =  fromIntegral size',
    bitmap = bitmap'
                     }

bitsInWord :: Word
bitsInWord = (#const BITS_IN(StgWord))

-- TODO: Ptr should not be polymorphic. I.e. use a saturized type.
payloadPtr :: Ptr a -> IO (Ptr Word)
payloadPtr sp = (#peek StgClosure, payload) sp
