module GHC.Exts.Heap.StackFFI where

#include "Rts.h"
#undef BLOCK_SIZE
#undef MBLOCK_SIZE
#undef BLOCKS_PER_MBLOCK
#include "DerivedConstants.h"

-- TODO: Check imports: Are all needed?
import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.InfoTable.Types
#if !defined(TABLES_NEXT_TO_CODE)
import GHC.Exts.Heap.Constants
import Data.Maybe
#endif
import Foreign
import Debug.Trace

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
} deriving (Show)

peekStgLargeBitmap :: Ptr LargeBitmap -> IO LargeBitmap
peekStgLargeBitmap largeBitmapPtr = do
-- #if !defined(TABLES_NEXT_TO_CODE)
--   largeBitmapPtr <- (#peek struct StgInfoTable_, layout.large_bitmap) itbl
-- #else
-- -- large_bitmap_offset
--   offset <- (#peek struct StgInfoTable_, layout.large_bitmap_offset) itbl
--   let largeBitmapPtr = plusPtr itbl offset
-- #endif
  traceM $ "peekStgLargeBitmap - largeBitmapPtr : " ++ show largeBitmapPtr
  size' <- (#peek StgLargeBitmap, size) largeBitmapPtr
  traceM $ "peekStgLargeBitmap - size' : " ++ show size'
  -- bitmapArrayPtr <- (#peek StgLargeBitmap, bitmap) largeBitmapPtr
  -- traceM $ "peekStgLargeBitmap - bitmapArrayPtr : " ++ show bitmapArrayPtr
  bitmap' <- peekArray size' (plusPtr largeBitmapPtr (#const OFFSET_StgLargeBitmap_bitmap))
  pure $ LargeBitmap {
    -- This is safe: ´StgLargeBitmap.size´ is a StgWord in C/RTS
    size =  fromIntegral size',
    bitmap = bitmap'
                     }

bitsInWord :: Word
bitsInWord = (#const BITS_IN(StgWord))

bytesInWord :: Word
bytesInWord = (#const sizeof(StgWord))

payloadOffset = (#size StgHeader) + (#const OFFSET_StgClosure_payload)
-- TODO: Ptr should not be polymorphic. I.e. use a saturized type.
-- TODO: Doesn't need to be here (in hsc file)
payloadPtr :: Ptr a -> Ptr Word
payloadPtr sp = plusPtr sp payloadOffset
