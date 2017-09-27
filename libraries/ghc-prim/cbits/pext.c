#include "Rts.h"
#include "MachDeps.h"

extern StgWord hs_pext8(StgWord src);
StgWord
hs_pext8(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

extern StgWord hs_pext16(StgWord src);
StgWord
hs_pext16(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

extern StgWord hs_pext32(StgWord src);
StgWord
hs_pext32(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

extern StgWord hs_pext64(StgWord64 src);
StgWord
hs_pext64(StgWord src, StgWord mask)
{
  uint64_t result = 0;
  int offset = 0;

  for (int bit = 0; bit != sizeof(uint64_t) * 8; ++bit) {
    const uint64_t src_bit = (src >> bit) & 1;
    const uint64_t mask_bit = (mask >> bit) & 1;

    if (mask_bit) {
      result |= (uint64_t)(src_bit) << offset;
      ++offset;
    }
  }

  return result;
}

#if WORD_SIZE_IN_BITS == 32

extern StgWord hs_pext(StgWord src);
StgWord
hs_pext(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

#elif WORD_SIZE_IN_BITS == 64

extern StgWord hs_pext(StgWord src);
StgWord
hs_pext(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

#else

#error Unknown machine word size

#endif
