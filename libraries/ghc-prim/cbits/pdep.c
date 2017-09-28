#include "Rts.h"
#include "MachDeps.h"

extern StgWord hs_pdep64(StgWord64 src, StgWord mask);
StgWord
hs_pdep64(StgWord src, StgWord mask)
{
  uint64_t result = 0;

  while (1) {
    // Mask out all but the lowest bit
    const uint64_t lowest = (-mask & mask);

    if (lowest == 0) {
      break;
    }

    const uint64_t lsb = (uint64_t)((int64_t)(src << 63) >> 63);

    result |= lsb & lowest;
    mask &= ~lowest;
    src >>= 1;
  }

  return result;
}

extern StgWord hs_pdep32(StgWord src, StgWord mask);
StgWord
hs_pdep32(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

extern StgWord hs_pdep16(StgWord src, StgWord mask);
StgWord
hs_pdep16(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

extern StgWord hs_pdep8(StgWord src, StgWord mask);
StgWord
hs_pdep8(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

#if WORD_SIZE_IN_BITS == 32

extern StgWord hs_pdep(StgWord src, StgWord mask);
StgWord
hs_pdep(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

#elif WORD_SIZE_IN_BITS == 64

extern StgWord hs_pdep(StgWord src, StgWord mask);
StgWord
hs_pdep(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

#else

#error Unknown machine word size

#endif
