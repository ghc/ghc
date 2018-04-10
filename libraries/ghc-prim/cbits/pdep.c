#include "Rts.h"
#include "MachDeps.h"

StgWord64
hs_pdep64(StgWord64 src, StgWord64 mask)
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

StgWord
hs_pdep32(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

StgWord
hs_pdep16(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}

StgWord
hs_pdep8(StgWord src, StgWord mask)
{
  return hs_pdep64(src, mask);
}
