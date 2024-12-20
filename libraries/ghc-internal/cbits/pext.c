#include "Rts.h"
#include "MachDeps.h"

StgWord64
hs_pext64(StgWord64 src, StgWord64 mask)
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

StgWord
hs_pext32(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

StgWord
hs_pext16(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}

StgWord
hs_pext8(StgWord src, StgWord mask)
{
  return hs_pext64(src, mask);
}
