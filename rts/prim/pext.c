#include "Rts.h"
#include "MachDeps.h"

static StgWord64
hs_pext(const unsigned char bit_width, const StgWord64 src, const StgWord64 mask)
{
  uint64_t result = 0;
  int offset = 0;

  for (int bit = 0; bit != bit_width; ++bit) {
    const uint64_t src_bit = (src >> bit) & 1;
    const uint64_t mask_bit = (mask >> bit) & 1;

    if (mask_bit) {
      result |= (uint64_t)(src_bit) << offset;
      ++offset;
    }
  }

  return result;
}

StgWord64
hs_pext64(const StgWord64 src, const StgWord64 mask)
{
  return hs_pext(64, src, mask);
}

// When dealing with values of bit-width shorter than uint64_t, ensure to
// cast the return value to correctly truncate the undefined upper bits.
// This is *VERY* important when GHC is using the LLVM backend!
StgWord
hs_pext32(const StgWord src, const StgWord mask)
{
  return (StgWord) ((StgWord32) hs_pext(32, src, mask));
}

StgWord
hs_pext16(const StgWord src, const StgWord mask)
{
  return (StgWord) ((StgWord16) hs_pext(16, src, mask));
}

StgWord
hs_pext8(const StgWord src, const StgWord mask)
{
  return (StgWord) ((StgWord8) hs_pext(8, src, mask));
}
