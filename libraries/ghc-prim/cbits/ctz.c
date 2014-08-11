#include "MachDeps.h"
#include "Rts.h"
#include <stdint.h>

// Fall-back implementations for count-trailing-zeros primop
//
// __builtin_ctz*() is supported by GCC and Clang

#if SIZEOF_UNSIGNED_INT == 4
StgWord
hs_ctz8(StgWord x)
{
  return (uint8_t)x ? __builtin_ctz(x) : 8;
}

StgWord
hs_ctz16(StgWord x)
{
  return (uint16_t)x ? __builtin_ctz(x) : 16;
}

StgWord
hs_ctz32(StgWord x)
{
  return (uint32_t)x ? __builtin_ctz(x) : 32;
}
#else
# error no suitable __builtin_ctz() found
#endif

StgWord
hs_ctz64(StgWord64 x)
{
#if SIZEOF_UNSIGNED_LONG == 8
  return x ? __builtin_ctzl(x) : 64;
#elif SIZEOF_UNSIGNED_LONG_LONG == 8
  return x ? __builtin_ctzll(x) : 64;
#else
# error no suitable __builtin_ctz() found
#endif
}
