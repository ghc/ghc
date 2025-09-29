#include "MachDeps.h"
#include "Rts.h"
#include <stdint.h>

// Fall-back implementations for count-leading-zeros primop
//
// __builtin_clz*() is supported by GCC and Clang

#if SIZEOF_UNSIGNED_INT == 4
StgWord
hs_clz8(StgWord x)
{
  return (uint8_t)x ? __builtin_clz((uint8_t)x)-24 : 8;
}

StgWord
hs_clz16(StgWord x)
{
  return (uint16_t)x ? __builtin_clz((uint16_t)x)-16 : 16;
}

StgWord
hs_clz32(StgWord x)
{
  return (uint32_t)x ? __builtin_clz((uint32_t)x) : 32;
}
#else
# error no suitable __builtin_clz() found
#endif

StgWord
hs_clz64(StgWord64 x)
{
#if SIZEOF_UNSIGNED_LONG == 8
  return x ? __builtin_clzl(x) : 64;
#elif SIZEOF_UNSIGNED_LONG_LONG == 8
  return x ? __builtin_clzll(x) : 64;
#else
# error no suitable __builtin_clz() found
#endif
}
