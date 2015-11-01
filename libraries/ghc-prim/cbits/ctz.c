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
#if defined(__GNUC__) && (defined(i386_HOST_ARCH) || defined(powerpc_HOST_ARCH))
  /* On Linux/i386, the 64bit `__builtin_ctzll()` instrinsic doesn't
     get inlined by GCC but rather a short `__ctzdi2` runtime function
     is inserted when needed into compiled object files.

     This workaround forces GCC on 32bit x86 to to express `hs_ctz64` in
     terms of the 32bit `__builtin_ctz()` (this is no loss, as there's no
     64bit BSF instruction on i686 anyway) and thus avoid the problematic
     out-of-line runtime function.
  */

  if (!x) return 64;

  return ((uint32_t)x ? __builtin_ctz((uint32_t)x)
                      : (__builtin_ctz(x >> 32) + 32));

#elif SIZEOF_UNSIGNED_LONG == 8
  return x ? __builtin_ctzl(x) : 64;
#elif SIZEOF_UNSIGNED_LONG_LONG == 8
  return x ? __builtin_ctzll(x) : 64;
#else
# error no suitable __builtin_ctz() found
#endif
}
