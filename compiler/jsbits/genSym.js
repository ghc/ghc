//#OPTIONS: CPP
#include "Unique.h"

// We assume that the unique tag occupies less than 32 bits (should be safe)
#define HIGH_UNIQUE_BITS (32 - UNIQUE_TAG_BITS)
#define HIGH_UNIQUE_MASK ((1 << HIGH_UNIQUE_BITS) - 1)

// The 'ghc_unique_inc' and 'ghc_unique_counter64' are in the native RTS. It allows them to be
// shared with plugins even if two different instances of the GHC library are
// loaded at the same time (#19940)
// However, cross compilers do not support plugins so we have moved these globals back
// into the compiler.
var h$ghc_unique_inc       = h$newByteArray(4);
h$ghc_unique_inc.i3[0]     = 1;
var h$ghc_unique_counter64   = h$newByteArray(8);
h$ghc_unique_counter64.i3[0] = 0;
h$ghc_unique_counter64.i3[1] = 0;

function h$genSym() {
  var rl = h$hs_plusWord64(h$ghc_unique_counter64.i3[1] >>> 0, h$ghc_unique_counter64.i3[0] >>> 0, 0, h$ghc_unique_inc.i3[0] >>> 0);
  h$ret1 = (h$ret1 & HIGH_UNIQUE_MASK) >>> 0;
  // h$ret1 contains the higher part (rh)
  h$ghc_unique_counter64.i3[0] = rl | 0;
  h$ghc_unique_counter64.i3[1] = h$ret1 | 0;
  return rl; // h$ret1 still contains rh
}
