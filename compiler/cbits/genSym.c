#include <Rts.h>
#include <assert.h>
#include "Unique.h"
#include <ghcversion.h>

// These global variables have been moved into the RTS.  It allows them to be
// shared with plugins even if two different instances of the GHC library are
// loaded at the same time (#19940)
//
// The CPP is thus about the RTS version GHC is linked against, and not the
// version of the GHC being built.
#if !MIN_VERSION_GLASGOW_HASKELL(9,9,0,0)
HsWord64 ghc_unique_counter64 = 0;
#endif
