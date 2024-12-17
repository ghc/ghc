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

#if MIN_VERSION_GLASGOW_HASKELL(9,9,0,0)
// Unique64 patch was present in 9.10 and later
#define HAVE_UNIQUE64 1
#elif !MIN_VERSION_GLASGOW_HASKELL(9,9,0,0) && MIN_VERSION_GLASGOW_HASKELL(9,8,4,0)
// Unique64 patch was backported to 9.8.4
#define HAVE_UNIQUE64 1
#elif !MIN_VERSION_GLASGOW_HASKELL(9,7,0,0) && MIN_VERSION_GLASGOW_HASKELL(9,6,7,0)
// Unique64 patch was backported to 9.6.7
#define HAVE_UNIQUE64 1
#endif

#if !defined(HAVE_UNIQUE64)
HsWord64 ghc_unique_counter64 = 0;
#endif
