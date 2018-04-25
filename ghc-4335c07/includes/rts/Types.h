/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS-specific types.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stddef.h>
#include <stdbool.h>

// Deprecated, use uint32_t instead.
typedef unsigned int nat __attribute__((deprecated));  /* uint32_t */

/* ullong (64|128-bit) type: only include if needed (not ANSI) */
#if defined(__GNUC__)
#define LL(x) (x##LL)
#else
#define LL(x) (x##L)
#endif

typedef struct StgClosure_   StgClosure;
typedef struct StgInfoTable_ StgInfoTable;
typedef struct StgTSO_       StgTSO;
