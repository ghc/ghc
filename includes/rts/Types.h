/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS-specific types.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_TYPES_H
#define RTS_TYPES_H

#include <stddef.h>

typedef unsigned int     nat;           /* at least 32 bits (like int) */

// Deprecated; just use StgWord instead
typedef StgWord lnat;

/* ullong (64|128-bit) type: only include if needed (not ANSI) */
#if defined(__GNUC__) 
#define LL(x) (x##LL)
#else
#define LL(x) (x##L)
#endif
  
typedef enum { 
    rtsFalse = 0, 
    rtsTrue 
} rtsBool;

typedef struct StgClosure_   StgClosure;
typedef struct StgInfoTable_ StgInfoTable;
typedef struct StgTSO_       StgTSO;

#endif /* RTS_TYPES_H */
