/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_SYMBOLS_H
#define RTS_SYMBOLS_H

#include "ghcautoconf.h"

#ifdef LEADING_UNDERSCORE
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

typedef struct _RtsSymbolVal {
    const char   *lbl;
    void   *addr;
} RtsSymbolVal;


extern  RtsSymbolVal rtsSyms[];

#endif /* RTS_SYMBOLS_H */
