/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcautoconf.h"
#include "LinkerInternals.h"
#include <stdbool.h>

#if defined(LEADING_UNDERSCORE)
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

typedef struct _RtsSymbolVal {
    const SymbolName* lbl;
    SymbolAddr* addr;
    bool weak;
} RtsSymbolVal;

extern RtsSymbolVal rtsSyms[];
