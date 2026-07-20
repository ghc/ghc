/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcautoconf.h"
#include "rts/RtsSymbols.h"

#if defined(LEADING_UNDERSCORE)
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

extern RtsSymbolVal* __attribute__((weak)) rtsExtraSyms(void);

/* See Note [_iob_func symbol].  */
#if defined(mingw32_HOST_OS)
extern const void* __rts_iob_func;
#endif
