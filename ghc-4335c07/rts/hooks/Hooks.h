/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * User-overridable RTS hooks.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

extern char *ghc_rts_opts;

extern void OnExitHook (void);
extern void StackOverflowHook (W_ stack_size);
extern void OutOfHeapHook (W_ request_size, W_ heap_size);
extern void MallocFailHook (W_ request_size /* in bytes */, const char *msg);
extern void FlagDefaultsHook (void);
extern void LongGCSync (uint32_t capno, Time t);
extern void LongGCSyncEnd (Time t);

#include "EndPrivate.h"
