/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void        heapCensus         (Time t);
void        initHeapProfiling  (void);
void        endHeapProfiling   (void);
void        freeHeapProfiling  (void);
bool        strMatchesSelector (const char* str, const char* sel);

#if defined(PROFILING)
// doingRetainerProfiling: `-hr` or `-hr<cc> -h<x>`
bool doingRetainerProfiling(void);
#endif

#include "EndPrivate.h"
