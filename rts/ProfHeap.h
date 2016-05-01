/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFHEAP_H
#define PROFHEAP_H

#include "BeginPrivate.h"

void        heapCensus         (Time t);
uint32_t    initHeapProfiling  (void);
void        endHeapProfiling   (void);
rtsBool     strMatchesSelector (const char* str, const char* sel);

#ifdef PROFILING
// doingRetainerProfiling: `-hr` or `-hr<cc> -h<x>`
rtsBool doingRetainerProfiling(void);
#endif

#include "EndPrivate.h"

#endif /* PROFHEAP_H */
