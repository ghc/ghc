/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFHEAP_H
#define PROFHEAP_H

#include "GetTime.h" // for Ticks

#include "BeginPrivate.h"

void    heapCensus         (Ticks t);
nat     initHeapProfiling  (void);
void    endHeapProfiling   (void);
rtsBool strMatchesSelector (char* str, char* sel);

#include "EndPrivate.h"

#endif /* PROFHEAP_H */
