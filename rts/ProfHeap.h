/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFHEAP_H
#define PROFHEAP_H

BEGIN_RTS_PRIVATE

void    heapCensus         (void);
nat     initHeapProfiling  (void);
void    endHeapProfiling   (void);
void    LDV_recordDead     (StgClosure *c, nat size);
rtsBool strMatchesSelector (char* str, char* sel);

END_RTS_PRIVATE

#endif /* PROFHEAP_H */
