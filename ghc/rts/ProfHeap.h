/* -----------------------------------------------------------------------------
 * $Id: ProfHeap.h,v 1.1 1999/09/15 13:46:29 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/


void heapCensus(void);
extern nat initHeapProfiling(void);
void endHeapProfiling(void);
