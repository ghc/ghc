/* -----------------------------------------------------------------------------
 * $Id: DebugProf.h,v 1.2 1998/12/02 13:28:15 simonm Exp $
 *
 * (c) The GHC Team 1998
 *
 * Simple Heap Profiling
 *
 * ---------------------------------------------------------------------------*/

#if !defined(PROFILING) && defined(DEBUG)

extern nat  initProfiling(void);
extern void endProfiling(void);
extern void heapCensus(bdescr *bd);

#endif
