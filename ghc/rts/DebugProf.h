/* -----------------------------------------------------------------------------
 * $Id: DebugProf.h,v 1.3 1999/09/15 13:45:16 simonmar Exp $
 *
 * (c) The GHC Team 1998
 *
 * Simple Heap Profiling
 *
 * ---------------------------------------------------------------------------*/

#if !defined(PROFILING) && defined(DEBUG)

extern void heapCensus(bdescr *bd);

#endif
