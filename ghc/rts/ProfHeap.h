/* -----------------------------------------------------------------------------
 * $Id: ProfHeap.h,v 1.2 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/


extern void    heapCensus( void );
extern nat     initHeapProfiling( void );
extern void    endHeapProfiling( void );
extern rtsBool closureSatisfiesConstraints( StgClosure* p );
