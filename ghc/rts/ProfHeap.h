/* -----------------------------------------------------------------------------
 * $Id: ProfHeap.h,v 1.4 2001/12/12 14:31:43 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFHEAP_H
#define PROFHEAP_H

extern void    heapCensus( void );
extern nat     initHeapProfiling( void );
extern void    endHeapProfiling( void );
extern rtsBool closureSatisfiesConstraints( StgClosure* p );
extern void    LDV_recordDead( StgClosure *c, nat size );
extern rtsBool strMatchesSelector( char* str, char* sel );

#endif
