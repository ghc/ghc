/* -----------------------------------------------------------------------------
 * $Id: GC.h,v 1.3 1999/01/26 11:12:45 simonm Exp $
 *
 * Prototypes for functions in GC.c
 *
 * ---------------------------------------------------------------------------*/

void threadPaused(StgTSO *);
StgClosure *isAlive(StgClosure *p);
