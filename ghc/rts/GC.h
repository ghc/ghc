/* -----------------------------------------------------------------------------
 * $Id: GC.h,v 1.5 2000/01/13 14:34:03 hwloidl Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in GC.c
 *
 * ---------------------------------------------------------------------------*/

void threadPaused(StgTSO *);
StgClosure *isAlive(StgClosure *p);
void GarbageCollect(void (*get_roots)(void));
