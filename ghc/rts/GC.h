/* -----------------------------------------------------------------------------
 * $Id: GC.h,v 1.6 2000/04/11 16:36:53 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in GC.c
 *
 * ---------------------------------------------------------------------------*/

void threadPaused(StgTSO *);
StgClosure *isAlive(StgClosure *p);
void GarbageCollect ( void (*get_roots)(void), rtsBool force_major_gc );
