/* -----------------------------------------------------------------------------
 * $Id: GC.h,v 1.4 1999/02/05 16:02:43 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in GC.c
 *
 * ---------------------------------------------------------------------------*/

void threadPaused(StgTSO *);
StgClosure *isAlive(StgClosure *p);
