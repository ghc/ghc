/* -----------------------------------------------------------------------------
 * $Id: Sanity.h,v 1.4 1999/02/05 16:02:52 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Sanity.c
 *
 * ---------------------------------------------------------------------------*/

#ifdef DEBUG
/* debugging routines */
extern void checkHeap  ( bdescr *bd, StgPtr start );
extern void checkChain ( bdescr *bd );
extern void checkStack ( StgPtr sp, StgPtr stack_end, StgUpdateFrame* su );
extern void checkTSO   ( StgTSO* tso );

extern StgOffset checkClosure( StgClosure* p );

/* test whether an object is already on update list */
extern rtsBool isBlackhole( StgTSO* tso, StgClosure* p );

#endif /* DEBUG */
 
