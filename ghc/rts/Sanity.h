/* -----------------------------------------------------------------------------
 * $Id: Sanity.h,v 1.5 2000/01/13 14:34:04 hwloidl Exp $
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
#if defined(GRAN)
extern void checkTSOsSanity(void);
extern rtsBool checkThreadQSanity (PEs proc, rtsBool check_TSO_too);
extern rtsBool checkThreadQsSanity (rtsBool check_TSO_too);
#endif

extern StgOffset checkClosure( StgClosure* p );

/* test whether an object is already on update list */
extern rtsBool isBlackhole( StgTSO* tso, StgClosure* p );

#endif /* DEBUG */
 
