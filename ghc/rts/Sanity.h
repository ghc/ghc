/* -----------------------------------------------------------------------------
 * $Id: Sanity.h,v 1.6 2000/03/31 03:09:36 hwloidl Exp $
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
extern void checkGlobalTSOList (rtsBool checkTSOs);
extern void checkStaticObjects ( void );
#if defined(GRAN)
extern void checkTSOsSanity(void);
extern rtsBool checkThreadQSanity (PEs proc, rtsBool check_TSO_too);
extern rtsBool checkThreadQsSanity (rtsBool check_TSO_too);
#endif
#if defined(PAR)
extern void checkBQ (StgBlockingQueueElement *bqe, StgClosure *closure);
extern void checkLAGAtable(rtsBool check_closures);
extern void checkHeapChunk(StgPtr start, StgPtr end);
#else
extern void checkBQ (StgTSO *bqe, StgClosure *closure);
#endif

extern StgOffset checkClosure( StgClosure* p );

/* test whether an object is already on update list */
extern rtsBool isBlackhole( StgTSO* tso, StgClosure* p );

#endif /* DEBUG */
 
