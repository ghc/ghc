/* -----------------------------------------------------------------------------
 * $Id: Sanity.h,v 1.9 2001/07/23 17:23:19 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Sanity.c
 *
 * ---------------------------------------------------------------------------*/

#ifdef DEBUG

# if defined(PAR)
# define PVM_PE_MASK    0xfffc0000
# define MAX_PVM_PES    MAX_PES
# define MAX_PVM_TIDS   MAX_PES
# define MAX_SLOTS      100000
# endif

/* debugging routines */
extern void checkHeap      ( bdescr *bd );
extern void checkHeapChunk ( StgPtr start, StgPtr end );
extern void checkChain     ( bdescr *bd );
extern void checkStack     ( StgPtr sp, StgPtr stack_end, StgUpdateFrame* su );
extern void checkTSO       ( StgTSO* tso );
extern void checkGlobalTSOList ( rtsBool checkTSOs );
extern void checkStaticObjects ( StgClosure* static_objects );
extern void checkStackChunk    ( StgPtr sp, StgPtr stack_end );
extern StgOffset checkClosure  ( StgClosure* p );

extern void checkMutableList   ( StgMutClosure *p, nat gen );
extern void checkMutOnceList   ( StgMutClosure *p, nat gen );

#if defined(GRAN)
extern void checkTSOsSanity(void);
extern rtsBool checkThreadQSanity (PEs proc, rtsBool check_TSO_too);
extern rtsBool checkThreadQsSanity (rtsBool check_TSO_too);
#endif

#if defined(PAR)
extern void checkBQ (StgBlockingQueueElement *bqe, StgClosure *closure);
#else
extern void checkBQ (StgTSO *bqe, StgClosure *closure);
#endif

#if defined(PAR)
extern void checkLAGAtable(rtsBool check_closures);
extern void checkHeapChunk(StgPtr start, StgPtr end);
#endif

/* test whether an object is already on update list */
extern rtsBool isBlackhole( StgTSO* tso, StgClosure* p );

#endif /* DEBUG */
 
