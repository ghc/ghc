/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Performing updates.
 *
 * ---------------------------------------------------------------------------*/

#ifndef UPDATES_H
#define UPDATES_H

/* -----------------------------------------------------------------------------
   Updates

   We have two layers of update macros.  The top layer, UPD_IND() and
   friends perform all the work of an update.  In detail:

      - if the closure being updated is a blocking queue, then all the
        threads waiting on the blocking queue are updated.

      - then the lower level updateWithIndirection() macro is invoked 
        to actually replace the closure with an indirection (see below).

   -------------------------------------------------------------------------- */

#ifdef TICKY_TICKY
# define UPD_IND(updclosure, heapptr) \
   UPD_PERM_IND(updclosure,heapptr)
# define UPD_SPEC_IND(updclosure, ind_info, heapptr, and_then) \
   UPD_PERM_IND(updclosure,heapptr); and_then
#else
#  define SEMI ;
# define UPD_IND(updclosure, heapptr) \
   UPD_REAL_IND(updclosure,INFO_PTR(stg_IND_info),heapptr,SEMI)
# define UPD_SPEC_IND(updclosure, ind_info, heapptr, and_then) \
   UPD_REAL_IND(updclosure,ind_info,heapptr,and_then)
#endif

/* These macros have to work in both C and C--, so here's the
 * impedence matching:
 */
#ifdef CMINUSMINUS
#define BLOCK_BEGIN
#define BLOCK_END
#define DECLARE_IPTR(info)  W_ info
#define FCALL               foreign "C"
#define INFO_PTR(info)      info
#define ARG_PTR             "ptr"
#else
#define BLOCK_BEGIN         {
#define BLOCK_END           }
#define DECLARE_IPTR(info)  const StgInfoTable *(info)
#define FCALL               /* nothing */
#define INFO_PTR(info)      &info
#define StgBlockingQueue_blocking_queue(closure) \
    (((StgBlockingQueue *)closure)->blocking_queue)
#define ARG_PTR             /* nothing */
#endif

/* UPD_IND actually does a PERM_IND if TICKY_TICKY is on;
   if you *really* need an IND use UPD_REAL_IND
 */
#define UPD_REAL_IND(updclosure, ind_info, heapptr, and_then)	\
        BLOCK_BEGIN						\
	DECLARE_IPTR(info);					\
	info = GET_INFO(updclosure);				\
	updateWithIndirection(ind_info,				\
			      updclosure,			\
			      heapptr,				\
			      and_then);			\
	BLOCK_END

#if defined(PROFILING) || defined(TICKY_TICKY)
#define UPD_PERM_IND(updclosure, heapptr)	\
        BLOCK_BEGIN				\
	updateWithPermIndirection(updclosure,	\
				  heapptr);	\
	BLOCK_END
#endif

#if defined(RTS_SUPPORTS_THREADS)

# ifdef TICKY_TICKY
#  define UPD_IND_NOLOCK(updclosure, heapptr)	\
        BLOCK_BEGIN				\
	updateWithPermIndirection(updclosure,	\
				  heapptr);	\
	BLOCK_END
# else
#  define UPD_IND_NOLOCK(updclosure, heapptr)			\
        BLOCK_BEGIN						\
	updateWithIndirection(INFO_PTR(stg_IND_info),		\
			      updclosure,			\
			      heapptr,); 			\
	BLOCK_END
# endif

#else
#define UPD_IND_NOLOCK(updclosure,heapptr) UPD_IND(updclosure,heapptr)
#endif

/* -----------------------------------------------------------------------------
   Awaken any threads waiting on a blocking queue (BLACKHOLE_BQ).
   -------------------------------------------------------------------------- */

#if defined(PAR) 

/* 
   In a parallel setup several types of closures might have a blocking queue:
     BLACKHOLE_BQ ... same as in the default concurrent setup; it will be
                      reawakened via calling UPD_IND on that closure after
		      having finished the computation of the graph
     FETCH_ME_BQ  ... a global indirection (FETCH_ME) may be entered by a 
                      local TSO, turning it into a FETCH_ME_BQ; it will be
		      reawakened via calling processResume
     RBH          ... a revertible black hole may be entered by another 
                      local TSO, putting it onto its blocking queue; since
		      RBHs only exist while the corresponding closure is in 
		      transit, they will be reawakened via calling 
		      convertToFetchMe (upon processing an ACK message)

   In a parallel setup a blocking queue may contain 3 types of closures:
     TSO           ... as in the default concurrent setup
     BLOCKED_FETCH ... indicating that a TSO on another PE is waiting for
                       the result of the current computation
     CONSTR        ... an RBHSave closure (which contains data ripped out of
                       the closure to make room for a blocking queue; since
		       it only contains data we use the exisiting type of
		       a CONSTR closure); this closure is the end of a 
		       blocking queue for an RBH closure; it only exists in
		       this kind of blocking queue and must be at the end
		       of the queue
*/		      
extern void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#define DO_AWAKEN_BQ(bqe, node)  STGCALL2(awakenBlockedQueue, bqe, node);

#define AWAKEN_BQ(info,closure)						\
     	if (info == &stg_BLACKHOLE_BQ_info ||               \
	    info == &stg_FETCH_ME_BQ_info ||                \
	    get_itbl(closure)->type == RBH) {		                \
		DO_AWAKEN_BQ(((StgBlockingQueue *)closure)->blocking_queue, closure);     	                \
	}

#elif defined(GRAN)

extern void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#define DO_AWAKEN_BQ(bq, node)  STGCALL2(awakenBlockedQueue, bq, node);

/* In GranSim we don't have FETCH_ME or FETCH_ME_BQ closures, so they are
   not checked. The rest of the code is the same as for GUM.
*/
#define AWAKEN_BQ(info,closure)						\
     	if (info == &stg_BLACKHOLE_BQ_info ||               \
	    get_itbl(closure)->type == RBH) {		                \
		DO_AWAKEN_BQ(((StgBlockingQueue *)closure)->blocking_queue, closure);     	                \
	}

#endif /* GRAN || PAR */


/* -----------------------------------------------------------------------------
   Updates: lower-level macros which update a closure with an
   indirection to another closure.

   There are several variants of this code.

       PROFILING:
   -------------------------------------------------------------------------- */

/* LDV profiling:
 * We call LDV_recordDead_FILL_SLOP_DYNAMIC(p1) regardless of the generation in 
 * which p1 resides.
 *
 * Note: 
 *   After all, we do *NOT* need to call LDV_RECORD_CREATE() for both IND and 
 *   IND_OLDGEN closures because they are inherently used. But, it corrupts
 *   the invariants that every closure keeps its creation time in the profiling
 *  field. So, we call LDV_RECORD_CREATE().
 */

/* In the DEBUG case, we also zero out the slop of the old closure,
 * so that the sanity checker can tell where the next closure is.
 *
 * Two important invariants: we should never try to update a closure
 * to point to itself, and the closure being updated should not
 * already have been updated (the mutable list will get messed up
 * otherwise).
 *
 * NB. We do *not* do this in THREADED_RTS mode, because when we have the
 * possibility of multiple threads entering the same closure, zeroing
 * the slop in one of the threads would have a disastrous effect on
 * the other (seen in the wild!).
 */
#ifdef CMINUSMINUS

#define FILL_SLOP(p)							\
  W_ inf;								\
  W_ sz;								\
  W_ i;									\
  inf = %GET_STD_INFO(p);						\
  if (%INFO_TYPE(inf) != HALF_W_(THUNK_SELECTOR)			\
	&& %INFO_TYPE(inf) != HALF_W_(BLACKHOLE)			\
	&& %INFO_TYPE(inf) != HALF_W_(CAF_BLACKHOLE)) {			\
      if (%INFO_TYPE(inf) == HALF_W_(AP_STACK)) {			\
          sz = StgAP_STACK_size(p) + BYTES_TO_WDS(SIZEOF_StgAP_STACK_NoThunkHdr); \
      } else {								\
          if (%INFO_TYPE(inf) == HALF_W_(AP)) {				\
	      sz = TO_W_(StgAP_n_args(p)) +  BYTES_TO_WDS(SIZEOF_StgAP_NoThunkHdr);	\
          } else {							\
              sz = TO_W_(%INFO_PTRS(inf)) + TO_W_(%INFO_NPTRS(inf));	\
	  }								\
      }									\
      i = 0;								\
      for:								\
        if (i < sz) {							\
          StgThunk_payload(p,i) = 0;					\
          i = i + 1;							\
          goto for;							\
        }								\
  }

#else /* !CMINUSMINUS */

INLINE_HEADER void
FILL_SLOP(StgClosure *p)
{						
    StgInfoTable *inf = get_itbl(p);		
    nat i, sz;

    switch (inf->type) {
    case BLACKHOLE:
    case CAF_BLACKHOLE:
    case THUNK_SELECTOR:
	return;
    case AP:
	sz = ((StgAP *)p)->n_args + sizeofW(StgAP) - sizeofW(StgThunkHeader);
	break;
    case AP_STACK:
	sz = ((StgAP_STACK *)p)->size + sizeofW(StgAP_STACK) - sizeofW(StgThunkHeader);
	break;
    default:
	sz = inf->layout.payload.ptrs + inf->layout.payload.nptrs;
        break;
    }
    for (i = 0; i < sz; i++) {
	((StgThunk *)p)->payload[i] = 0;
    }
}

#endif /* CMINUSMINUS */

#if !defined(DEBUG) || defined(THREADED_RTS)
#define DEBUG_FILL_SLOP(p) /* do nothing */
#else
#define DEBUG_FILL_SLOP(p) FILL_SLOP(p)
#endif

/* We have two versions of this macro (sadly), one for use in C-- code,
 * and the other for C.
 *
 * The and_then argument is a performance hack so that we can paste in
 * the continuation code directly.  It helps shave a couple of
 * instructions off the common case in the update code, which is
 * worthwhile (the update code is often part of the inner loop).
 * (except that gcc now appears to common up this code again and
 * invert the optimisation.  Grrrr --SDM).
 */
#ifdef CMINUSMINUS
#define generation(n) (W_[generations] + n*SIZEOF_generation)
#define updateWithIndirection(ind_info, p1, p2, and_then)	\
    W_ bd;							\
								\
    DEBUG_FILL_SLOP(p1);					\
    LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(p1);			\
    StgInd_indirectee(p1) = p2;					\
    foreign "C" wb() [];					\
    bd = Bdescr(p1);						\
    if (bdescr_gen_no(bd) != 0 :: CInt) {			\
      foreign "C" recordMutableCap(p1 "ptr",			\
				   MyCapability() "ptr",	\
		                   bdescr_gen_no(bd));		\
      SET_INFO(p1, stg_IND_OLDGEN_info);			\
      LDV_RECORD_CREATE(p1);					\
      TICK_UPD_OLD_IND();					\
      and_then;							\
    } else {							\
      SET_INFO(p1, ind_info);					\
      LDV_RECORD_CREATE(p1);					\
      TICK_UPD_NEW_IND();					\
      and_then;							\
  }
#else
#define updateWithIndirection(ind_info, p1, p2, and_then)		\
  {									\
    bdescr *bd;								\
									\
    /* cas(p1, 0, &stg_WHITEHOLE_info); */				\
    ASSERT( (P_)p1 != (P_)p2 && !closure_IND(p1) );			\
    DEBUG_FILL_SLOP(p1);						\
    LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(p1);				\
    ((StgInd *)p1)->indirectee = p2;					\
    wb();								\
    bd = Bdescr((P_)p1);						\
    if (bd->gen_no != 0) {						\
      recordMutableGenLock(p1, &generations[bd->gen_no]);		\
      SET_INFO(p1, &stg_IND_OLDGEN_info);				\
      TICK_UPD_OLD_IND();						\
      and_then;								\
    } else {								\
      SET_INFO(p1, ind_info);						\
      LDV_RECORD_CREATE(p1);						\
      TICK_UPD_NEW_IND();						\
      and_then;								\
    }									\
  }
#endif

/* The permanent indirection version isn't performance critical.  We
 * therefore use an inline C function instead of the C-- macro.
 */
#ifndef CMINUSMINUS
INLINE_HEADER void
updateWithPermIndirection(StgClosure *p1,
	                  StgClosure *p2) 
{
  bdescr *bd;

  ASSERT( p1 != p2 && !closure_IND(p1) );

  /*
   * @LDV profiling
   * Destroy the old closure.
   * Nb: LDV_* stuff cannot mix with ticky-ticky
   */
  LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(p1);

  bd = Bdescr((P_)p1);
  if (bd->gen_no != 0) {
    recordMutableGenLock(p1, &generations[bd->gen_no]);
    ((StgInd *)p1)->indirectee = p2;
    SET_INFO(p1, &stg_IND_OLDGEN_PERM_info);
    /*
     * @LDV profiling
     * We have just created a new closure.
     */
    LDV_RECORD_CREATE(p1);
    TICK_UPD_OLD_PERM_IND();
  } else {
    ((StgInd *)p1)->indirectee = p2;
    SET_INFO(p1, &stg_IND_PERM_info);
    /*
     * @LDV profiling
     * We have just created a new closure.
     */
    LDV_RECORD_CREATE(p1);
    TICK_UPD_NEW_PERM_IND(p1);
  }
}
#endif

#endif /* UPDATES_H */
