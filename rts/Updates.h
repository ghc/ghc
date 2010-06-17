/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Performing updates.
 *
 * ---------------------------------------------------------------------------*/

#ifndef UPDATES_H
#define UPDATES_H

#ifndef CMINUSMINUS
#include "BeginPrivate.h"
#endif

/* -----------------------------------------------------------------------------
   Updates
   -------------------------------------------------------------------------- */

/* LDV profiling:
 * We call LDV_recordDead_FILL_SLOP_DYNAMIC(p1) regardless of the generation in 
 * which p1 resides.
 *
 * Note: 
 *   After all, we do *NOT* need to call LDV_RECORD_CREATE() for IND
 *   closures because they are inherently used. But, it corrupts
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
  if (%INFO_TYPE(inf) != HALF_W_(BLACKHOLE)) {				\
      if (%INFO_TYPE(inf) == HALF_W_(THUNK_SELECTOR)) {			\
	  sz = BYTES_TO_WDS(SIZEOF_StgSelector_NoThunkHdr);		\
     } else {								\
          if (%INFO_TYPE(inf) == HALF_W_(AP_STACK)) {			\
              sz = StgAP_STACK_size(p) + BYTES_TO_WDS(SIZEOF_StgAP_STACK_NoThunkHdr); \
          } else {							\
              if (%INFO_TYPE(inf) == HALF_W_(AP)) {			\
	          sz = TO_W_(StgAP_n_args(p)) +  BYTES_TO_WDS(SIZEOF_StgAP_NoThunkHdr);	\
              } else {							\
                  sz = TO_W_(%INFO_PTRS(inf)) + TO_W_(%INFO_NPTRS(inf)); \
	      }								\
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
	goto no_slop;
	// we already filled in the slop when we overwrote the thunk
	// with BLACKHOLE, and also an evacuated BLACKHOLE is only the
	// size of an IND.
    case THUNK_SELECTOR:
	sz = sizeofW(StgSelector) - sizeofW(StgThunkHeader);
	break;
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
no_slop:
    ;
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

#define updateWithIndirection(p1, p2, and_then)	\
    W_ bd;							\
								\
    DEBUG_FILL_SLOP(p1);					\
    LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(p1);			\
    StgInd_indirectee(p1) = p2;					\
    prim %write_barrier() [];					\
    SET_INFO(p1, stg_BLACKHOLE_info);                           \
    LDV_RECORD_CREATE(p1);                                      \
    bd = Bdescr(p1);						\
    if (bdescr_gen_no(bd) != 0 :: bits16) {			\
      recordMutableCap(p1, TO_W_(bdescr_gen_no(bd)), R1);  	\
      TICK_UPD_OLD_IND();					\
      and_then;							\
    } else {							\
      TICK_UPD_NEW_IND();					\
      and_then;							\
  }

#else /* !CMINUSMINUS */

INLINE_HEADER void updateWithIndirection (Capability *cap, 
                                          StgClosure *p1, 
                                          StgClosure *p2)
{
    bdescr *bd;
    
    ASSERT( (P_)p1 != (P_)p2 );
    /* not necessarily true: ASSERT( !closure_IND(p1) ); */
    /* occurs in RaiseAsync.c:raiseAsync() */
    DEBUG_FILL_SLOP(p1);
    LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(p1);
    ((StgInd *)p1)->indirectee = p2;
    write_barrier();
    SET_INFO(p1, &stg_BLACKHOLE_info);
    LDV_RECORD_CREATE(p1);
    bd = Bdescr((StgPtr)p1);
    if (bd->gen_no != 0) {
        recordMutableCap(p1, cap, bd->gen_no);
        TICK_UPD_OLD_IND();
    } else {
        TICK_UPD_NEW_IND();
    }
}

#endif /* CMINUSMINUS */

#ifndef CMINUSMINUS
#include "EndPrivate.h"
#endif

#endif /* UPDATES_H */
