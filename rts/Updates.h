/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2013
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
 *   After all, we do *NOT* need to call LDV_RECORD_CREATE() for IND
 *   closures because they are inherently used. But, it corrupts
 *   the invariants that every closure keeps its creation time in the profiling
 *  field. So, we call LDV_RECORD_CREATE().
 */

/*
 * We have two versions of this macro (sadly), one for use in C-- code,
 * and the other for C.
 *
 * The and_then argument is a performance hack so that we can paste in
 * the continuation code directly.  It helps shave a couple of
 * instructions off the common case in the update code, which is
 * worthwhile (the update code is often part of the inner loop).
 */
#ifdef CMINUSMINUS

#define UPDATE_FRAME_FIELDS(w_,p_,info_ptr,ccs,p2,updatee)      \
                 w_ info_ptr,                           \
                 PROF_HDR_FIELDS(w_,ccs,p2)              \
                 p_ updatee


#define updateWithIndirection(p1, p2, and_then) \
    W_ bd;							\
								\
    OVERWRITING_CLOSURE(p1);                                    \
    StgInd_indirectee(p1) = p2;                                 \
    prim_write_barrier;                                         \
    SET_INFO(p1, stg_BLACKHOLE_info);                           \
    LDV_RECORD_CREATE(p1);                                      \
    bd = Bdescr(p1);						\
    if (bdescr_gen_no(bd) != 0 :: bits16) {			\
      recordMutableCap(p1, TO_W_(bdescr_gen_no(bd)));           \
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
    OVERWRITING_CLOSURE(p1);
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
