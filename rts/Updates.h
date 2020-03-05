/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2013
 *
 * Performing updates.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if !defined(CMINUSMINUS)
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
#if defined(CMINUSMINUS)

#define UPDATE_FRAME_FIELDS(w_,p_,info_ptr,ccs,p2,updatee)      \
                 w_ info_ptr,                           \
                 PROF_HDR_FIELDS(w_,ccs,p2)              \
                 p_ updatee

/*
 * Getting the memory barriers correct here is quite tricky. Essentially
 * the write barrier ensures that any writes to the new indirectee are visible
 * before we introduce the indirection.
 * See Note [Heap memory barriers] in SMP.h.
 */
#define updateWithIndirection(p1, p2, and_then) \
    W_ bd;                                                      \
                                                                \
    prim_write_barrier;                                         \
    OVERWRITING_CLOSURE(p1);                                    \
    bd = Bdescr(p1);                                            \
    if (bdescr_gen_no(bd) != 0 :: bits16) {                     \
      IF_NONMOVING_WRITE_BARRIER_ENABLED {                      \
        ccall updateRemembSetPushThunk_(BaseReg, p1 "ptr");     \
      }                                                         \
      recordMutableCap(p1, TO_W_(bdescr_gen_no(bd)));           \
      TICK_UPD_OLD_IND();                                       \
    } else {                                                    \
      TICK_UPD_NEW_IND();                                       \
    }                                                           \
    StgInd_indirectee(p1) = p2;                                 \
    prim_write_barrier;                                         \
    SET_INFO(p1, stg_BLACKHOLE_info);                           \
    LDV_RECORD_CREATE(p1);                                      \
    and_then;

#else /* !CMINUSMINUS */

INLINE_HEADER void updateWithIndirection (Capability *cap,
                                          StgClosure *p1,
                                          StgClosure *p2)
{
    ASSERT( (P_)p1 != (P_)p2 );
    /* not necessarily true: ASSERT( !closure_IND(p1) ); */
    /* occurs in RaiseAsync.c:raiseAsync() */
    /* See Note [Heap memory barriers] in SMP.h */
    write_barrier();
    bdescr *bd = Bdescr((StgPtr)p1);
    if (bd->gen_no != 0) {
      IF_NONMOVING_WRITE_BARRIER_ENABLED {
          updateRemembSetPushThunk(cap, (StgThunk*)p1);
      }
        recordMutableCap(p1, cap, bd->gen_no);
        TICK_UPD_OLD_IND();
    } else {
        TICK_UPD_NEW_IND();
    }
    OVERWRITING_CLOSURE(p1);
    ((StgInd *)p1)->indirectee = p2;
    write_barrier();
    SET_INFO(p1, &stg_BLACKHOLE_info);
    LDV_RECORD_CREATE(p1);
}

#endif /* CMINUSMINUS */

#if !defined(CMINUSMINUS)
#include "EndPrivate.h"
#endif
