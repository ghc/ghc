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
#include "sm/Globalise.h"
#endif

#ifndef CMINUSMINUS
#include "BeginPrivate.h"
#endif

#include "WritePolicy.h"

/* -----------------------------------------------------------------------------
   Updates
   -------------------------------------------------------------------------- */

/* LDV profiling:
 *   After all, we do *NOT* need to call LDV_RECORD_CREATE() for IND
 *   closures because they are inherently used. But, it corrupts the
 *   invariants that every closure keeps its creation time in the
 *   profiling field. So, we call LDV_RECORD_CREATE().
 */

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

// NB. for young-generation updates we use stg_IND_direct_info as an
// optimisation - this kind of indirection assumes that the indirectee
// is a value, so can just return its payload rather than entering it.
// Hence the argument p2 here is required to be a value, not another
// indirection, BLACKHOLE, or thunk.
#define updateWithIndirectionToValue(p1, p2, and_then)		\
    W_ bd, p3;							\
								\
    OVERWRITING_CLOSURE(p1);                                    \
    bd = Bdescr(p1);                                            \
    if (bdescr_gen_no(bd) != 0 :: bits16) {			\
      ("ptr" p3) = foreign "C" UPDATE_GLOBALISE(MyCapability(), p2 "ptr"); \
      StgInd_indirectee(p1) = p3;			        \
      prim %write_barrier() [];					\
      SET_INFO(p1, stg_BLACKHOLE_info);                         \
      LDV_RECORD_CREATE(p1);                                    \
      TICK_UPD_OLD_IND();					\
      and_then;							\
    } else {							\
      StgInd_indirectee(p1) = p2;				\
      prim %write_barrier() [];					\
      SET_INFO(p1, stg_IND_direct_info);                        \
      LDV_RECORD_CREATE(p1);                                    \
      TICK_UPD_NEW_IND();					\
      and_then;							\
  }

#else /* !CMINUSMINUS */

// NB. unlike updateWithIndirectionToValue above, p2 is not required
// to be a value, and we use the more generic stg_IND_info.
INLINE_HEADER void updateWithIndirection (Capability *cap, 
                                          StgClosure *p1, 
                                          StgClosure *p2)
{
    bdescr *bd;
    StgClosure *p3;
    
    ASSERT( (P_)p1 != (P_)p2 );
    /* not necessarily true: ASSERT( !closure_IND(p1) ); */
    /* occurs in RaiseAsync.c:raiseAsync() */
    OVERWRITING_CLOSURE(p1);
    bd = Bdescr((StgPtr)p1);
    if (bd->gen_no != 0) {
        p3 = UPDATE_GLOBALISE(cap, p2);
        ((StgInd *)p1)->indirectee = p3;
        write_barrier();
        SET_INFO(p1, &stg_BLACKHOLE_info);
        LDV_RECORD_CREATE(p1);
        TICK_UPD_OLD_IND();
    } else {
        ((StgInd *)p1)->indirectee = p2;
        write_barrier();
        SET_INFO(p1, &stg_IND_info);
        LDV_RECORD_CREATE(p1);
        TICK_UPD_NEW_IND();
    }
}

#endif /* CMINUSMINUS */

#ifndef CMINUSMINUS
#include "EndPrivate.h"
#endif

#endif /* UPDATES_H */
