/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Profiling.h"
#include "LdvProfile.h"
#include "Stats.h"
#include "RtsUtils.h"
#include "Schedule.h"

bool isInherentlyUsed( StgHalfWord closure_type )
{
    switch(closure_type) {
    case TSO:
    case STACK:
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    case TVAR:
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
    case ARR_WORDS:
    case WEAK:
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    case BCO:
    case PRIM:
    case MUT_PRIM:
    case TREC_CHUNK:
        return true;

    default:
        return false;
    }
}

/* --------------------------------------------------------------------------
 * This function is called eventually on every object destroyed during
 * a garbage collection, whether it is a major garbage collection or
 * not.  If c is an 'inherently used' closure, nothing happens.  If c
 * is an ordinary closure, LDV_recordDead() is called on c with its
 * proper size which excludes the profiling header portion in the
 * closure.  Returns the size of the closure, including the profiling
 * header portion, so that the caller can find the next closure.
 * ----------------------------------------------------------------------- */
STATIC_INLINE uint32_t
processHeapClosureForDead( const StgClosure *c )
{
    uint32_t size;
    const StgInfoTable *info;

    info = get_itbl(c);

    info = c->header.info;
    if (IS_FORWARDING_PTR(info)) {
        // The size of the evacuated closure is currently stored in
        // the LDV field.  See SET_EVACUAEE_FOR_LDV() in
        // rts/include/StgLdvProf.h.
        return LDVW(c);
    }
    info = INFO_PTR_TO_STRUCT(info);

    ASSERT(((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) <= era &&
           ((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) > 0);
    ASSERT(((LDVW(c) & LDV_STATE_MASK) == LDV_STATE_CREATE) ||
           (
               (LDVW(c) & LDV_LAST_MASK) <= era &&
               (LDVW(c) & LDV_LAST_MASK) > 0
               ));


    size = closure_sizeW(c);

    /*
          'inherently used' cases: do nothing.
    */
    if(isInherentlyUsed(info->type))
        return size;

    switch (info->type) {
        /*
          ordinary cases: call LDV_recordDead().
        */
    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_SELECTOR:
    case THUNK_2_0:
    case THUNK_1_1:
    case THUNK_0_2:
    case AP:
    case PAP:
    case AP_STACK:
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_NOCAF:
    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_2_0:
    case FUN_1_1:
    case FUN_0_2:
    case CONTINUATION:
    case BLACKHOLE:
    case BLOCKING_QUEUE:
        /*
          'Ignore' cases
        */
        // Why can we ignore IND closures? We assume that
        // any census is preceded by a major garbage collection, which
        // IND closures cannot survive. Therefore, it is no
        // use considering IND closures in the meanwhile
        // because they will perish before the next census at any
        // rate.
    case IND:
        // Found a dead closure: record its size
        LDV_recordDead(c, size);
        return size;

        /*
          Error case
        */
        // static objects
    case IND_STATIC:
    case FUN_STATIC:
    case THUNK_STATIC:
        // stack objects
    case UPDATE_FRAME:
    case CATCH_FRAME:
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
        // others
    case INVALID_OBJECT:
    case COMPACT_NFDATA:
    default:
        barf("Invalid object in processHeapClosureForDead(): %d", info->type);
        return 0;
    }
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosureForDead() on every *dead* closures in the
 * heap blocks starting at bd.
 * ----------------------------------------------------------------------- */
static void
processHeapForDead( bdescr *bd )
{
    while (bd != NULL) {
        StgPtr p = bdescr_start(bd);
        const StgPtr free = bdescr_free(bd);
        while (p < free) {
            p += processHeapClosureForDead((StgClosure *)p);
            while (p < free && !*p)   // skip slop
                p++;
        }
        ASSERT(p == free);
        bd = bd->link;
    }
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosureForDead() on every *dead* closures in the closure
 * chain.
 * ----------------------------------------------------------------------- */
static void
processChainForDead( bdescr *bd )
{
    // Any object still in the chain is dead!
    while (bd != NULL) {
        if (!(bd->flags & BF_PINNED)) {
            processHeapClosureForDead((StgClosure *)bdescr_start(bd));
        }
        bd = bd->link;
    }
}

/* --------------------------------------------------------------------------
 * Start a census for *dead* closures, and calls
 * processHeapClosureForDead() on every closure which died in the
 * current garbage collection.  This function is called from a garbage
 * collector right before tidying up, when all dead closures are still
 * stored in the heap and easy to identify.  Generations 0 through N
 * have just been garbage collected.
 * ----------------------------------------------------------------------- */
void
LdvCensusForDead( uint32_t N )
{
    uint32_t g;

    // era == 0 means that LDV profiling is currently turned off.
    if (era == 0)
        return;

    if (RtsFlags.GcFlags.generations == 1) {
        //
        // Todo: support LDV for two-space garbage collection.
        //
        barf("Lag/Drag/Void profiling not supported with -G1");
    } else {
        for (g = 0; g <= N; g++) {
            processHeapForDead(generations[g].old_blocks);
            processChainForDead(generations[g].large_objects);
        }
    }
}

/* --------------------------------------------------------------------------
 * Regard any closure in the current heap as dead or moribund and update
 * LDV statistics accordingly.
 * Called from shutdownHaskell() in RtsStartup.c.
 * Also, stops LDV profiling by resetting 'era' to 0.
 * ----------------------------------------------------------------------- */
void
LdvCensusKillAll( void )
{
    LdvCensusForDead(RtsFlags.GcFlags.generations - 1);
}

#endif /* PROFILING */
