/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifdef PROFILING

#include "Rts.h"
#include "LdvProfile.h"
#include "RtsFlags.h"
#include "Profiling.h"
#include "Stats.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "Schedule.h"

/* --------------------------------------------------------------------------
 * Fills in the slop when a *dynamic* closure changes its type.
 * First calls LDV_recordDead() to declare the closure is dead, and then
 * fills in the slop.
 * 
 *  Invoked when:
 *    1) blackholing, UPD_BH_UPDATABLE() and UPD_BH_SINGLE_ENTRY (in
 * 	 includes/StgMacros.h), threadLazyBlackHole() and 
 * 	 threadSqueezeStack() (in GC.c).
 *    2) updating with indirection closures, updateWithIndirection() 
 * 	 and updateWithPermIndirection() (in Storage.h).
 * 
 *  LDV_recordDead_FILL_SLOP_DYNAMIC() is not called on 'inherently used' 
 *  closures such as TSO. It is not called on PAP because PAP is not updatable.
 *  ----------------------------------------------------------------------- */
void 
LDV_recordDead_FILL_SLOP_DYNAMIC( StgClosure *p )
{
    StgInfoTable *info;
    nat nw, i;

#if defined(__GNUC__) && __GNUC__ < 3 && defined(DEBUG)
#error Please use gcc 3.0+ to compile this file with DEBUG; gcc < 3.0 miscompiles it
#endif

    if (era > 0) {
	info = get_itbl((p));
	switch (info->type) {
	case THUNK_1_0:
	case THUNK_0_1:
	case THUNK_2_0:
	case THUNK_1_1:
	case THUNK_0_2:
	case THUNK_SELECTOR:
	    nw = MIN_UPD_SIZE;
	    break;
	case THUNK:
	    nw = info->layout.payload.ptrs + info->layout.payload.nptrs;
	    if (nw < MIN_UPD_SIZE)
		nw = MIN_UPD_SIZE;
	    break;
	case AP:
	    nw = sizeofW(StgPAP) - sizeofW(StgHeader) + ((StgPAP *)p)->n_args;
	    break;
	case AP_STACK:
	    nw = sizeofW(StgAP_STACK) - sizeofW(StgHeader)
		+ ((StgAP_STACK *)p)->size;
	    break;
	case CAF_BLACKHOLE:
	case BLACKHOLE:
	case SE_BLACKHOLE:
	case SE_CAF_BLACKHOLE:
	    nw = info->layout.payload.ptrs + info->layout.payload.nptrs;
	    break;
	default:
	    barf("Unexpected closure type %u in LDV_recordDead_FILL_SLOP_DYNAMIC()", info->type);
	    break;
	}
	LDV_recordDead((StgClosure *)(p), nw + sizeofW(StgHeader));
	for (i = 0; i < nw; i++) {
	    ((StgClosure *)(p))->payload[i] = 0;
	}
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
STATIC_INLINE nat
processHeapClosureForDead( StgClosure *c )
{
    nat size;
    StgInfoTable *info;

    info = get_itbl(c);

    if (info->type != EVACUATED) {
	ASSERT(((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) <= era &&
	       ((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) > 0);
	ASSERT(((LDVW(c) & LDV_STATE_MASK) == LDV_STATE_CREATE) ||
	       (
		   (LDVW(c) & LDV_LAST_MASK) <= era &&
		   (LDVW(c) & LDV_LAST_MASK) > 0
		   ));
    }

    switch (info->type) {
	/*
	  'inherently used' cases: do nothing.
	*/

    case TSO:
	size = tso_sizeW((StgTSO *)c);
	return size;

    case MVAR:
	size = sizeofW(StgMVar);
	return size;

    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
	size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)c);
	return size;

    case ARR_WORDS:
	size = arr_words_sizeW((StgArrWords *)c);
	return size;

    case WEAK:
    case MUT_VAR:
    case FOREIGN:
    case BCO:
    case STABLE_NAME:
	size = sizeW_fromITBL(info);
	return size;

	/*
	  ordinary cases: call LDV_recordDead().
	*/

    case THUNK:
	size = stg_max(sizeW_fromITBL(info), sizeofW(StgHeader) + MIN_UPD_SIZE);
	break;

    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_2_0:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_SELECTOR:
	size = sizeofW(StgHeader) + MIN_UPD_SIZE;
	break;

    case AP:
    case PAP:
	size = pap_sizeW((StgPAP *)c);
	break;

    case AP_STACK:
	size = ap_stack_sizeW((StgAP_STACK *)c);
	break;

    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:

    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_2_0:
    case FUN_1_1:
    case FUN_0_2:

    case BLACKHOLE:
    case SE_BLACKHOLE:
    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
	size = sizeW_fromITBL(info);
	break;

    case IND_PERM:
    case IND_OLDGEN_PERM:
	size = sizeofW(StgInd);
	break;

	/*
	  'Ingore' cases
	*/
	// Why can we ignore IND/IND_OLDGEN closures? We assume that
	// any census is preceded by a major garbage collection, which
	// IND/IND_OLDGEN closures cannot survive. Therefore, it is no
	// use considering IND/IND_OLDGEN closures in the meanwhile
	// because they will perish before the next census at any
	// rate.
    case IND:
    case IND_OLDGEN:
	size = sizeofW(StgInd);
	return size;

    case EVACUATED:
	// The size of the evacuated closure is currently stored in
	// the LDV field.  See SET_EVACUAEE_FOR_LDV() in
	// includes/StgLdvProf.h.
	return LDVW(c);

	/*
	  Error case
	*/
	// static objects
    case IND_STATIC:
    case CONSTR_STATIC:
    case FUN_STATIC:
    case THUNK_STATIC:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_NOCAF_STATIC:
	// stack objects
    case UPDATE_FRAME:
    case CATCH_FRAME:
    case STOP_FRAME:
    case RET_DYN:
    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
    case RET_BIG:
    case RET_VEC_BIG:
	// others
    case BLOCKED_FETCH:
    case FETCH_ME:
    case FETCH_ME_BQ:
    case RBH:
    case REMOTE_REF:
    case INVALID_OBJECT:
    default:
	barf("Invalid object in processHeapClosureForDead(): %d", info->type);
	return 0;
    }

    // Found a dead closure: record its size
    LDV_recordDead(c, size);
    return size;
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosureForDead() on every *dead* closures in the
 * heap blocks starting at bd.
 * ----------------------------------------------------------------------- */
static void
processHeapForDead( bdescr *bd )
{
    StgPtr p;

    while (bd != NULL) {
	p = bd->start;
	while (p < bd->free) {
	    p += processHeapClosureForDead((StgClosure *)p);
	    while (p < bd->free && !*p)   // skip slop
		p++;
	}
	ASSERT(p == bd->free);
	bd = bd->link;
    }
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosureForDead() on every *dead* closures in the nursery.
 * ----------------------------------------------------------------------- */
static void
processNurseryForDead( void )
{
    StgPtr p, bdLimit;
    bdescr *bd;

    bd = MainCapability.r.rNursery;
    while (bd->start < bd->free) {
	p = bd->start;
	bdLimit = bd->start + BLOCK_SIZE_W;
	while (p < bd->free && p < bdLimit) {
	    p += processHeapClosureForDead((StgClosure *)p);
	    while (p < bd->free && p < bdLimit && !*p)  // skip slop
		p++;
	}
	bd = bd->link;
	if (bd == NULL)
	    break;
    }
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosureForDead() on every *dead* closures in the
 * small object pool.
 * ----------------------------------------------------------------------- */
static void
processSmallObjectPoolForDead( void )
{
    bdescr *bd;
    StgPtr p;

    bd = small_alloc_list;

    // first block
    if (bd == NULL)
	return;

    p = bd->start;
    while (p < alloc_Hp) {
	p += processHeapClosureForDead((StgClosure *)p);
	while (p < alloc_Hp && !*p)     // skip slop
	    p++;
    }
    ASSERT(p == alloc_Hp);

    bd = bd->link;
    while (bd != NULL) {
	p = bd->start;
	while (p < bd->free) {
	    p += processHeapClosureForDead((StgClosure *)p);
	    while (p < bd->free && !*p)    // skip slop
		p++;
	}
	ASSERT(p == bd->free);
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
	processHeapClosureForDead((StgClosure *)bd->start);
	bd = bd->link;
    }
}

/* --------------------------------------------------------------------------
 * Start a census for *dead* closures, and calls
 * processHeapClosureForDead() on every closure which died in the
 * current garbage collection.  This function is called from a garbage
 * collector right before tidying up, when all dead closures are still
 * stored in the heap and easy to identify.  Generations 0 through N
 * have just beed garbage collected.
 * ----------------------------------------------------------------------- */
void
LdvCensusForDead( nat N )
{
    nat g, s;

    // ldvTime == 0 means that LDV profiling is currently turned off.
    if (era == 0)
	return;

    if (RtsFlags.GcFlags.generations == 1) {
	//
	// Todo: support LDV for two-space garbage collection.
	//
	barf("Lag/Drag/Void profiling not supported with -G1");
    } else {
	for (g = 0; g <= N; g++)
	    for (s = 0; s < generations[g].n_steps; s++) {
		if (g == 0 && s == 0) {
		    processSmallObjectPoolForDead();
		    processNurseryForDead();
		    processChainForDead(generations[g].steps[s].large_objects);
		} else{
		    processHeapForDead(generations[g].steps[s].blocks);
		    processChainForDead(generations[g].steps[s].large_objects);
		}
	    }
    }
}

/* --------------------------------------------------------------------------
 * Regard any closure in the current heap as dead or moribund and update
 * LDV statistics accordingly.
 * Called from shutdownHaskell() in RtsStartup.c.
 * Also, stops LDV profiling by resetting ldvTime to 0.
 * ----------------------------------------------------------------------- */
void
LdvCensusKillAll( void )
{
    LdvCensusForDead(RtsFlags.GcFlags.generations - 1);
}

#endif /* PROFILING */
