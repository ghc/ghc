/* -----------------------------------------------------------------------------
 * $Id: LdvProfile.c,v 1.1 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifdef PROFILING

#include "Stg.h"
#include "Rts.h"
#include "LdvProfile.h"
#include "RtsFlags.h"
#include "Itimer.h"
#include "Proftimer.h"
#include "Profiling.h"
#include "Stats.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "Schedule.h"

/*
  ldvTime stores the current LDV time, that is, the current era.  It
  is one larger than the number of times LDV profiling has been
  performed, i.e.,
  ldvTime - 1 == the number of time LDV profiling was executed
              == the number of censuses made so far.
  RESTRICTION:
    ldvTime must be no longer than LDV_SHIFT (15 or 30) bits.
  Invariants:
    LDV profiling is turned off if ldvTime is 0.
    LDV profiling is turned on if ldvTime is > 0.
    ldvTime is initialized to 1 in initLdvProfiling().
    If LDV profiling is not performed, ldvTime must remain 0 (e.g., when we
    are doing retainer profiling).
  ldvTime is set to 1 in initLdvProfiling().
  ldvTime is set back to 0 in shutdownHaskell().
  In the meanwhile, ldvTime increments.
*/
nat ldvTime = 0;
#
// ldvTimeSave is set in LdvCensusKillAll(), and stores the final number of
// times that LDV profiling was proformed.
static nat ldvTimeSave;

// gi[] stores the statistics obtained at each heap census.
// gi[0] is not used. See initLdvProfiling().
LdvGenInfo *gi;

#define giINCREMENT   32      // allocation unit for gi[]
static nat giLength;          // current length of gi[]

// giMax is initialized to 2^LDV_SHIFT in initLdvProfiling().
// When ldvTime reaches giMax, the profiling stops because a closure can
// store only up to (giMax - 1) as its creation or last use time.
static nat giMax;

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
    if (ldvTime > 0) {
	StgInfoTable *inf = get_itbl((p));
	nat nw, i;
	switch (inf->type) {
	case THUNK_1_0:
	case THUNK_0_1:
	case THUNK_2_0:
	case THUNK_1_1:
	case THUNK_0_2:
	case THUNK_SELECTOR:
	    nw = MIN_UPD_SIZE;
	    break;
	case THUNK:
	    nw = inf->layout.payload.ptrs + inf->layout.payload.nptrs;
	    if (nw < MIN_UPD_SIZE)
		nw = MIN_UPD_SIZE;
	    break;
	case AP_UPD:
	    nw = sizeofW(StgPAP) - sizeofW(StgHeader) + ((StgPAP *)p)->n_args;
	    break;
	case CAF_BLACKHOLE:
	case BLACKHOLE:
	case SE_BLACKHOLE:
	case SE_CAF_BLACKHOLE:
	    nw = inf->layout.payload.ptrs + inf->layout.payload.nptrs;
	    break;
	default:
	    barf("Unexpected closure type %u in LDV_recordDead_FILL_SLOP_DYNAMIC()", inf->type);
	    break;
	}
	LDV_recordDead((StgClosure *)(p), nw + sizeofW(StgHeader));
	for (i = 0; i < nw; i++) {
	    ((StgClosure *)(p))->payload[i] = 0;
	}
    }
}

/* --------------------------------------------------------------------------
 * Initialize gi[ldvTime].
 * ----------------------------------------------------------------------- */
static inline void
giInitForCurrentEra(void)
{
    gi[ldvTime].notUsed = 0;
    gi[ldvTime].inherentlyUsed = 0;
    gi[ldvTime].used = 0;

    gi[ldvTime].voidNew = 0;
    gi[ldvTime].dragNew = 0;
}

/* --------------------------------------------------------------------------
 * Increases ldvTime by 1 and initialize gi[ldvTime].
 * Reallocates gi[] and increases its size if needed.
 * ----------------------------------------------------------------------- */
static void
incrementLdvTime( void )
{
    ldvTime++;

    if (ldvTime == giMax) {
	fprintf(stderr,
		"Lag/Drag/Void profiling limit %u reached. "
		"Please increase the profiling interval with -L option.\n",
		giLength);
	barf("Current profiling interval = %f seconds",
	     (float)RtsFlags.ProfFlags.profileInterval / 1000.0 );
    }

    if (ldvTime % giINCREMENT == 0) {
	gi = stgReallocBytes(gi, sizeof(LdvGenInfo) * (giLength + giINCREMENT),
                             "incrementLdvTime");
	giLength += giINCREMENT;
    }

    // What a stupid bug I struggled against for such a long time! I
    // placed giInitForCurrentEra() before the above rellocation part,
    // and it cost me three hours!
    giInitForCurrentEra();
}

/* --------------------------------------------------------------------------
 * Initialization code for LDV profiling.
 * ----------------------------------------------------------------------- */
void
initLdvProfiling( void )
{
    nat p;

    gi = stgMallocBytes(sizeof(LdvGenInfo) * giINCREMENT, "initLdvProfiling");
    giLength = giINCREMENT;

    ldvTime = 1;              // turn on LDV profiling.
    giInitForCurrentEra();

    // giMax = 2^LDV_SHIFT
    giMax = 1;
    for (p = 0; p < LDV_SHIFT; p++)
	giMax *= 2;
}

/* --------------------------------------------------------------------------
 * This function must be called before f-closing prof_file.
 * Still hp_file is open; see endHeapProfiling() in ProfHeap.c.
 * ----------------------------------------------------------------------- */
void
endLdvProfiling( void )
{
    nat t;
    int sumVoidNew, sumDragNew;

    // Now we compute voidTotal and dragTotal of each LdvGenInfo structure.
    sumVoidNew = 0;
    sumDragNew = 0;
    for (t = 0; t < ldvTimeSave; t++) {
	sumVoidNew += gi[t].voidNew;
	sumDragNew += gi[t].dragNew;
	gi[t].voidTotal = sumVoidNew;
	gi[t].dragTotal = sumDragNew;
    }

    // t = 0 is wrong (because ldvTime == 0 indicates LDV profiling is
    // turned off.
    for (t = 1;t < ldvTimeSave; t++) {
	fprintf(hp_file, "MARK %f\n", gi[t].time);
	fprintf(hp_file, "BEGIN_SAMPLE %f\n", gi[t].time);
	fprintf(hp_file, "VOID\t%u\n", gi[t].voidTotal * sizeof(StgWord));
	fprintf(hp_file, "LAG\t%u\n", (gi[t].notUsed - gi[t].voidTotal) * sizeof(StgWord));
	fprintf(hp_file, "USE\t%u\n", (gi[t].used - gi[t].dragTotal) * sizeof(StgWord));
	fprintf(hp_file, "INHERENT_USE\t%u\n", gi[t].inherentlyUsed * sizeof(StgWord));
	fprintf(hp_file, "DRAG\t%u\n", gi[t].dragTotal * sizeof(StgWord));
	fprintf(hp_file, "END_SAMPLE %f\n", gi[t].time);
    }
}

/* --------------------------------------------------------------------------
 * Print the statistics.
 * This function is called after each retainer profiling.
 * ----------------------------------------------------------------------- */
static void
outputLdvSet( void )
{
}

/* --------------------------------------------------------------------------
 * This function is eventually called on every object in the heap
 * during a census.  Any census is initiated immediately after a major
 * garbage collection, and we exploit this fact in the implementation.
 * If c is an 'inherently used' closure, gi[ldvTime].inherentlyUsed is
 * updated.  If c is an ordinary closure, either gi[ldvTime].notUsed or
 * gi[ldvTime].used is updated.
 * ----------------------------------------------------------------------- */
static inline nat
processHeapClosure(StgClosure *c)
{
    nat size;
    StgInfoTable *info;

    info = get_itbl(c);

    ASSERT(
	((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) <= ldvTime &&
	((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) > 0
        );
    ASSERT(
	((LDVW(c) & LDV_STATE_MASK) == LDV_STATE_CREATE) ||
	(
	    (LDVW(c) & LDV_LAST_MASK) <= ldvTime &&
	    (LDVW(c) & LDV_LAST_MASK) > 0
	    )
	);

    switch (info->type) {
	/*
	  'inherently used' cases: add to gi[ldvTime].inherentlyUsed
	*/

    case TSO:
	size = tso_sizeW((StgTSO *)c);
	goto inherently_used;

    case MVAR:
	size = sizeofW(StgMVar);
	goto inherently_used;

    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
	size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)c);
	goto inherently_used;

    case ARR_WORDS:
	size = arr_words_sizeW((StgArrWords *)c);
	goto inherently_used;

    case WEAK:
    case MUT_VAR:
    case MUT_CONS:
    case FOREIGN:
    case BCO:
    case STABLE_NAME:
	size = sizeW_fromITBL(info);
	goto inherently_used;

	/*
	  ordinary cases: add to gi[ldvTime].notUsed if c is not being used.
	  add to gi[ldvTime].used if c is being used.
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

    case AP_UPD:
    case PAP:
	size = pap_sizeW((StgPAP *)c);
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

    case BLACKHOLE_BQ:
    case BLACKHOLE:
    case SE_BLACKHOLE:
    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
	size = sizeW_fromITBL(info);
	break;

    case IND_PERM:
	size = sizeofW(StgInd);
	break;

    case IND_OLDGEN_PERM:
	size = sizeofW(StgIndOldGen);
	break;

	/*
	  Error case
	*/
    case IND:           // IND cannot appear after major GCs.
    case IND_OLDGEN:    // IND_OLDGEN cannot appear major GCs.
    case EVACUATED:     // EVACUATED is encountered only during GCs.
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
    case SEQ_FRAME:
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
	barf("Invalid object in processHeapClosure(): %d", info->type);
	return 0;
    }

    /*
       ordinary cases:
       We can compute either gi[ldvTime].notUsed or gi[ldvTime].used; the other
       can be computed from the total sum of costs.
       At the moment, we choose to compute gi[ldvTime].notUsed, which seems to
       be smaller than gi[ldvTime].used.
    */

    // ignore closures that don't satisfy our constraints.
    if (closureSatisfiesConstraints(c)) {
	if ((LDVW(c) & LDV_STATE_MASK) == LDV_STATE_CREATE)
	    gi[ldvTime].notUsed += size - sizeofW(StgProfHeader);
	else
	    gi[ldvTime].used += size - sizeofW(StgProfHeader);
    }
    return size;

inherently_used:
    // ignore closures that don't satisfy our constraints.
    if (closureSatisfiesConstraints(c)) {
	gi[ldvTime].inherentlyUsed += size - sizeofW(StgProfHeader);
    }
    return size;
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosure() on every closure in the heap blocks
 * begining at bd during a census.
 * ----------------------------------------------------------------------- */
static void
processHeap( bdescr *bd )
{
    StgPtr p;
    nat size;

    while (bd != NULL) {
	p = bd->start;
	while (p < bd->free) {
	    size = processHeapClosure((StgClosure *)p);
	    p += size;
	    while (p < bd->free && !*p)   // skip slop
		p++;
	}
	ASSERT(p == bd->free);
	bd = bd->link;
    }
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosure() on every closure in the small object pool
 * during a census.
 * ----------------------------------------------------------------------- */
static void
processSmallObjectPool( void )
{
    bdescr *bd;
    StgPtr p;
    nat size;

    bd = small_alloc_list;

    // first block
    if (bd == NULL)
	return;

    p = bd->start;
    while (p < alloc_Hp) {
	size = processHeapClosure((StgClosure *)p);
	p += size;
	while (p < alloc_Hp && !*p)     // skip slop
	    p++;
    }
    ASSERT(p == alloc_Hp);

    bd = bd->link;
    while (bd != NULL) {
	p = bd->start;
	while (p < bd->free) {
	    size = processHeapClosure((StgClosure *)p);
	    p += size;
	    while (p < bd->free && !*p)    // skip slop
		p++;
	}
	ASSERT(p == bd->free);
	bd = bd->link;
    }
}

/* --------------------------------------------------------------------------
 * Calls processHeapClosure() on every (large) closure in the object
 * chain beginning at bd during a census.
 * ----------------------------------------------------------------------- */
static void
processChain( bdescr *bd )
{
    while (bd != NULL) {
	// bd->free - bd->start is not an accurate measurement of the
	// object size.  Actually it is always zero, so we compute its
	// size explicitly.
	processHeapClosure((StgClosure *)bd->start);
	bd = bd->link;
  }
}

/* --------------------------------------------------------------------------
 * Starts a census for LDV profiling.
 * Invariants:
 *   Any call to LdvCensus() is preceded by a major garbage collection.
 * ----------------------------------------------------------------------- */
void
LdvCensus( void )
{
    nat g, s;

    // ldvTime == 0 means that LDV profiling is currently turned off.
    if (ldvTime == 0)
	return;

    stat_startLDV();
    //
    // Todo: when we perform LDV profiling, the Haskell mutator time seems to
    //       be affected by -S or -s runtime option. For instance, the
    //       following two options should result in nearly same
    //       profiling outputs, but the second run (without -Sstderr
    //       option) spends almost twice as long in the Haskell
    //       mutator as the first run:
    //
    //       1) +RTS -Sstderr -hL -RTS
    //       2) +RTS -hL -RTS
    //
    //       This is quite a subtle bug because this wierd phenomenon is not
    //       observed in retainer profiling, yet mut_user_time_during_LDV() is
    //       completely orthogonal to mut_user_time_during_RP(). However, the
    //       overall shapes of the resultant graphs are almost the same.
    //
    gi[ldvTime].time = mut_user_time_during_LDV();
    if (RtsFlags.GcFlags.generations == 1) {
	//
	// Todo: support LDV for two-space garbage collection.
	//
	barf("Lag/Drag/Void profiling not supported with -G1");
    } else {
	for (g = 0; g < RtsFlags.GcFlags.generations; g++)
	    for (s = 0; s < generations[g].n_steps; s++) {
		if (g == 0 && s == 0) {
		    // after a major GC, the nursery must be empty,
		    // and no need to call processNursery().
		    ASSERT(MainCapability.r.rNursery->start ==
			   MainCapability.r.rNursery->free);
		    processSmallObjectPool();
		    processChain(generations[g].steps[s].large_objects);
		} else{
		    processHeap(generations[g].steps[s].blocks);
		    processChain(generations[g].steps[s].large_objects);
		}
	    }
    }
    outputLdvSet();   // output to hp_file
    stat_endLDV();    // output to prof_file

    incrementLdvTime();
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
static inline nat
processHeapClosureForDead( StgClosure *c )
{
    nat size;
    StgInfoTable *info;

    info = get_itbl(c);

    if (info->type != EVACUATED) {
	ASSERT(((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) <= ldvTime &&
	       ((LDVW(c) & LDV_CREATE_MASK) >> LDV_SHIFT) > 0);
	ASSERT(((LDVW(c) & LDV_STATE_MASK) == LDV_STATE_CREATE) ||
	       (
		   (LDVW(c) & LDV_LAST_MASK) <= ldvTime &&
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
    case MUT_CONS:
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

    case AP_UPD:
    case PAP:
	size = pap_sizeW((StgPAP *)c);
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

    case BLACKHOLE_BQ:
    case BLACKHOLE:
    case SE_BLACKHOLE:
    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
	size = sizeW_fromITBL(info);
	break;

    case IND_PERM:
	size = sizeofW(StgInd);
	break;

    case IND_OLDGEN_PERM:
	size = sizeofW(StgIndOldGen);
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
	size = sizeofW(StgInd);
	return size;

    case IND_OLDGEN:
	size = sizeofW(StgIndOldGen);
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
    case SEQ_FRAME:
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
    if (ldvTime == 0)
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

    // record the time when LDV profiling stops.
    ldvTimeSave = ldvTime;

    // and, stops LDV profiling.
    ldvTime = 0;
}

#endif /* PROFILING */
