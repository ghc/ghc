/* -----------------------------------------------------------------------------
 * $Id: ProfHeap.c,v 1.28 2001/11/27 15:30:06 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#if defined(DEBUG) && !defined(PROFILING)
#define DEBUG_HEAP_PROF
#else
#undef DEBUG_HEAP_PROF
#endif

#if defined(PROFILING) || defined(DEBUG_HEAP_PROF)

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Profiling.h"
#include "Storage.h"
#include "ProfHeap.h"
#include "Stats.h"
#include "Hash.h"
#include "StrHash.h"
#include "RetainerProfile.h"
#include "LdvProfile.h"
#include "Arena.h"

#ifdef DEBUG_HEAP_PROF
#include "Printer.h"
static void fprint_data(FILE *fp);
#endif

/* -----------------------------------------------------------------------------
 * era stores the current time period.  It is the same as the
 * number of censuses that have been performed.
 *
 * RESTRICTION:
 *   era must be no longer than LDV_SHIFT (15 or 30) bits.
 * Invariants:
 *   era is initialized to 0 in initHeapProfiling().
 *
 * max_era is initialized to 2^LDV_SHIFT in initHeapProfiling().
 * When era reaches max_era, the profiling stops because a closure can
 * store only up to (max_era - 1) as its creation or last use time.
 * -------------------------------------------------------------------------- */
nat era;
static nat max_era;

/* -----------------------------------------------------------------------------
   counters
   -------------------------------------------------------------------------- */
typedef struct _counter {
    void *identity;
    union {
	nat resid;
	struct {
	    int prim;     // total size of 'inherently used' closures
	    int unused;   // total size of 'never used' closures
	    int used;     // total size of 'used at least once' closures
	    int void_new;  // current total size of 'destroyed without being used' closures
	    int drag_new;  // current total size of 'used at least once and waiting to die'
	} ldv;
    } c;
    struct _counter *next;
} counter;

typedef struct {
    double      time;    // the time in MUT time when the census is made
    HashTable * hash;
    counter   * ctrs;
    Arena     * arena;

    // for LDV profiling, when just displaying by LDV
    int       prim;
    int       not_used;
    int       used;
    int       void_total;
    int       drag_total;
} Census;

Census *censuses = NULL;
nat n_censuses = 0;

/* --------------------------------------------------------------------------
 * Profiling type predicates
 * ----------------------------------------------------------------------- */
#ifdef PROFILING
static inline rtsBool
doingLDVProfiling( void )
{
    return (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV 
	    || RtsFlags.ProfFlags.bioSelector != NULL);
}

static inline rtsBool
doingRetainerProfiling( void )
{
    return (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER
	    || RtsFlags.ProfFlags.retainerSelector != NULL);
}
#endif // PROFILING

// Precesses a closure 'c' being destroyed whose size is 'size'.
// Make sure that LDV_recordDead() is not invoked on 'inherently used' closures
// such as TSO; they should not be involved in computing dragNew or voidNew.
// 
// Even though era is checked in both LdvCensusForDead() and 
// LdvCensusKillAll(), we still need to make sure that era is > 0 because 
// LDV_recordDead() may be called from elsewhere in the runtime system. E.g., 
// when a thunk is replaced by an indirection object.

#ifdef PROFILING
void
LDV_recordDead( StgClosure *c, nat size )
{
    if (era > 0 && closureSatisfiesConstraints(c)) {
	nat t;
	size -= sizeofW(StgProfHeader);
	if ((LDVW((c)) & LDV_STATE_MASK) == LDV_STATE_CREATE) {
	    t = (LDVW((c)) & LDV_CREATE_MASK) >> LDV_SHIFT;
	    if (t < era) {
		censuses[t].void_total   += (int)size;
		censuses[era].void_total -= (int)size;
	    }
	} else {
	    t = LDVW((c)) & LDV_LAST_MASK;
	    if (t + 1 < era) {
		censuses[t + 1].drag_total += size;
		censuses[era].drag_total   -= size;
	    }
	}
    }
}
#endif

/* --------------------------------------------------------------------------
 * Initialize censuses[era];
 * ----------------------------------------------------------------------- */
static inline void
initEra(void)
{
    censuses[era].not_used = 0;
    censuses[era].used     = 0;
    censuses[era].prim     = 0;
    censuses[era].void_total = 0;
    censuses[era].drag_total = 0;
}

/* --------------------------------------------------------------------------
 * Increases era by 1 and initialize census[era].
 * Reallocates gi[] and increases its size if needed.
 * ----------------------------------------------------------------------- */
static void
nextEra( void )
{
#ifdef PROFILING
    if (doingLDVProfiling()) { 
	era++;

	if (era == max_era) {
	    barf("maximum number of censuses reached; use +RTS -i to reduce");
	}
	
	if (era == n_censuses) {
	    n_censuses *= 2;
	    censuses = stgReallocBytes(censuses, sizeof(Census) * n_censuses,
				       "nextEra");
	}
    }
#endif // PROFILING
	
    initEra();
}

/* -------------------------------------------------------------------------- */

#ifdef DEBUG_HEAP_PROF
FILE *hp_file;

void initProfiling1( void )
{
}

void initProfiling2( void )
{
  initHeapProfiling();
}

void endProfiling( void )
{
  endHeapProfiling();
}
#endif /* DEBUG_HEAP_PROF */

nat
initHeapProfiling(void)
{
    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return 0;
    }

    // we only count eras if we're doing LDV profiling.  Otherwise era
    // is fixed at zero.
#ifdef PROFILING
    if (doingLDVProfiling()) {
	era = 1;
    } else
#endif
    {
	era = 0;
    }

    {   // max_era = 2^LDV_SHIFT
	nat p;
	max_era = 1;
	for (p = 0; p < LDV_SHIFT; p++)
	    max_era *= 2;
    }

    n_censuses = 32;
    censuses = stgMallocBytes(sizeof(Census) * n_censuses, "initHeapProfiling");

    fprintf(hp_file, "JOB \"%s", prog_argv[0]);

#ifdef PROFILING
    {
	int count;
	for(count = 1; count < prog_argc; count++)
	    fprintf(hp_file, " %s", prog_argv[count]);
	fprintf(hp_file, " +RTS ");
	for(count = 0; count < rts_argc; count++)
	    fprintf(hp_file, "%s ", rts_argv[count]);
	fprintf(hp_file, "\n");
    }
#endif /* PROFILING */

    fprintf(hp_file, "\"\n" );

    fprintf(hp_file, "DATE \"%s\"\n", time_str());

    fprintf(hp_file, "SAMPLE_UNIT \"seconds\"\n");
    fprintf(hp_file, "VALUE_UNIT \"bytes\"\n");

    fprintf(hp_file, "BEGIN_SAMPLE 0.00\n");
    fprintf(hp_file, "END_SAMPLE 0.00\n");

#ifdef DEBUG_HEAP_PROF
    DEBUG_LoadSymbols(prog_argv[0]);
#endif

#ifdef PROFILING
    if (doingRetainerProfiling()) {
	initRetainerProfiling();
    }
#endif

    return 0;
}

void
endHeapProfiling(void)
{
    StgDouble seconds;

    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return;
    }

#ifdef PROFILING
    if (doingRetainerProfiling()) {
	endRetainerProfiling();
    }
#endif

#ifdef PROFILING
  // Note: 
  //   We do not need to perform a major garbage collection because all the
  //   closures created since the last census will not affect the profiling
  //   statistics anyhow.
  if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) 
    LdvCensusKillAll();
#endif

#ifdef PROFILING
    // At last... we can output the census info for LDV profiling
    if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) {
	nat t;
	int sumVoidNew, sumDragNew;

	// Now we compute void_total and drag_total for each census
	sumVoidNew = 0;
	sumDragNew = 0;
	for (t = 1; t < era; t++) { // note: start at 1, not 0
	    sumVoidNew += censuses[t].void_total;
	    sumDragNew += censuses[t].drag_total;
	    censuses[t].void_total = sumVoidNew;
	    censuses[t].drag_total = sumDragNew;
	    ASSERT( censuses[t].void_total < censuses[t].not_used );
	    ASSERT( censuses[t].drag_total < censuses[t].used );
	}
	
	for (t = 1; t < era; t++) { // note: start at 1, not 0
	    fprintf(hp_file, "MARK %f\n", censuses[t].time);
	    fprintf(hp_file, "BEGIN_SAMPLE %f\n", censuses[t].time);
	    fprintf(hp_file, "VOID\t%u\n", censuses[t].void_total * sizeof(W_));
	    fprintf(hp_file, "LAG\t%u\n", 
		    (censuses[t].not_used - censuses[t].void_total) * sizeof(W_));
	    fprintf(hp_file, "USE\t%u\n", 
		    (censuses[t].used - censuses[t].drag_total) * sizeof(W_));
	    fprintf(hp_file, "INHERENT_USE\t%u\n", 
		    censuses[t].prim * sizeof(W_));
	    fprintf(hp_file, "DRAG\t%u\n", censuses[t].drag_total * sizeof(W_));
	    fprintf(hp_file, "END_SAMPLE %f\n", censuses[t].time);
	}
    }
#endif

    seconds = mut_user_time();
    fprintf(hp_file, "BEGIN_SAMPLE %0.2f\n", seconds);
    fprintf(hp_file, "END_SAMPLE %0.2f\n", seconds);
    fclose(hp_file);
}

#ifdef DEBUG_HEAP_PROF
/* -----------------------------------------------------------------------------
   Closure Type Profiling;

   PROBABLY TOTALLY OUT OF DATE -- ToDo (SDM)
   -------------------------------------------------------------------------- */

static char *type_names[] = {
      "INVALID_OBJECT"
    , "CONSTR"
    , "CONSTR_INTLIKE"
    , "CONSTR_CHARLIKE"
    , "CONSTR_STATIC"
    , "CONSTR_NOCAF_STATIC"

    , "FUN"
    , "FUN_STATIC"

    , "THUNK"
    , "THUNK_STATIC"
    , "THUNK_SELECTOR"

    , "BCO"
    , "AP_UPD"

    , "PAP"

    , "IND"
    , "IND_OLDGEN"
    , "IND_PERM"
    , "IND_OLDGEN_PERM"
    , "IND_STATIC"

    , "RET_BCO"
    , "RET_SMALL"
    , "RET_VEC_SMALL"
    , "RET_BIG"
    , "RET_VEC_BIG"
    , "RET_DYN"
    , "UPDATE_FRAME"
    , "CATCH_FRAME"
    , "STOP_FRAME"
    , "SEQ_FRAME"

    , "BLACKHOLE"
    , "BLACKHOLE_BQ"
    , "MVAR"

    , "ARR_WORDS"

    , "MUT_ARR_PTRS"
    , "MUT_ARR_PTRS_FROZEN"
    , "MUT_VAR"

    , "WEAK"
    , "FOREIGN"
  
    , "TSO"

    , "BLOCKED_FETCH"
    , "FETCH_ME"

    , "EVACUATED"
};

#endif /* DEBUG_HEAP_PROF */


#ifdef PROFILING
static void
fprint_ccs(FILE *fp, CostCentreStack *ccs, nat max_length)
{
    char buf[max_length+1];
    nat next_offset = 0;
    nat written;
    char *template;

    // MAIN on its own gets printed as "MAIN", otherwise we ignore MAIN.
    if (ccs == CCS_MAIN) {
	fprintf(fp, "MAIN");
	return;
    }

    // keep printing components of the stack until we run out of space
    // in the buffer.  If we run out of space, end with "...".
    for (; ccs != NULL && ccs != CCS_MAIN; ccs = ccs->prevStack) {

	// CAF cost centres print as M.CAF, but we leave the module
	// name out of all the others to save space.
	if (!strcmp(ccs->cc->label,"CAF")) {
	    written = snprintf(buf+next_offset, 
			       (int)max_length-3-(int)next_offset,
			       "%s.CAF", ccs->cc->module);
	} else {
	    if (ccs->prevStack != NULL && ccs->prevStack != CCS_MAIN) {
		template = "%s/";
	    } else {
		template = "%s";
	    }
	    written = snprintf(buf+next_offset, 
			       (int)max_length-3-(int)next_offset,
			       template, ccs->cc->label);
	}

	if (next_offset+written >= max_length-4) {
	    sprintf(buf+max_length-4, "...");
	    break;
	} else {
	    next_offset += written;
	}
    }
    fprintf(fp, "%s", buf);
}

static rtsBool
str_matches_selector( char* str, char* sel )
{
   char* p;
   // fprintf(stderr, "str_matches_selector %s %s\n", str, sel);
   while (1) {
       // Compare str against wherever we've got to in sel.
       p = str;
       while (*p != '\0' && *sel != ',' && *sel != '\0' && *p == *sel) {
	   p++; sel++;
       }
       // Match if all of str used and have reached the end of a sel fragment.
       if (*p == '\0' && (*sel == ',' || *sel == '\0'))
	   return rtsTrue;
       
       // No match.  Advance sel to the start of the next elem.
       while (*sel != ',' && *sel != '\0') sel++;
       if (*sel == ',') sel++;
       
       /* Run out of sel ?? */
       if (*sel == '\0') return rtsFalse;
   }
}

// Figure out whether a closure should be counted in this census, by
// testing against all the specified constraints.
rtsBool
closureSatisfiesConstraints( StgClosure* p )
{
   rtsBool b;
   if (RtsFlags.ProfFlags.modSelector) {
       b = str_matches_selector( ((StgClosure *)p)->header.prof.ccs->cc->module,
				 RtsFlags.ProfFlags.modSelector );
       if (!b) return rtsFalse;
   }
   if (RtsFlags.ProfFlags.descrSelector) {
       b = str_matches_selector( (get_itbl((StgClosure *)p))->prof.closure_desc,
				 RtsFlags.ProfFlags.descrSelector );
       if (!b) return rtsFalse;
   }
   if (RtsFlags.ProfFlags.typeSelector) {
       b = str_matches_selector( (get_itbl((StgClosure *)p))->prof.closure_type,
                                RtsFlags.ProfFlags.typeSelector );
       if (!b) return rtsFalse;
   }
   if (RtsFlags.ProfFlags.ccSelector) {
       b = str_matches_selector( ((StgClosure *)p)->header.prof.ccs->cc->label,
				 RtsFlags.ProfFlags.ccSelector );
       if (!b) return rtsFalse;
   }
   if (RtsFlags.ProfFlags.retainerSelector) {
       RetainerSet *rs;
       nat i;
       rs = retainerSetOf((StgClosure *)p);
       if (rs != NULL) {
	   for (i = 0; i < rs->num; i++) {
	       b = str_matches_selector( rs->element[i]->cc->label,
					 RtsFlags.ProfFlags.retainerSelector );
	       if (b) return rtsTrue;
	   }
       }
       return rtsFalse;
   }
   return rtsTrue;
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
 * Print out the results of a heap census.
 * -------------------------------------------------------------------------- */
static void
dumpCensus( Census *census )
{
    counter *ctr;

#ifdef PROFILING
    // We can't generate any info for LDV profiling until
    // the end of the run...
    if (doingLDVProfiling()) { return; }
#endif

    fprintf(hp_file, "BEGIN_SAMPLE %0.2f\n", census->time);

    for (ctr = census->ctrs; ctr != NULL; ctr = ctr->next) {

#ifdef DEBUG_HEAP_PROF
	switch (RtsFlags.ProfFlags.doHeapProfile) {
	case HEAP_BY_INFOPTR:
	    fprint_data(hp_file);
	    break;
	case HEAP_BY_CLOSURE_TYPE:
	    fprint_closure_types(hp_file);
	    break;
	}
#endif
	
#ifdef PROFILING
	switch (RtsFlags.ProfFlags.doHeapProfile) {
	case HEAP_BY_CCS:
	    fprint_ccs(hp_file, (CostCentreStack *)ctr->identity, 30);
	    break;
	case HEAP_BY_MOD:
	case HEAP_BY_DESCR:
	case HEAP_BY_TYPE:
	    fprintf(hp_file, "%s", (char *)ctr->identity);
	    break;
	case HEAP_BY_RETAINER:
	{
	    RetainerSet *rs = (RetainerSet *)ctr->identity;

	    // it might be the distinguished retainer set rs_MANY:
	    if (rs == &rs_MANY) {
		fprintf(hp_file, "MANY");
		break;
	    }

	    // Mark this retainer set by negating its id, because it
	    // has appeared in at least one census.  We print the
	    // values of all such retainer sets into the log file at
	    // the end.  A retainer set may exist but not feature in
	    // any censuses if it arose as the intermediate retainer
	    // set for some closure during retainer set calculation.
	    if (rs->id > 0)
		rs->id = -(rs->id);

	    // report in the unit of bytes: * sizeof(StgWord)
	    printRetainerSetShort(hp_file, rs);
	    break;
	}
	default:
	    barf("dumpCensus; doHeapProfile");
	}
#endif

	fprintf(hp_file, "\t%d\n", ctr->c.resid * sizeof(W_));
    }

    fprintf(hp_file, "END_SAMPLE %0.2f\n", census->time);
}

/* -----------------------------------------------------------------------------
 * Code to perform a heap census.
 * -------------------------------------------------------------------------- */
static void
heapCensusChain( Census *census, bdescr *bd )
{
    StgPtr p;
    StgInfoTable *info;
    void *identity;
    nat size;
    counter *ctr;
    nat real_size;
    rtsBool prim;

    for (; bd != NULL; bd = bd->link) {
	p = bd->start;
	while (p < bd->free) {
	    info = get_itbl((StgClosure *)p);
	    prim = rtsFalse;
	    
	    switch (info->type) {

	    case CONSTR:
	    case FUN:
	    case THUNK:
	    case IND_PERM:
	    case IND_OLDGEN_PERM:
	    case CAF_BLACKHOLE:
	    case SE_CAF_BLACKHOLE:
	    case SE_BLACKHOLE:
	    case BLACKHOLE:
	    case BLACKHOLE_BQ:
	    case CONSTR_INTLIKE:
	    case CONSTR_CHARLIKE:
	    case FUN_1_0:
	    case FUN_0_1:
	    case FUN_1_1:
	    case FUN_0_2:
	    case FUN_2_0:
	    case THUNK_1_1:
	    case THUNK_0_2:
	    case THUNK_2_0:
	    case CONSTR_1_0:
	    case CONSTR_0_1:
	    case CONSTR_1_1:
	    case CONSTR_0_2:
	    case CONSTR_2_0:
		size = sizeW_fromITBL(info);
		break;
		
	    case BCO:
	    case MVAR:
	    case WEAK:
	    case FOREIGN:
	    case STABLE_NAME:
	    case MUT_VAR:
	    case MUT_CONS:
		prim = rtsTrue;
		size = sizeW_fromITBL(info);
		break;

	    case THUNK_1_0:		/* ToDo - shouldn't be here */
	    case THUNK_0_1:		/* "  ditto  " */
	    case THUNK_SELECTOR:
		size = sizeofW(StgHeader) + MIN_UPD_SIZE;
		break;

	    case PAP:
	    case AP_UPD:
		size = pap_sizeW((StgPAP *)p);
		break;
		
	    case ARR_WORDS:
		prim = rtsTrue;
		size = arr_words_sizeW(stgCast(StgArrWords*,p));
		break;
		
	    case MUT_ARR_PTRS:
	    case MUT_ARR_PTRS_FROZEN:
		prim = rtsTrue;
		size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)p);
		break;
		
	    case TSO:
		prim = rtsTrue;
		size = tso_sizeW((StgTSO *)p);
		break;
		
	    default:
		barf("heapCensus");
	    }
	    
	    identity = NULL;

#ifdef DEBUG_HEAP_PROF
	    real_size = size;
	    switch (RtsFlags.ProfFlags.doHeapProfile) {
	    case HEAP_BY_INFOPTR:
		identity = (void *)((StgClosure *)p)->header.info; 
		break;
	    case HEAP_BY_CLOSURE_TYPE:
		identity = type_names[info->type];
		break;
	    default:
		barf("heapCensus; doHeapProfile");
	    }
#endif
	    
#ifdef PROFILING
	    // subtract the profiling overhead
	    real_size = size - sizeofW(StgProfHeader);

	    if (closureSatisfiesConstraints((StgClosure*)p)) {
		switch (RtsFlags.ProfFlags.doHeapProfile) {
		case HEAP_BY_CCS:
		    identity = ((StgClosure *)p)->header.prof.ccs;
		    break;
		case HEAP_BY_MOD:
		    identity = ((StgClosure *)p)->header.prof.ccs->cc->module;
		    break;
		case HEAP_BY_DESCR:
		    identity = (get_itbl((StgClosure *)p))->prof.closure_desc;
		    break;
		case HEAP_BY_TYPE:
		    identity = (get_itbl((StgClosure *)p))->prof.closure_type;
		    break;
		case HEAP_BY_RETAINER:
		    identity = retainerSetOf((StgClosure *)p);
		    break;
		case HEAP_BY_LDV:
		    if (prim)
			census->prim += real_size;
		    else if ((LDVW(p) & LDV_STATE_MASK) == LDV_STATE_CREATE)
			census->not_used += real_size;
		    else
			census->used += real_size;
		    // NOTE: don't break here.  We're not using the
		    // hash table.
		    p += size;
		    continue;
		default:
		    barf("heapCensus; doHeapProfile");
		}
	    }
#endif

	    if (identity != NULL) {
		ctr = lookupHashTable( census->hash, (StgWord)identity );
		if (ctr != NULL) {
		    ctr->c.resid += real_size;
		} else {
		    ctr = arenaAlloc( census->arena, sizeof(counter) );
		    insertHashTable( census->hash, (StgWord)identity, ctr );
		    ctr->c.resid = real_size;
		    ctr->identity = identity;
		    ctr->next = census->ctrs;
		    census->ctrs = ctr;
		}
	    }

	    p += size;
	}
    }
}

void
heapCensus( void )
{
  nat g, s;
  Census *census;

  stat_startHeapCensus();

  census = &censuses[era];
  census->time  = mut_user_time();
  census->hash  = allocHashTable();
  census->ctrs  = NULL;
  census->arena = newArena();
    
  // calculate retainer sets if necessary
#ifdef PROFILING
  if (doingRetainerProfiling()) {
      retainerProfile();
  }
#endif

  // traverse the heap, collecting the census info
  heapCensusChain( census, small_alloc_list );
  if (RtsFlags.GcFlags.generations == 1) {
      heapCensusChain( census, g0s0->to_blocks );
  } else {
      for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	  for (s = 0; s < generations[g].n_steps; s++) {
	      heapCensusChain( census, generations[g].steps[s].blocks );
	      // Are we interested in large objects?  might be
	      // confusing to include the stack in a heap profile.
	      // heapCensusChain( census, generations[g].steps[s].large_objects );
	  }
      }
  }

  // dump out the census info
  dumpCensus( census );

  // free our storage
  freeHashTable(census->hash, NULL/* don't free the elements */);
  arenaFree(census->arena);

  // we're into the next time period now
  nextEra();

  stat_endHeapCensus();
}    

#endif /* PROFILING || DEBUG_HEAP_PROF */

