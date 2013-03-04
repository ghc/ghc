/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2003
 *
 * Support for heap profiling
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Profiling.h"
#include "ProfHeap.h"
#include "Stats.h"
#include "Hash.h"
#include "RetainerProfile.h"
#include "LdvProfile.h"
#include "Arena.h"
#include "Printer.h"
#include "sm/GCThread.h"

#include <string.h>

/* -----------------------------------------------------------------------------
 * era stores the current time period.  It is the same as the
 * number of censuses that have been performed.
 *
 * RESTRICTION:
 *   era must be no longer than LDV_SHIFT (15 or 30) bits.
 * Invariants:
 *   era is initialized to 1 in initHeapProfiling().
 *
 * max_era is initialized to 2^LDV_SHIFT in initHeapProfiling().
 * When era reaches max_era, the profiling stops because a closure can
 * store only up to (max_era - 1) as its creation or last use time.
 * -------------------------------------------------------------------------- */
unsigned int era;
static nat max_era;

/* -----------------------------------------------------------------------------
 * Counters
 *
 * For most heap profiles each closure identity gets a simple count
 * of live words in the heap at each census.  However, if we're
 * selecting by biography, then we have to keep the various
 * lag/drag/void counters for each identity.
 * -------------------------------------------------------------------------- */
typedef struct _counter {
    void *identity;
    union {
	nat resid;
	struct {
	    long prim;     // total size of 'inherently used' closures
	    long not_used; // total size of 'never used' closures
	    long used;     // total size of 'used at least once' closures
	    long void_total;  // current total size of 'destroyed without being used' closures
	    long drag_total;  // current total size of 'used at least once and waiting to die'
	} ldv;
    } c;
    struct _counter *next;
} counter;

STATIC_INLINE void
initLDVCtr( counter *ctr )
{
    ctr->c.ldv.prim = 0;
    ctr->c.ldv.not_used = 0;
    ctr->c.ldv.used = 0;
    ctr->c.ldv.void_total = 0;
    ctr->c.ldv.drag_total = 0;
}

typedef struct {
    double      time;    // the time in MUT time when the census is made
    HashTable * hash;
    counter   * ctrs;
    Arena     * arena;

    // for LDV profiling, when just displaying by LDV
    long       prim;
    long       not_used;
    long       used;
    long       void_total;
    long       drag_total;
} Census;

static Census *censuses = NULL;
static nat n_censuses = 0;

#ifdef PROFILING
static void aggregateCensusInfo( void );
#endif

static void dumpCensus( Census *census );

static rtsBool closureSatisfiesConstraints( StgClosure* p );

/* ----------------------------------------------------------------------------
 * Find the "closure identity", which is a unique pointer representing
 * the band to which this closure's heap space is attributed in the
 * heap profile.
 * ------------------------------------------------------------------------- */
static void *
closureIdentity( StgClosure *p )
{
    switch (RtsFlags.ProfFlags.doHeapProfile) {

#ifdef PROFILING
    case HEAP_BY_CCS:
	return p->header.prof.ccs;
    case HEAP_BY_MOD:
	return p->header.prof.ccs->cc->module;
    case HEAP_BY_DESCR:
	return GET_PROF_DESC(get_itbl(p));
    case HEAP_BY_TYPE:
	return GET_PROF_TYPE(get_itbl(p));
    case HEAP_BY_RETAINER:
	// AFAIK, the only closures in the heap which might not have a
	// valid retainer set are DEAD_WEAK closures.
	if (isRetainerSetFieldValid(p))
	    return retainerSetOf(p);
	else
	    return NULL;

#else
    case HEAP_BY_CLOSURE_TYPE:
    {
        StgInfoTable *info;
        info = get_itbl(p);
        switch (info->type) {
        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_0_1:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_0_2:
        case CONSTR_STATIC:
        case CONSTR_NOCAF_STATIC:
            return GET_CON_DESC(itbl_to_con_itbl(info));
        default:
            return closure_type_names[info->type];
        }
    }

#endif
    default:
	barf("closureIdentity");
    }
}

/* --------------------------------------------------------------------------
 * Profiling type predicates
 * ----------------------------------------------------------------------- */
#ifdef PROFILING
STATIC_INLINE rtsBool
doingLDVProfiling( void )
{
    return (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV 
	    || RtsFlags.ProfFlags.bioSelector != NULL);
}

STATIC_INLINE rtsBool
doingRetainerProfiling( void )
{
    return (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER
	    || RtsFlags.ProfFlags.retainerSelector != NULL);
}
#endif /* PROFILING */

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
    void *id;
    nat t;
    counter *ctr;

    if (era > 0 && closureSatisfiesConstraints(c)) {
	size -= sizeofW(StgProfHeader);
	ASSERT(LDVW(c) != 0);
	if ((LDVW((c)) & LDV_STATE_MASK) == LDV_STATE_CREATE) {
	    t = (LDVW((c)) & LDV_CREATE_MASK) >> LDV_SHIFT;
	    if (t < era) {
		if (RtsFlags.ProfFlags.bioSelector == NULL) {
                    censuses[t].void_total   += (long)size;
                    censuses[era].void_total -= (long)size;
		    ASSERT(censuses[t].void_total < censuses[t].not_used);
		} else {
		    id = closureIdentity(c);
		    ctr = lookupHashTable(censuses[t].hash, (StgWord)id);
		    ASSERT( ctr != NULL );
                    ctr->c.ldv.void_total += (long)size;
		    ctr = lookupHashTable(censuses[era].hash, (StgWord)id);
		    if (ctr == NULL) {
			ctr = arenaAlloc(censuses[era].arena, sizeof(counter));
			initLDVCtr(ctr);
			insertHashTable(censuses[era].hash, (StgWord)id, ctr);
			ctr->identity = id;
			ctr->next = censuses[era].ctrs;
			censuses[era].ctrs = ctr;
		    }
                    ctr->c.ldv.void_total -= (long)size;
		}
	    }
	} else {
	    t = LDVW((c)) & LDV_LAST_MASK;
	    if (t + 1 < era) {
		if (RtsFlags.ProfFlags.bioSelector == NULL) {
		    censuses[t+1].drag_total += size;
		    censuses[era].drag_total -= size;
		} else {
		    void *id;
		    id = closureIdentity(c);
		    ctr = lookupHashTable(censuses[t+1].hash, (StgWord)id);
		    ASSERT( ctr != NULL );
                    ctr->c.ldv.drag_total += (long)size;
		    ctr = lookupHashTable(censuses[era].hash, (StgWord)id);
		    if (ctr == NULL) {
			ctr = arenaAlloc(censuses[era].arena, sizeof(counter));
			initLDVCtr(ctr);
			insertHashTable(censuses[era].hash, (StgWord)id, ctr);
			ctr->identity = id;
			ctr->next = censuses[era].ctrs;
			censuses[era].ctrs = ctr;
		    }
                    ctr->c.ldv.drag_total -= (long)size;
		}
	    }
	}
    }
}
#endif

/* --------------------------------------------------------------------------
 * Initialize censuses[era];
 * ----------------------------------------------------------------------- */

STATIC_INLINE void
initEra(Census *census)
{
    census->hash  = allocHashTable();
    census->ctrs  = NULL;
    census->arena = newArena();

    census->not_used   = 0;
    census->used       = 0;
    census->prim       = 0;
    census->void_total = 0;
    census->drag_total = 0;
}

STATIC_INLINE void
freeEra(Census *census)
{
    if (RtsFlags.ProfFlags.bioSelector != NULL)
        // when bioSelector==NULL, these are freed in heapCensus()
    {
        arenaFree(census->arena);
        freeHashTable(census->hash, NULL);
    }
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
	    errorBelch("maximum number of censuses reached; use +RTS -i to reduce");
	    stg_exit(EXIT_FAILURE);
	}
	
	if (era == n_censuses) {
	    n_censuses *= 2;
	    censuses = stgReallocBytes(censuses, sizeof(Census) * n_censuses,
				       "nextEra");
	}
    }
#endif /* PROFILING */

    initEra( &censuses[era] );
}

/* ----------------------------------------------------------------------------
 * Heap profiling by info table
 * ------------------------------------------------------------------------- */

#if !defined(PROFILING)
FILE *hp_file;
static char *hp_filename;

void initProfiling1 (void)
{
}

void freeProfiling (void)
{
}

void initProfiling2 (void)
{
    char *prog;

    prog = stgMallocBytes(strlen(prog_name) + 1, "initProfiling2");
    strcpy(prog, prog_name);
#ifdef mingw32_HOST_OS
    // on Windows, drop the .exe suffix if there is one
    {
        char *suff;
        suff = strrchr(prog,'.');
        if (suff != NULL && !strcmp(suff,".exe")) {
            *suff = '\0';
        }
    }
#endif

  if (RtsFlags.ProfFlags.doHeapProfile) {
    /* Initialise the log file name */
    hp_filename = stgMallocBytes(strlen(prog) + 6, "hpFileName");
    sprintf(hp_filename, "%s.hp", prog);
    
    /* open the log file */
    if ((hp_file = fopen(hp_filename, "w")) == NULL) {
      debugBelch("Can't open profiling report file %s\n", 
	      hp_filename);
      RtsFlags.ProfFlags.doHeapProfile = 0;
      return;
    }
  }
  
  stgFree(prog);

  initHeapProfiling();
}

void endProfiling( void )
{
  endHeapProfiling();
}
#endif /* !PROFILING */

static void
printSample(rtsBool beginSample, StgDouble sampleValue)
{
    StgDouble fractionalPart, integralPart;
    fractionalPart = modf(sampleValue, &integralPart);
    fprintf(hp_file, "%s %" FMT_Word64 ".%02" FMT_Word64 "\n",
            (beginSample ? "BEGIN_SAMPLE" : "END_SAMPLE"),
            (StgWord64)integralPart, (StgWord64)(fractionalPart * 100));
}

/* --------------------------------------------------------------------------
 * Initialize the heap profilier
 * ----------------------------------------------------------------------- */
nat
initHeapProfiling(void)
{
    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return 0;
    }

#ifdef PROFILING
    if (doingLDVProfiling() && doingRetainerProfiling()) {
	errorBelch("cannot mix -hb and -hr");
	stg_exit(EXIT_FAILURE);
    }
#endif

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

    // max_era = 2^LDV_SHIFT
	max_era = 1 << LDV_SHIFT;

    n_censuses = 32;
    censuses = stgMallocBytes(sizeof(Census) * n_censuses, "initHeapProfiling");

    initEra( &censuses[era] );

    /* initProfilingLogFile(); */
    fprintf(hp_file, "JOB \"%s", prog_name);

#ifdef PROFILING
    {
	int count;
	for(count = 1; count < prog_argc; count++)
	    fprintf(hp_file, " %s", prog_argv[count]);
	fprintf(hp_file, " +RTS");
	for(count = 0; count < rts_argc; count++)
	    fprintf(hp_file, " %s", rts_argv[count]);
    }
#endif /* PROFILING */

    fprintf(hp_file, "\"\n" );

    fprintf(hp_file, "DATE \"%s\"\n", time_str());

    fprintf(hp_file, "SAMPLE_UNIT \"seconds\"\n");
    fprintf(hp_file, "VALUE_UNIT \"bytes\"\n");

    printSample(rtsTrue, 0);
    printSample(rtsFalse, 0);

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
    if (doingLDVProfiling()) {
	nat t;
	LdvCensusKillAll();
	aggregateCensusInfo();
	for (t = 1; t < era; t++) {
	    dumpCensus( &censuses[t] );
	}
    }
#endif

#ifdef PROFILING
    if (doingLDVProfiling()) {
        nat t;
        for (t = 1; t <= era; t++) {
            freeEra( &censuses[t] );
        }
    } else {
        freeEra( &censuses[0] );
    }
#else
    freeEra( &censuses[0] );
#endif

    stgFree(censuses);

    seconds = mut_user_time();
    printSample(rtsTrue, seconds);
    printSample(rtsFalse, seconds);
    fclose(hp_file);
}



#ifdef PROFILING
static size_t
buf_append(char *p, const char *q, char *end)
{
    int m;

    for (m = 0; p < end; p++, q++, m++) {
	*p = *q;
	if (*q == '\0') { break; }
    }
    return m;
}

static void
fprint_ccs(FILE *fp, CostCentreStack *ccs, nat max_length)
{
    char buf[max_length+1], *p, *buf_end;

    // MAIN on its own gets printed as "MAIN", otherwise we ignore MAIN.
    if (ccs == CCS_MAIN) {
	fprintf(fp, "MAIN");
	return;
    }

    fprintf(fp, "(%ld)", ccs->ccsID);

    p = buf;
    buf_end = buf + max_length + 1;

    // keep printing components of the stack until we run out of space
    // in the buffer.  If we run out of space, end with "...".
    for (; ccs != NULL && ccs != CCS_MAIN; ccs = ccs->prevStack) {

	// CAF cost centres print as M.CAF, but we leave the module
	// name out of all the others to save space.
	if (!strcmp(ccs->cc->label,"CAF")) {
	    p += buf_append(p, ccs->cc->module, buf_end);
	    p += buf_append(p, ".CAF", buf_end);
	} else {
	    p += buf_append(p, ccs->cc->label, buf_end);
	    if (ccs->prevStack != NULL && ccs->prevStack != CCS_MAIN) {
		p += buf_append(p, "/", buf_end);
	    }
	}
	
	if (p >= buf_end) {
	    sprintf(buf+max_length-4, "...");
	    break;
	}
    }
    fprintf(fp, "%s", buf);
}

rtsBool
strMatchesSelector( char* str, char* sel )
{
   char* p;
   // debugBelch("str_matches_selector %s %s\n", str, sel);
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

#endif /* PROFILING */

/* -----------------------------------------------------------------------------
 * Figure out whether a closure should be counted in this census, by
 * testing against all the specified constraints.
 * -------------------------------------------------------------------------- */
static rtsBool
closureSatisfiesConstraints( StgClosure* p )
{
#if !defined(PROFILING)
    (void)p;   /* keep gcc -Wall happy */
    return rtsTrue;
#else
   rtsBool b;

   // The CCS has a selected field to indicate whether this closure is
   // deselected by not being mentioned in the module, CC, or CCS
   // selectors.
   if (!p->header.prof.ccs->selected) {
       return rtsFalse;
   }

   if (RtsFlags.ProfFlags.descrSelector) {
       b = strMatchesSelector( (GET_PROF_DESC(get_itbl((StgClosure *)p))),
				 RtsFlags.ProfFlags.descrSelector );
       if (!b) return rtsFalse;
   }
   if (RtsFlags.ProfFlags.typeSelector) {
       b = strMatchesSelector( (GET_PROF_TYPE(get_itbl((StgClosure *)p))),
                                RtsFlags.ProfFlags.typeSelector );
       if (!b) return rtsFalse;
   }
   if (RtsFlags.ProfFlags.retainerSelector) {
       RetainerSet *rs;
       nat i;
       // We must check that the retainer set is valid here.  One
       // reason it might not be valid is if this closure is a
       // a newly deceased weak pointer (i.e. a DEAD_WEAK), since
       // these aren't reached by the retainer profiler's traversal.
       if (isRetainerSetFieldValid((StgClosure *)p)) {
	   rs = retainerSetOf((StgClosure *)p);
	   if (rs != NULL) {
	       for (i = 0; i < rs->num; i++) {
		   b = strMatchesSelector( rs->element[i]->cc->label,
					   RtsFlags.ProfFlags.retainerSelector );
		   if (b) return rtsTrue;
	       }
	   }
       }
       return rtsFalse;
   }
   return rtsTrue;
#endif /* PROFILING */
}

/* -----------------------------------------------------------------------------
 * Aggregate the heap census info for biographical profiling
 * -------------------------------------------------------------------------- */
#ifdef PROFILING
static void
aggregateCensusInfo( void )
{
    HashTable *acc;
    nat t;
    counter *c, *d, *ctrs;
    Arena *arena;

    if (!doingLDVProfiling()) return;

    // Aggregate the LDV counters when displaying by biography.
    if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) {
        long void_total, drag_total;

	// Now we compute void_total and drag_total for each census
	// After the program has finished, the void_total field of
	// each census contains the count of words that were *created*
	// in this era and were eventually void.  Conversely, if a
	// void closure was destroyed in this era, it will be
	// represented by a negative count of words in void_total.
	//
	// To get the count of live words that are void at each
	// census, just propagate the void_total count forwards:

	void_total = 0;
	drag_total = 0;
	for (t = 1; t < era; t++) { // note: start at 1, not 0
	    void_total += censuses[t].void_total;
	    drag_total += censuses[t].drag_total;
	    censuses[t].void_total = void_total;
	    censuses[t].drag_total = drag_total;

	    ASSERT( censuses[t].void_total <= censuses[t].not_used );
	    // should be true because: void_total is the count of
	    // live words that are void at this census, which *must*
	    // be less than the number of live words that have not
	    // been used yet.

	    ASSERT( censuses[t].drag_total <= censuses[t].used );
	    // similar reasoning as above.
	}
	
	return;
    }

    // otherwise... we're doing a heap profile that is restricted to
    // some combination of lag, drag, void or use.  We've kept all the
    // census info for all censuses so far, but we still need to
    // aggregate the counters forwards.

    arena = newArena();
    acc = allocHashTable();
    ctrs = NULL;

    for (t = 1; t < era; t++) {

	// first look through all the counters we're aggregating
	for (c = ctrs; c != NULL; c = c->next) {
	    // if one of the totals is non-zero, then this closure
	    // type must be present in the heap at this census time...
	    d = lookupHashTable(censuses[t].hash, (StgWord)c->identity);

	    if (d == NULL) {
		// if this closure identity isn't present in the
		// census for this time period, then our running
		// totals *must* be zero.
		ASSERT(c->c.ldv.void_total == 0 && c->c.ldv.drag_total == 0);

		// debugCCS(c->identity);
		// debugBelch(" census=%d void_total=%d drag_total=%d\n",
		//         t, c->c.ldv.void_total, c->c.ldv.drag_total);
	    } else {
		d->c.ldv.void_total += c->c.ldv.void_total;
		d->c.ldv.drag_total += c->c.ldv.drag_total;
		c->c.ldv.void_total =  d->c.ldv.void_total;
		c->c.ldv.drag_total =  d->c.ldv.drag_total;

		ASSERT( c->c.ldv.void_total >= 0 );
		ASSERT( c->c.ldv.drag_total >= 0 );
	    }
	}

	// now look through the counters in this census to find new ones
	for (c = censuses[t].ctrs; c != NULL; c = c->next) {
	    d = lookupHashTable(acc, (StgWord)c->identity);
	    if (d == NULL) {
		d = arenaAlloc( arena, sizeof(counter) );
		initLDVCtr(d);
		insertHashTable( acc, (StgWord)c->identity, d );
		d->identity = c->identity;
		d->next = ctrs;
		ctrs = d;
		d->c.ldv.void_total = c->c.ldv.void_total;
		d->c.ldv.drag_total = c->c.ldv.drag_total;
	    }
	    ASSERT( c->c.ldv.void_total >= 0 );
	    ASSERT( c->c.ldv.drag_total >= 0 );
	}
    }

    freeHashTable(acc, NULL);
    arenaFree(arena);
}
#endif

/* -----------------------------------------------------------------------------
 * Print out the results of a heap census.
 * -------------------------------------------------------------------------- */
static void
dumpCensus( Census *census )
{
    counter *ctr;
    long count;

    printSample(rtsTrue, census->time);

#ifdef PROFILING
    if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) {
      fprintf(hp_file, "VOID\t%lu\n", (unsigned long)(census->void_total) * sizeof(W_));
	fprintf(hp_file, "LAG\t%lu\n", 
		(unsigned long)(census->not_used - census->void_total) * sizeof(W_));
	fprintf(hp_file, "USE\t%lu\n", 
		(unsigned long)(census->used - census->drag_total) * sizeof(W_));
	fprintf(hp_file, "INHERENT_USE\t%lu\n", 
		(unsigned long)(census->prim) * sizeof(W_));
	fprintf(hp_file, "DRAG\t%lu\n",
		(unsigned long)(census->drag_total) * sizeof(W_));
	printSample(rtsFalse, census->time);
	return;
    }
#endif

    for (ctr = census->ctrs; ctr != NULL; ctr = ctr->next) {

#ifdef PROFILING
	if (RtsFlags.ProfFlags.bioSelector != NULL) {
	    count = 0;
	    if (strMatchesSelector("lag", RtsFlags.ProfFlags.bioSelector))
		count += ctr->c.ldv.not_used - ctr->c.ldv.void_total;
	    if (strMatchesSelector("drag", RtsFlags.ProfFlags.bioSelector))
		count += ctr->c.ldv.drag_total;
	    if (strMatchesSelector("void", RtsFlags.ProfFlags.bioSelector))
		count += ctr->c.ldv.void_total;
	    if (strMatchesSelector("use", RtsFlags.ProfFlags.bioSelector))
		count += ctr->c.ldv.used - ctr->c.ldv.drag_total;
	} else
#endif
	{
	    count = ctr->c.resid;
	}

	ASSERT( count >= 0 );

	if (count == 0) continue;

#if !defined(PROFILING)
	switch (RtsFlags.ProfFlags.doHeapProfile) {
	case HEAP_BY_CLOSURE_TYPE:
	    fprintf(hp_file, "%s", (char *)ctr->identity);
	    break;
	}
#endif
	
#ifdef PROFILING
	switch (RtsFlags.ProfFlags.doHeapProfile) {
	case HEAP_BY_CCS:
	    fprint_ccs(hp_file, (CostCentreStack *)ctr->identity, RtsFlags.ProfFlags.ccsLength);
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
	    printRetainerSetShort(hp_file, rs, RtsFlags.ProfFlags.ccsLength);
	    break;
	}
	default:
	    barf("dumpCensus; doHeapProfile");
	}
#endif

	fprintf(hp_file, "\t%" FMT_SizeT "\n", (W_)count * sizeof(W_));
    }

    printSample(rtsFalse, census->time);
}


static void heapProfObject(Census *census, StgClosure *p, nat size,
                           rtsBool prim
#ifndef PROFILING
                           STG_UNUSED
#endif
                           )
{
    void *identity;
    nat real_size;
    counter *ctr;

            identity = NULL;

#ifdef PROFILING
	    // subtract the profiling overhead
	    real_size = size - sizeofW(StgProfHeader);
#else
	    real_size = size;
#endif

	    if (closureSatisfiesConstraints((StgClosure*)p)) {
#ifdef PROFILING
		if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) {
		    if (prim)
			census->prim += real_size;
		    else if ((LDVW(p) & LDV_STATE_MASK) == LDV_STATE_CREATE)
			census->not_used += real_size;
		    else
			census->used += real_size;
		} else
#endif
		{
		    identity = closureIdentity((StgClosure *)p);

		    if (identity != NULL) {
			ctr = lookupHashTable( census->hash, (StgWord)identity );
			if (ctr != NULL) {
#ifdef PROFILING
			    if (RtsFlags.ProfFlags.bioSelector != NULL) {
				if (prim)
				    ctr->c.ldv.prim += real_size;
				else if ((LDVW(p) & LDV_STATE_MASK) == LDV_STATE_CREATE)
				    ctr->c.ldv.not_used += real_size;
				else
				    ctr->c.ldv.used += real_size;
			    } else
#endif
			    {
				ctr->c.resid += real_size;
			    }
			} else {
			    ctr = arenaAlloc( census->arena, sizeof(counter) );
			    initLDVCtr(ctr);
			    insertHashTable( census->hash, (StgWord)identity, ctr );
			    ctr->identity = identity;
			    ctr->next = census->ctrs;
			    census->ctrs = ctr;

#ifdef PROFILING
			    if (RtsFlags.ProfFlags.bioSelector != NULL) {
				if (prim)
				    ctr->c.ldv.prim = real_size;
				else if ((LDVW(p) & LDV_STATE_MASK) == LDV_STATE_CREATE)
				    ctr->c.ldv.not_used = real_size;
				else
				    ctr->c.ldv.used = real_size;
			    } else
#endif
			    {
				ctr->c.resid = real_size;
			    }
			}
		    }
		}
	    }
}

/* -----------------------------------------------------------------------------
 * Code to perform a heap census.
 * -------------------------------------------------------------------------- */
static void
heapCensusChain( Census *census, bdescr *bd )
{
    StgPtr p;
    StgInfoTable *info;
    nat size;
    rtsBool prim;

    for (; bd != NULL; bd = bd->link) {

        // HACK: pretend a pinned block is just one big ARR_WORDS
        // owned by CCS_PINNED.  These blocks can be full of holes due
        // to alignment constraints so we can't traverse the memory
        // and do a proper census.
        if (bd->flags & BF_PINNED) {
            StgClosure arr;
            SET_HDR(&arr, &stg_ARR_WORDS_info, CCS_PINNED);
            heapProfObject(census, &arr, bd->blocks * BLOCK_SIZE_W, rtsTrue);
            continue;
        }

	p = bd->start;
	while (p < bd->free) {
	    info = get_itbl((StgClosure *)p);
            prim = rtsFalse;
	    
	    switch (info->type) {

	    case THUNK:
		size = thunk_sizeW_fromITBL(info);
		break;

	    case THUNK_1_1:
	    case THUNK_0_2:
	    case THUNK_2_0:
		size = sizeofW(StgThunkHeader) + 2;
		break;

	    case THUNK_1_0:
	    case THUNK_0_1:
	    case THUNK_SELECTOR:
		size = sizeofW(StgThunkHeader) + 1;
		break;

	    case CONSTR:
	    case FUN:
	    case IND_PERM:
	    case BLACKHOLE:
	    case BLOCKING_QUEUE:
	    case FUN_1_0:
	    case FUN_0_1:
	    case FUN_1_1:
	    case FUN_0_2:
	    case FUN_2_0:
	    case CONSTR_1_0:
	    case CONSTR_0_1:
	    case CONSTR_1_1:
	    case CONSTR_0_2:
	    case CONSTR_2_0:
		size = sizeW_fromITBL(info);
		break;

	    case IND:
		// Special case/Delicate Hack: INDs don't normally
		// appear, since we're doing this heap census right
		// after GC.  However, GarbageCollect() also does
		// resurrectThreads(), which can update some
		// blackholes when it calls raiseAsync() on the
		// resurrected threads.  So we know that any IND will
		// be the size of a BLACKHOLE.
		size = BLACKHOLE_sizeW();
		break;

	    case BCO:
                prim = rtsTrue;
		size = bco_sizeW((StgBCO *)p);
		break;

            case MVAR_CLEAN:
            case MVAR_DIRTY:
            case TVAR:
            case WEAK:
	    case PRIM:
	    case MUT_PRIM:
	    case MUT_VAR_CLEAN:
	    case MUT_VAR_DIRTY:
		prim = rtsTrue;
		size = sizeW_fromITBL(info);
		break;

	    case AP:
		size = ap_sizeW((StgAP *)p);
		break;

	    case PAP:
		size = pap_sizeW((StgPAP *)p);
		break;

	    case AP_STACK:
		size = ap_stack_sizeW((StgAP_STACK *)p);
		break;
		
	    case ARR_WORDS:
		prim = rtsTrue;
		size = arr_words_sizeW((StgArrWords*)p);
		break;
		
	    case MUT_ARR_PTRS_CLEAN:
	    case MUT_ARR_PTRS_DIRTY:
	    case MUT_ARR_PTRS_FROZEN:
	    case MUT_ARR_PTRS_FROZEN0:
		prim = rtsTrue;
		size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)p);
		break;
		
	    case TSO:
		prim = rtsTrue;
#ifdef PROFILING
		if (RtsFlags.ProfFlags.includeTSOs) {
                    size = sizeofW(StgTSO);
		    break;
		} else {
		    // Skip this TSO and move on to the next object
                    p += sizeofW(StgTSO);
		    continue;
		}
#else
                size = sizeofW(StgTSO);
		break;
#endif

            case STACK:
		prim = rtsTrue;
#ifdef PROFILING
		if (RtsFlags.ProfFlags.includeTSOs) {
                    size = stack_sizeW((StgStack*)p);
                    break;
		} else {
		    // Skip this TSO and move on to the next object
                    p += stack_sizeW((StgStack*)p);
		    continue;
		}
#else
                size = stack_sizeW((StgStack*)p);
		break;
#endif

            case TREC_CHUNK:
		prim = rtsTrue;
		size = sizeofW(StgTRecChunk);
		break;

	    default:
		barf("heapCensus, unknown object: %d", info->type);
	    }
	    
            heapProfObject(census,(StgClosure*)p,size,prim);

	    p += size;
	}
    }
}

void heapCensus (Time t)
{
  nat g, n;
  Census *census;
  gen_workspace *ws;

  census = &censuses[era];
  census->time  = mut_user_time_until(t);
    
  // calculate retainer sets if necessary
#ifdef PROFILING
  if (doingRetainerProfiling()) {
      retainerProfile();
  }
#endif

#ifdef PROFILING
  stat_startHeapCensus();
#endif

  // Traverse the heap, collecting the census info
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      heapCensusChain( census, generations[g].blocks );
      // Are we interested in large objects?  might be
      // confusing to include the stack in a heap profile.
      heapCensusChain( census, generations[g].large_objects );

      for (n = 0; n < n_capabilities; n++) {
          ws = &gc_threads[n]->gens[g];
          heapCensusChain(census, ws->todo_bd);
          heapCensusChain(census, ws->part_list);
          heapCensusChain(census, ws->scavd_list);
      }
  }

  // dump out the census info
#ifdef PROFILING
    // We can't generate any info for LDV profiling until
    // the end of the run...
    if (!doingLDVProfiling())
	dumpCensus( census );
#else
    dumpCensus( census );
#endif


  // free our storage, unless we're keeping all the census info for
  // future restriction by biography.
#ifdef PROFILING
  if (RtsFlags.ProfFlags.bioSelector == NULL)
  {
      freeHashTable( census->hash, NULL/* don't free the elements */ );
      arenaFree( census->arena );
      census->hash = NULL;
      census->arena = NULL;
  }
#endif

  // we're into the next time period now
  nextEra();

#ifdef PROFILING
  stat_endHeapCensus();
#endif
}    

