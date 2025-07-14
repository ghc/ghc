/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2003
 *
 * Support for heap profiling
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Profiling.h"
#include "ProfHeap.h"
#include "ProfHeapInternal.h"
#include "Stats.h"
#include "Hash.h"
#include "RetainerProfile.h"
#include "LdvProfile.h"
#include "Arena.h"
#include "Printer.h"
#include "Trace.h"
#include "sm/GCThread.h"

#include <fs_rts.h>
#include <string.h>

#if defined(darwin_HOST_OS)
#include <xlocale.h>
#else
#include <locale.h>
#endif

FILE *hp_file;
static char *hp_filename; /* heap profile (hp2ps style) log file */

/* ------------------------------------------------------------------------
 * Locales
 *
 * The heap profile contains information that is sensitive to the C runtime's
 * LC_NUMERIC locale settings.  By default libc starts in a "C" setting that's
 * the same everywhere, and the one hp2ps expects.  But the program may change
 * that at runtime.  So we change it back when we're writing a sample, and
 * restore it before yielding back.
 *
 * On POSIX.1-2008 systems, this is done with the locale_t opaque type, created
 * with newlocale() at profiler init, switched to with uselocale() and freed at
 * exit with freelocale().
 *
 * As an exception for Darwin, this comes through the <xlocale.h> header instead
 * of <locale.h>.
 *
 * On platforms which don't have uselocale(3), we fall back to setlocale() which
 * mutates the global state. This is of course not thread-safe but is better
 * than nothing.
 *
 * On Windows, a different _locale_t opaque type does exist, but isn't directly
 * usable without special-casing all printf() and related calls, which I'm not
 * motivated to trawl through as I don't even have a Windows box to test on.
 * (But if you do and are so inclined, be my guest!)
 * So we just call setlocale(), making it thread-local and restoring the
 * locale and its thread-locality state on yield.
 * --------------------------------------------------------------------- */

#if defined(mingw32_HOST_OS)
static int prof_locale_per_thread = -1;
static const char *saved_locale = NULL;
#elif defined(HAVE_USELOCALE)
static locale_t prof_locale = 0, saved_locale = 0;
#else
static char *saved_locale = NULL;
#endif

STATIC_INLINE void
init_prof_locale( void )
{
#if defined(HAVE_USELOCALE)
    if (! prof_locale) {
        prof_locale = newlocale(LC_NUMERIC_MASK, "POSIX", 0);
        if (! prof_locale) {
            sysErrorBelch("Couldn't allocate heap profiler locale");
            /* non-fatal: risk using an unknown locale, but won't crash */
        }
    }
#endif
}

STATIC_INLINE void
free_prof_locale( void )
{
#if defined(HAVE_USELOCALE)
    if (prof_locale) {
        freelocale(prof_locale);
        prof_locale = 0;
    }
#endif
}

STATIC_INLINE void
set_prof_locale( void )
{
#if defined(mingw32_HOST_OS)
    prof_locale_per_thread = _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);
    saved_locale = setlocale(LC_NUMERIC, NULL);
    setlocale(LC_NUMERIC, "C");
#elif defined(HAVE_USELOCALE)
    saved_locale = uselocale(prof_locale);
#else
    saved_locale = setlocale(LC_NUMERIC, NULL);
    setlocale(LC_NUMERIC, "C");
#endif
}

STATIC_INLINE void
restore_locale( void )
{
#if defined(mingw32_HOST_OS)
    _configthreadlocale(prof_locale_per_thread);
    setlocale(LC_NUMERIC, saved_locale);
#elif defined(HAVE_USELOCALE)
    uselocale(saved_locale);
#else
    setlocale(LC_NUMERIC, saved_locale);
#endif
}

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
static uint32_t max_era;

StgWord user_era;

void
setUserEra (StgWord w){
  user_era = w;
}

StgWord
getUserEra (void){
  return user_era;
}

StgWord
incrementUserEra (StgWord w){
  return atomic_inc(&user_era, w);
}

inline void
initLDVCtr( counter *ctr )
{
    ctr->c.ldv.prim = 0;
    ctr->c.ldv.not_used = 0;
    ctr->c.ldv.used = 0;
    ctr->c.ldv.void_total = 0;
    ctr->c.ldv.drag_total = 0;
}

static Census *censuses = NULL;
static uint32_t n_censuses = 0;

#if defined(PROFILING)
static void aggregateCensusInfo( void );
#endif

static void dumpCensus( Census *census );

static bool closureSatisfiesConstraints( const StgClosure* p );

/* ----------------------------------------------------------------------------
 * Find the "closure identity", which is a unique pointer representing
 * the band to which this closure's heap space is attributed in the
 * heap profile.
 * ------------------------------------------------------------------------- */
static const void *
closureIdentity( const StgClosure *p )
{
    switch (RtsFlags.ProfFlags.doHeapProfile) {

#if defined(PROFILING)
    case HEAP_BY_CCS:
        return p->header.prof.ccs;
    case HEAP_BY_MOD:
        return p->header.prof.ccs->cc->module;
    case HEAP_BY_DESCR:
        return GET_PROF_DESC(get_itbl(p));
    case HEAP_BY_ERA:
        // Static objects should have user_era = 0
        // MP: If user_era == 0 then closureIdentity returns the NULL pointer, and
        // the closure is not counted to the census
        return (void *)p->header.prof.hp.era;
    case HEAP_BY_TYPE:
        return GET_PROF_TYPE(get_itbl(p));
    case HEAP_BY_RETAINER:
        // AFAIK, the only closures in the heap which might not have a
        // valid retainer set are DEAD_WEAK closures.
        if (isRetainerSetValid(p))
            return retainerSetOf(p);
        else
            return NULL;
#endif

    case HEAP_BY_CLOSURE_TYPE:
    {
        const StgInfoTable *info;
        info = get_itbl(p);
        switch (info->type) {
        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_0_1:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_0_2:
        case CONSTR_NOCAF:
            return GET_CON_DESC(itbl_to_con_itbl(info));
        default:
            return closure_type_names[info->type];
        }
    }
    case HEAP_BY_INFO_TABLE: {
        return get_itbl(p);
        }

    default:
        barf("closureIdentity");
    }
}

/* --------------------------------------------------------------------------
 * Profiling type predicates
 * ----------------------------------------------------------------------- */

// Processes a closure 'c' being destroyed whose size is 'size'.
// Make sure that LDV_recordDead() is not invoked on 'inherently used' closures
// such as TSO; they should not be involved in computing dragNew or voidNew.
//
// Even though era is checked in both LdvCensusForDead() and
// LdvCensusKillAll(), we still need to make sure that era is > 0 because
// LDV_recordDead() may be called from elsewhere in the runtime system. E.g.,
// when a thunk is replaced by an indirection object.

#if defined(PROFILING)
void
LDV_recordDead( const StgClosure *c, uint32_t size )
{
    const void *id;
    uint32_t t;
    counter *ctr;

    ASSERT(!isInherentlyUsed(get_itbl(c)->type));

    if (era > 0 && closureSatisfiesConstraints(c)) {
        size -= sizeofW(StgProfHeader);
        ASSERT(LDVW(c) != 0);
        if ((LDVW((c)) & LDV_STATE_MASK) == LDV_STATE_CREATE) {
            t = (LDVW((c)) & LDV_CREATE_MASK) >> LDV_SHIFT;
            if (t < era) {
                if (RtsFlags.ProfFlags.bioSelector == NULL) {
                    censuses[t].void_total   += size;
                    censuses[era].void_total -= size;
                    ASSERT(censuses[t].void_total <= censuses[t].not_used);
                } else {
                    id = closureIdentity(c);
                    ctr = lookupHashTable(censuses[t].hash, (StgWord)id);
                    if (ctr == NULL)
                        barf("LDV_recordDead: Failed to find counter for closure %p", c);

                    ctr->c.ldv.void_total += size;
                    ctr = lookupHashTable(censuses[era].hash, (StgWord)id);
                    if (ctr == NULL) {
                        ctr = arenaAlloc(censuses[era].arena, sizeof(counter));
                        initLDVCtr(ctr);
                        insertHashTable(censuses[era].hash, (StgWord)id, ctr);
                        ctr->identity = id;
                        ctr->next = censuses[era].ctrs;
                        censuses[era].ctrs = ctr;
                    }
                    ctr->c.ldv.void_total -= size;
                }
            }
        } else {
            t = LDVW((c)) & LDV_LAST_MASK;
            if (t + 1 < era) {
                if (RtsFlags.ProfFlags.bioSelector == NULL) {
                    censuses[t+1].drag_total += size;
                    censuses[era].drag_total -= size;
                } else {
                    const void *id;
                    id = closureIdentity(c);
                    ctr = lookupHashTable(censuses[t+1].hash, (StgWord)id);
                    ASSERT( ctr != NULL );
                    ctr->c.ldv.drag_total += size;
                    ctr = lookupHashTable(censuses[era].hash, (StgWord)id);
                    if (ctr == NULL) {
                        ctr = arenaAlloc(censuses[era].arena, sizeof(counter));
                        initLDVCtr(ctr);
                        insertHashTable(censuses[era].hash, (StgWord)id, ctr);
                        ctr->identity = id;
                        ctr->next = censuses[era].ctrs;
                        censuses[era].ctrs = ctr;
                    }
                    ctr->c.ldv.drag_total -= size;
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
    // N.B. When not LDV profiling we reinitialise the same Census over
    // and over again. Consequently, we need to ensure that we free the
    // resources from the previous census.
    if (census->hash) {
        freeHashTable(census->hash, NULL);
    }
    if (census->arena) {
        arenaFree(census->arena);
    }

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
    arenaFree(census->arena);
    freeHashTable(census->hash, NULL);
}

/* --------------------------------------------------------------------------
 * Increases era by 1 and initialize census[era].
 * Reallocates gi[] and increases its size if needed.
 * ----------------------------------------------------------------------- */

static void
nextEra( void )
{
    if (user_era > 0 && RtsFlags.ProfFlags.incrementUserEra){
      user_era++;
    }

#if defined(PROFILING)
    if (doingLDVProfiling()) {
        era++;

        if (era == max_era) {
            errorBelch("Maximum number of censuses reached.");
            if (rtsConfig.rts_opts_suggestions == true) {
                if (rtsConfig.rts_opts_enabled == RtsOptsAll)  {
                    errorBelch("Use `+RTS -i' to reduce censuses.");
                } else  {
                    errorBelch("Relink with -rtsopts and "
                               "use `+RTS -i' to reduce censuses.");
                }
            }
            stg_exit(EXIT_FAILURE);
        }

        if (era == n_censuses) {
            n_censuses *= 2;
            censuses = stgReallocBytes(censuses, sizeof(Census) * n_censuses,
                                       "nextEra");
            memset(&censuses[era], 0, sizeof(Census) * n_censuses / 2);
        }
    }
#endif /* PROFILING */

    initEra( &censuses[era] );
}

/* ----------------------------------------------------------------------------
 * Heap profiling by info table
 * ------------------------------------------------------------------------- */

static void
printEscapedString(const char* string)
{
    for (const char* p = string; *p != '\0'; ++p) {
        if (*p == '\"') {
            // Escape every " as ""
            fputc('"', hp_file);
        }
        fputc(*p, hp_file);
    }
}

static void
printSample(bool beginSample, StgDouble sampleValue)
{
    fprintf(hp_file, "%s %f\n",
            (beginSample ? "BEGIN_SAMPLE" : "END_SAMPLE"),
            sampleValue);
    if (!beginSample) {
        fflush(hp_file);
    }
}


void freeHeapProfiling (void)
{
    free_prof_locale();
}

/* --------------------------------------------------------------------------
 * Initialize the heap profiler
 * ----------------------------------------------------------------------- */
void
initHeapProfiling(void)
{
    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return;
    }

    init_prof_locale();
    set_prof_locale();

    char *stem;

    if (RtsFlags.CcFlags.outputFileNameStem) {
        stem = stgMallocBytes(strlen(RtsFlags.CcFlags.outputFileNameStem) + 1, "initHeapProfiling");
        strcpy(stem, RtsFlags.CcFlags.outputFileNameStem);
    } else {
        stem = stgMallocBytes(strlen(prog_name) + 1, "initHeapProfiling");
        strcpy(stem, prog_name);

        // Drop the platform's executable suffix if there is one
#if defined(mingw32_HOST_OS)
        dropExtension(stem, ".exe");
#elif defined(wasm32_HOST_ARCH)
        dropExtension(stem, ".wasm");
#endif
    }

  if (RtsFlags.ProfFlags.doHeapProfile) {
    /* Initialise the log file name */
    hp_filename = stgMallocBytes(strlen(stem) + 6, "hpFileName");
    sprintf(hp_filename, "%s.hp", stem);

    /* open the log file */
    if ((hp_file = __rts_fopen(hp_filename, "w+")) == NULL) {
      debugBelch("Can't open profiling report file %s\n",
              hp_filename);
      RtsFlags.ProfFlags.doHeapProfile = 0;
      stgFree(stem);
      return;
    }
  }

  stgFree(stem);

#if defined(PROFILING)
    if (doingLDVProfiling() && doingRetainerProfiling()) {
        errorBelch("cannot mix -hb and -hr");
        stg_exit(EXIT_FAILURE);
    }
#if defined(THREADED_RTS)
    // See #12019.
    if (doingLDVProfiling() && RtsFlags.ParFlags.nCapabilities > 1) {
        errorBelch("-hb cannot be used with multiple capabilities");
        stg_exit(EXIT_FAILURE);
    }
#endif
#endif

#if defined(PROFILING)
    if (doingErasProfiling()){
      user_era = 1;
    }
#else
    user_era = 0;
#endif

    // we only count eras if we're doing LDV profiling.  Otherwise era
    // is fixed at zero.
#if defined(PROFILING)
    if (doingLDVProfiling()) {
        era = 1;
        n_censuses = 32;
    } else
#endif
    {
        era = 0;
        n_censuses = 1;
    }

    // max_era = 2^LDV_SHIFT
    max_era = 1 << LDV_SHIFT;

    censuses = stgMallocBytes(sizeof(Census) * n_censuses, "initHeapProfiling");

    // Ensure that arena and hash are NULL since otherwise initEra will attempt to free them.
    for (unsigned int i=0; i < n_censuses; i++) {
        censuses[i].arena = NULL;
        censuses[i].hash = NULL;
    }
    initEra( &censuses[era] );

    /* initProfilingLogFile(); */
    fprintf(hp_file, "JOB \"");
    printEscapedString(prog_name);

#if defined(PROFILING)
    for (int i = 1; i < prog_argc; ++i) {
        fputc(' ', hp_file);
        printEscapedString(prog_argv[i]);
    }
    fprintf(hp_file, " +RTS");
    for (int i = 0; i < rts_argc; ++i) {
        fputc(' ', hp_file);
        printEscapedString(rts_argv[i]);
    }
#endif /* PROFILING */

    fprintf(hp_file, "\"\n" );

    fprintf(hp_file, "DATE \"%s\"\n", time_str());

    fprintf(hp_file, "SAMPLE_UNIT \"seconds\"\n");
    fprintf(hp_file, "VALUE_UNIT \"bytes\"\n");

    printSample(true, 0);
    printSample(false, 0);

#if defined(PROFILING)
    if (doingRetainerProfiling()) {
        initRetainerProfiling();
    }
#endif

    restore_locale();

    traceInitEvent(traceHeapProfBegin);
}

void
endHeapProfiling(void)
{
    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return;
    }

    set_prof_locale();

#if defined(PROFILING)
    if (doingRetainerProfiling()) {
        endRetainerProfiling();
    } else if (doingLDVProfiling()) {
        uint32_t t;
        LdvCensusKillAll();
        aggregateCensusInfo();
        for (t = 1; t < era; t++) {
            dumpCensus( &censuses[t] );
        }

        if (RtsFlags.ProfFlags.bioSelector != NULL) {
            for (t = 1; t <= era; t++) {
                freeEra( &censuses[t] );
            }
        } else {
            freeEra( &censuses[era] );
        }
    } else {
        freeEra( &censuses[0] );
    }
#else
    freeEra( &censuses[0] );
#endif

    stgFree(censuses);

    RTSStats stats;
    getRTSStats(&stats);
    Time mut_time = stats.mutator_cpu_ns;
    StgDouble seconds = TimeToSecondsDbl(mut_time);
    printSample(true, seconds);
    printSample(false, seconds);
    fclose(hp_file);

    restore_locale();
}



#if defined(PROFILING)
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
fprint_ccs(FILE *fp, CostCentreStack *ccs, uint32_t max_length)
{
    char buf[max_length+1], *p, *buf_end;

    // MAIN on its own gets printed as "MAIN", otherwise we ignore MAIN.
    if (ccs == CCS_MAIN) {
        fprintf(fp, "MAIN");
        return;
    }

    fprintf(fp, "(%" FMT_Int ")", ccs->ccsID);

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

bool
strMatchesSelector( const char* str, const char* sel )
{
   const char* p;
   // debugBelch("str_matches_selector %s %s\n", str, sel);
   while (1) {
       // Compare str against wherever we've got to in sel.
       p = str;
       while (*p != '\0' && *sel != ',' && *sel != '\0' && *p == *sel) {
           p++; sel++;
       }
       // Match if all of str used and have reached the end of a sel fragment.
       if (*p == '\0' && (*sel == ',' || *sel == '\0'))
           return true;

       // No match.  Advance sel to the start of the next elem.
       while (*sel != ',' && *sel != '\0') sel++;
       if (*sel == ',') sel++;

       /* Run out of sel ?? */
       if (*sel == '\0') return false;
   }
}

#endif /* PROFILING */

/* -----------------------------------------------------------------------------
 * Figure out whether a closure should be counted in this census, by
 * testing against all the specified constraints.
 * -------------------------------------------------------------------------- */
static bool
closureSatisfiesConstraints( const StgClosure* p )
{
#if !defined(PROFILING)
    (void)p;   /* keep gcc -Wall happy */
    return true;
#else
   bool b;

   // The CCS has a selected field to indicate whether this closure is
   // deselected by not being mentioned in the module, CC, or CCS
   // selectors.
   if (!p->header.prof.ccs->selected) {
       return false;
   }

   if (RtsFlags.ProfFlags.descrSelector) {
       b = strMatchesSelector( (GET_PROF_DESC(get_itbl((StgClosure *)p))),
                                 RtsFlags.ProfFlags.descrSelector );
       if (!b) return false;
   }
   if (RtsFlags.ProfFlags.typeSelector) {
       b = strMatchesSelector( (GET_PROF_TYPE(get_itbl((StgClosure *)p))),
                                RtsFlags.ProfFlags.typeSelector );
       if (!b) return false;
   }
   if (RtsFlags.ProfFlags.eraSelector) {
      return (p->header.prof.hp.era == RtsFlags.ProfFlags.eraSelector);
   }
   if (RtsFlags.ProfFlags.retainerSelector) {
       RetainerSet *rs;
       uint32_t i;
       // We must check that the retainer set is valid here.  One
       // reason it might not be valid is if this closure is a
       // a newly deceased weak pointer (i.e. a DEAD_WEAK), since
       // these aren't reached by the retainer profiler's traversal.
       if (isRetainerSetValid((StgClosure *)p)) {
           rs = retainerSetOf((StgClosure *)p);
           if (rs != NULL) {
               for (i = 0; i < rs->num; i++) {
                   b = strMatchesSelector( rs->element[i]->cc->label,
                                           RtsFlags.ProfFlags.retainerSelector );
                   if (b) return true;
               }
           }
       }
       return false;
   }
   return true;
#endif /* PROFILING */
}

/* -----------------------------------------------------------------------------
 * Aggregate the heap census info for biographical profiling
 * -------------------------------------------------------------------------- */
#if defined(PROFILING)
static void
aggregateCensusInfo( void )
{
    HashTable *acc;
    uint32_t t;
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
    ssize_t count;

    set_prof_locale();

    printSample(true, census->time);


    if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) {
      traceHeapBioProfSampleBegin(era, census->rtime);
    } else {
      traceHeapProfSampleBegin(era);
    }



#if defined(PROFILING)

    /* change typecast to uint64_t to remove
     * print formatting warning. See #12636 */
    if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV) {
        fprintf(hp_file, "VOID\t%" FMT_Word64 "\n",
                (uint64_t)(census->void_total *
                                     sizeof(W_)));
        fprintf(hp_file, "LAG\t%" FMT_Word64 "\n",
                (uint64_t)((census->not_used - census->void_total) *
                                     sizeof(W_)));
        fprintf(hp_file, "USE\t%" FMT_Word64 "\n",
                (uint64_t)((census->used - census->drag_total) *
                                     sizeof(W_)));
        fprintf(hp_file, "INHERENT_USE\t%" FMT_Word64 "\n",
                (uint64_t)(census->prim * sizeof(W_)));
        fprintf(hp_file, "DRAG\t%" FMT_Word64 "\n",
                (uint64_t)(census->drag_total * sizeof(W_)));


        // Eventlog
        traceHeapProfSampleString("VOID",
                (census->void_total * sizeof(W_)));
        traceHeapProfSampleString("LAG",
                ((census->not_used - census->void_total) *
                                     sizeof(W_)));
        traceHeapProfSampleString("USE",
                ((census->used - census->drag_total) *
                                     sizeof(W_)));
        traceHeapProfSampleString("INHERENT_USE",
                (census->prim * sizeof(W_)));
        traceHeapProfSampleString("DRAG",
                (census->drag_total * sizeof(W_)));

        traceHeapProfSampleEnd(era);
        printSample(false, census->time);
        return;
    }
#endif

    for (ctr = census->ctrs; ctr != NULL; ctr = ctr->next) {

#if defined(PROFILING)
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

        switch (RtsFlags.ProfFlags.doHeapProfile) {
        case HEAP_BY_CLOSURE_TYPE:
            fprintf(hp_file, "%s", (char *)ctr->identity);
            traceHeapProfSampleString((char *)ctr->identity,
                                      count * sizeof(W_));
            break;
        case HEAP_BY_INFO_TABLE:
            fprintf(hp_file, "%p", ctr->identity);
            char str[100];
            sprintf(str, "%p", ctr->identity);
            traceHeapProfSampleString(str, count * sizeof(W_));
            break;
#if defined(PROFILING)
        case HEAP_BY_CCS:
            fprint_ccs(hp_file, (CostCentreStack *)ctr->identity,
                       RtsFlags.ProfFlags.ccsLength);
            traceHeapProfSampleCostCentre((CostCentreStack *)ctr->identity,
                                          count * sizeof(W_));
            break;
        case HEAP_BY_ERA:
            fprintf(hp_file, "%" FMT_Word, (StgWord)ctr->identity);
            char str_era[100];
            sprintf(str_era, "%" FMT_Word, (StgWord)ctr->identity);
            traceHeapProfSampleString(str_era, count * sizeof(W_));
            break;
        case HEAP_BY_MOD:
        case HEAP_BY_DESCR:
        case HEAP_BY_TYPE:
            fprintf(hp_file, "%s", (char *)ctr->identity);
            traceHeapProfSampleString((char *)ctr->identity,
                                      count * sizeof(W_));
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
            printRetainerSetShort(hp_file, rs, (W_)count * sizeof(W_)
                                             , RtsFlags.ProfFlags.ccsLength);
            break;
        }
#endif
        default:
            barf("dumpCensus; doHeapProfile");
        }

        fprintf(hp_file, "\t%" FMT_Word "\n", (W_)count * sizeof(W_));
    }

    traceHeapProfSampleEnd(era);
    printSample(false, census->time);

    restore_locale();
}

inline counter*
heapInsertNewCounter(Census *census, StgWord identity)
{
    counter *ctr = arenaAlloc(census->arena, sizeof(counter));

    initLDVCtr(ctr);
    insertHashTable( census->hash, identity, ctr );
    ctr->identity = (void*)identity;
    ctr->next = census->ctrs;
    census->ctrs = ctr;

    return ctr;
}

static void heapProfObject(Census *census, StgClosure *p, size_t size,
                           bool prim
#if !defined(PROFILING)
                           STG_UNUSED
#endif
                           )
{
    const void *identity;
    size_t real_size;
    counter *ctr;

            identity = NULL;

#if defined(PROFILING)
            // subtract the profiling overhead
            real_size = size - sizeofW(StgProfHeader);
#else
            real_size = size;
#endif

            if (closureSatisfiesConstraints((StgClosure*)p)) {
#if defined(PROFILING)
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
                        ctr = lookupHashTable(census->hash, (StgWord)identity);
                        if (ctr != NULL) {
#if defined(PROFILING)
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
                            ctr = heapInsertNewCounter(census, (StgWord)identity);
#if defined(PROFILING)
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

// Compact objects require special handling code because they
// are not stored consecutively in memory (rather, each object
// is a list of objects), and that would break the while loop
// below. But we know that each block holds at most one object
// so we don't need the loop.
//
// See Note [Compact Normal Forms] for details.
static void
heapCensusCompactList(Census *census, bdescr *bd)
{
    for (; bd != NULL; bd = bd->link) {
        StgCompactNFDataBlock *block = (StgCompactNFDataBlock*)bd->start;
        StgCompactNFData *str = block->owner;
        heapProfObject(census, (StgClosure*)str,
                       compact_nfdata_full_sizeW(str), true);
    }
}

/*
 * Take a census of the contents of a "normal" (e.g. not large, not compact)
 * heap block. This can, however, handle PINNED blocks.
 */
static void
heapCensusBlock(Census *census, bdescr *bd)
{
    StgPtr p = bd->start;

    // In the case of PINNED blocks there can be (zeroed) slop at the beginning
    // due to object alignment.
    if (bd->flags & BF_PINNED) {
        while (p < bd->free && !*p) p++;
    }

    while (p < bd->free) {
        const StgInfoTable *info = get_itbl((const StgClosure *)p);
        bool prim = false;
        size_t size;

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

        case FUN:
        case BLACKHOLE:
        case BLOCKING_QUEUE:
        case FUN_1_0:
        case FUN_0_1:
        case FUN_1_1:
        case FUN_0_2:
        case FUN_2_0:
        case CONSTR:
        case CONSTR_NOCAF:
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
            prim = true;
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
            prim = true;
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
            prim = true;
            size = arr_words_sizeW((StgArrBytes*)p);
            break;

        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
        case MUT_ARR_PTRS_FROZEN_CLEAN:
        case MUT_ARR_PTRS_FROZEN_DIRTY:
            prim = true;
            size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)p);
            break;

        case SMALL_MUT_ARR_PTRS_CLEAN:
        case SMALL_MUT_ARR_PTRS_DIRTY:
        case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
        case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
            prim = true;
            size = small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs *)p);
            break;

        case TSO:
            prim = true;
            size = sizeofW(StgTSO);
            break;

        case STACK:
            prim = true;
            size = stack_sizeW((StgStack*)p);
            break;

        case TREC_CHUNK:
            prim = true;
            size = sizeofW(StgTRecChunk);
            break;

        case CONTINUATION:
            size = continuation_sizeW((StgContinuation *)p);
            break;

        case COMPACT_NFDATA:
            barf("heapCensus, found compact object in the wrong list");
            break;

        default:
            barf("heapCensus, unknown object: %d", info->type);
        }

        heapProfObject(census,(StgClosure*)p,size,prim);

        p += size;

        /* skip over slop, see Note [slop on the heap] */
        while (p < bd->free && !*p) p++;
        /* Note [skipping slop in the heap profiler]
         * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         * We make sure to zero slop that can remain after a major GC so
         * here we can assume any slop words we see until the block's free
         * pointer are zero. Since info pointers are always nonzero we can
         * use this to scan for the next valid heap closure.
         *
         * Note that not all types of slop are relevant here, only the ones
         * that can remain after major GC. So essentially just large objects
         * and pinned objects. All other closures will have been packed nice
         * and tight into fresh blocks.
         */
    }
}

// determine whether a closure should be assigned to the PRIM cost-centre.
static bool
closureIsPrim (StgPtr p)
{
  bool prim = false;
  const StgInfoTable *info = get_itbl((const StgClosure *)p);
  switch (info->type) {
    case THUNK:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_SELECTOR:
    case FUN:
    case BLACKHOLE:
    case BLOCKING_QUEUE:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case IND:
    case AP:
    case PAP:
    case AP_STACK:
    case CONTINUATION:
        prim = false;
        break;

    case BCO:
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    case TVAR:
    case WEAK:
    case PRIM:
    case MUT_PRIM:
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    case ARR_WORDS:
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
    case TSO:
    case STACK:
    case TREC_CHUNK:
        prim = true;
        break;

    case COMPACT_NFDATA:
        barf("heapCensus, found compact object in the wrong list");
        break;

    default:
        barf("heapCensus, unknown object: %d", info->type);
  }
  return prim;
}

static void
heapCensusSegment (Census* census, struct NonmovingSegment* seg )
{
  unsigned int block_size = nonmovingSegmentBlockSize(seg);
  unsigned int block_count = nonmovingSegmentBlockCount(seg);

  for (unsigned int b = 0; b < block_count; b++) {
    StgPtr p = nonmovingSegmentGetBlock(seg, b);
    // ignore unmarked heap objects
    if (!nonmovingClosureMarkedThisCycle(p)) continue;
    // NB: We round up the size of objects to the segment block size.
    // This aligns with live bytes accounting for the nonmoving collector.
    heapProfObject(census, (StgClosure*)p, block_size / sizeof(W_), closureIsPrim(p));
  }
}

/* Note [Non-concurrent nonmoving collector heap census]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * When using the nonmoving collector, we currently disable concurrent collection
 * to simplify heap census accounting.
 *
 * Without concurrent allocation, marked objects on the nonmoving heap are exactly
 * the live objects.
 *
 * We disable concurrent collection both for GCs that lead to a heap census and not.
 * This is because a concurrent collection can overlap with a GC that is meant
 * to perform a heap census. Alternatively we could better handle the case where
 * a non-concurrent collection is triggered while a non-concurrent collection
 * is running.
 */

static void
heapCensusSegmentList (Census* census, struct NonmovingSegment* seg )
{
  for (; seg; seg = seg->link) {
    heapCensusSegment(census, seg);
  }
}

/* -----------------------------------------------------------------------------
 * Code to perform a heap census.
 * -------------------------------------------------------------------------- */
static void
heapCensusChain( Census *census, bdescr *bd )
{
    for (; bd != NULL; bd = bd->link) {
        // When we shrink a large ARR_WORDS, we do not adjust the free pointer
        // of the associated block descriptor, thus introducing slop at the end
        // of the object.  This slop remains after GC, violating the assumption
        // of the loop below that all slop has been eliminated (#11627).
        // The slop isn't always zeroed (e.g. in non-profiling mode, cf
        // OVERWRITING_CLOSURE_OFS).
        // Consequently, we handle large ARR_WORDS objects as a special case.
        if (bd->flags & BF_LARGE) {
            StgPtr p = bd->start;
            // There may be some initial zeros due to object alignment.
            while (p < bd->free && !*p) p++;
            if (get_itbl((StgClosure *)p)->type == ARR_WORDS) {
                size_t size = arr_words_sizeW((StgArrBytes *)p);
                bool prim = true;
                heapProfObject(census, (StgClosure *)p, size, prim);
                continue;
            }
        }

        heapCensusBlock(census, bd);
    }
}

// Time is process CPU time of beginning of current GC and is used as
// the mutator CPU time reported as the census timestamp.
void heapCensus (Time t)
{
  uint32_t g, n;
  Census *census;
  gen_workspace *ws;

  census = &censuses[era];
  census->time  = TimeToSecondsDbl(t);
  census->rtime = TimeToNS(stat_getElapsedTime());


  // calculate retainer sets if necessary
#if defined(PROFILING)
  if (doingRetainerProfiling()) {
      retainerProfile();
  }
#endif

#if defined(PROFILING)
  stat_startHeapCensus();
#endif

  // Traverse the heap, collecting the census info
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      heapCensusChain( census, generations[g].blocks );
      // Are we interested in large objects?  might be
      // confusing to include the stack in a heap profile.
      heapCensusChain( census, generations[g].large_objects );
      heapCensusCompactList ( census, generations[g].compact_objects );

      for (n = 0; n < getNumCapabilities(); n++) {
          ws = &gc_threads[n]->gens[g];
          heapCensusChain(census, ws->todo_bd);
          heapCensusChain(census, ws->part_list);
          heapCensusChain(census, ws->scavd_list);
      }
  }

  if (RtsFlags.GcFlags.useNonmoving) {
    for (unsigned int i = 0; i < nonmoving_alloca_cnt; i++) {
      heapCensusSegmentList(census, nonmovingHeap.allocators[i].filled);
      heapCensusSegmentList(census, nonmovingHeap.allocators[i].saved_filled);
      heapCensusSegmentList(census, nonmovingHeap.allocators[i].active);

      heapCensusChain(census, nonmoving_large_objects);
      heapCensusCompactList(census, nonmoving_compact_objects);

      // segments living on capabilities
      for (unsigned int j = 0; j < getNumCapabilities(); j++) {
        Capability* cap = getCapability(j);
        heapCensusSegment(census, cap->current_segments[i]);
      }
    }

  }

  // dump out the census info
#if defined(PROFILING)
    // We can't generate any info for LDV profiling until
    // the end of the run...
    if (!doingLDVProfiling())
        dumpCensus( census );
#else
    dumpCensus( census );
#endif


  // free our storage, unless we're keeping all the census info for
  // future restriction by biography.
#if defined(PROFILING)
  if (RtsFlags.ProfFlags.bioSelector == NULL)
#endif
  {
      freeEra(census);
      census->hash = NULL;
      census->arena = NULL;
  }

  // we're into the next time period now
  nextEra();

#if defined(PROFILING)
  stat_endHeapCensus();
#endif
}
