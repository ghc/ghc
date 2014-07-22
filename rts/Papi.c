/* -----------------------------------------------------------------------------
 * (c) The GHC Team 2006
 *
 * Initialization and use of the PAPI performance monitoring library
 *
 *
 * For adding events or add your processor counters modify
 *
 *   init_countable_events
 *   papi_report
 *
 * ---------------------------------------------------------------------------*/


#ifdef USE_PAPI /* ugly */

#include <papi.h>
/* The posix symbols get defined in a header included from papi.h.
 * undefind them here to allow redefinition in PosixSource.h */
#undef _POSIX_SOURCE
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Stats.h"
#include "Papi.h"

// used to protect the aggregated counters
#ifdef THREADED_RTS
static Mutex papi_counter_mutex;
#endif

struct _papi_events {
  int event_code;
  const char * event_name;
};

/* Beware, these counters are Opteron specific
 * I obtained the numbers using the papi_avail
 * and papi_native_avail utilities.
 * This is certainly not the official PAPI way
 * of doing things.
 */
#define FR_BR 0x40000040
#define FR_BR_MIS 0x40000041
#define FR_BR_MISCOMPARE 0x40000048
#define DC_ACCESS 0x40000019
#define DC_MISS 0x4000001a
#define FR_DISPATCH_STALLS 0x40000054
#define FR_DISPATCH_STALLS_BR 0x40000055
#define FR_DISPATCH_STALLS_FULL_REORDER 0x40000058
#define FR_DISPATCH_STALLS_FULL_RESERVATION 0x40000059
#define FR_DISPATCH_STALLS_FULL_LS 0x4000005b
#define DC_L2_REFILL_MOES 0x40001e1b
#define DC_SYS_REFILL_MOES 0x40001e1c

/* This is bad, it should be in a header */
#define BIG_STRING_LEN 512


#define PAPI_CHECK(CALL)                                                \
  if((papi_error=(CALL)) != PAPI_OK) {                                  \
    debugBelch("PAPI function failed in module %s at line %d "          \
               "with error code %d\n",                                  \
              __FILE__,__LINE__,papi_error);                            \
  }

/* While PAPI reporting is going on this flag is on */
int papi_is_reporting;

/* Event sets and counter arrays for GC and mutator */

int MutatorEvents = PAPI_NULL;
int GCEvents = PAPI_NULL;

int papi_error;

/* Arbitrary, to avoid using malloc */
#define MAX_PAPI_EVENTS 10
static char papiNativeEventNames[MAX_PAPI_EVENTS][PAPI_MAX_STR_LEN];

static nat n_papi_events = 0;


/* Events counted during GC and Mutator execution */
/* There's a trailing comma, do all C compilers accept that? */
static struct _papi_events papi_events[MAX_PAPI_EVENTS];
long_long MutatorCounters[MAX_PAPI_EVENTS];
long_long GC0Counters[MAX_PAPI_EVENTS];
long_long GC1Counters[MAX_PAPI_EVENTS];

long_long start_mutator_cycles;
long_long mutator_cycles = 0;
long_long start_gc_cycles;
long_long gc0_cycles = 0;
long_long gc1_cycles = 0;



static long_long papi_counter(long_long values[],int event);
static void papi_add_events(int EventSet);

static nat max_hardware_counters = 2;

/* If you want to add events to count, extend the
 * init_countable_events and the papi_report function.
 * Be aware that your processor can count a limited number
 * of events simultaneously, you can turn on multiplexing
 * to increase that number, though.
 */
static void papi_add_event(const char *name, int code)
{
    if (n_papi_events >= max_hardware_counters) {
        errorBelch("too many PAPI events for this CPU (max: %d)",
                   max_hardware_counters);
        stg_exit(EXIT_FAILURE);
    }
    papi_events[n_papi_events].event_code = code;
    papi_events[n_papi_events].event_name = name;
    n_papi_events++;
}

static void
init_countable_events(void)
{
    max_hardware_counters = PAPI_num_counters();

#define PAPI_ADD_EVENT(EVENT) papi_add_event(#EVENT,EVENT)

    if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_BRANCH) {
        PAPI_ADD_EVENT(FR_BR);
        PAPI_ADD_EVENT(FR_BR_MIS);
        // Docs are wrong? Opteron does not count indirect branch
        // misses exclusively
        PAPI_ADD_EVENT(FR_BR_MISCOMPARE);
    } else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_STALLS) {
        PAPI_ADD_EVENT(FR_DISPATCH_STALLS);
        PAPI_ADD_EVENT(FR_DISPATCH_STALLS_BR);
        PAPI_ADD_EVENT(FR_DISPATCH_STALLS_FULL_LS);
    } else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L1) {
        PAPI_ADD_EVENT(PAPI_L1_DCA);
        PAPI_ADD_EVENT(PAPI_L1_DCM);
    } else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L2) {
        PAPI_ADD_EVENT(PAPI_L2_DCA);
        PAPI_ADD_EVENT(PAPI_L2_DCM);
    } else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CB_EVENTS) {
        PAPI_ADD_EVENT(DC_L2_REFILL_MOES);
        PAPI_ADD_EVENT(DC_SYS_REFILL_MOES);
        PAPI_ADD_EVENT(FR_BR_MIS);
    } else if (RtsFlags.PapiFlags.eventType==PAPI_USER_EVENTS) {
        nat i;
        char *name;
        char *asciiEventCode;
        int code;
        for (i = 0; i < RtsFlags.PapiFlags.numUserEvents; i++) {
          if(RtsFlags.PapiFlags.userEventsKind[i] == PAPI_PRESET_EVENT_KIND) {
            name = RtsFlags.PapiFlags.userEvents[i];
            PAPI_CHECK(PAPI_event_name_to_code(name, &code))
          }
          else { // PAPI_NATIVE_EVENT_KIND
            asciiEventCode = RtsFlags.PapiFlags.userEvents[i];
            name = papiNativeEventNames[i];
            code = strtol(asciiEventCode, NULL, 16 /* hex number expected */);
            PAPI_CHECK(PAPI_event_code_to_name(code, name))
          }
          papi_add_event(name, code);
        }
    } else {
        // PAPI_ADD_EVENT(PAPI_L1_DCA); // L1 data cache accesses
        // PAPI_ADD_EVENT(PAPI_L1_ICR); // L1 instruction cache reads
        // PAPI_ADD_EVENT(PAPI_L1_ICM); // L1 instruction cache misses
        // PAPI_ADD_EVENT(PAPI_L1_STM); // L1 store misses
        // PAPI_ADD_EVENT(PAPI_L1_DCM); // L1 data cache misses
        // PAPI_ADD_EVENT(PAPI_L1_LDM); // L1 load misses
        // PAPI_ADD_EVENT(PAPI_L2_TCM); // L2 cache misses
        // PAPI_ADD_EVENT(PAPI_L2_STM); // L2 store misses
        // PAPI_ADD_EVENT(PAPI_L2_DCW); // L2 data cache writes
        // PAPI_ADD_EVENT(PAPI_L2_DCR); // L2 data cache reads
        // PAPI_ADD_EVENT(PAPI_L2_TCW); // L2 cache writes
        // PAPI_ADD_EVENT(PAPI_L2_TCR); // L2 cache reads
        // PAPI_ADD_EVENT(PAPI_CA_CLN); // exclusive access to clean cache line
        // PAPI_ADD_EVENT(PAPI_TLB_DM); // TLB misses
        PAPI_ADD_EVENT(PAPI_TOT_INS); // Total instructions
        PAPI_ADD_EVENT(PAPI_TOT_CYC); // Total instructions
        // PAPI_ADD_EVENT(PAPI_CA_SHR); // exclusive access to shared cache line
        // PAPI_ADD_EVENT(PAPI_RES_STL); // Cycles stalled on any resource

    }

    // We might also consider:
    //  PAPI_BR_MSP     Conditional branch instructions mispredicted
    //  PAPI_RES_STL    Cycles stalled on any resource
};


static void
papi_report_event(const char *name, StgWord64 value)
{
    static char temp[BIG_STRING_LEN];
    showStgWord64(value,temp,rtsTrue/*commas*/);
    statsPrintf("  %15s  %15s\n", name, temp);
}

/* This function reports counters for GC and mutator */
static void
papi_report(long_long counters[])
{
    nat i;

/* Report the value of a counter as a percentage of another counter */
#define PAPI_REPORT_PCT(EVENTSET,EVENT,EVENTTOT) \
    statsPrintf("   " #EVENT " %% of " #EVENTTOT " : %.1f%%\n",      \
         papi_counter(EVENTSET,EVENT)*100.0/papi_counter(EVENTSET,EVENTTOT))

    for (i = 0; i < n_papi_events; i++)
    {
        papi_report_event(papi_events[i].event_name, counters[i]);
    }

    if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_BRANCH) {
        PAPI_REPORT_PCT(counters,FR_BR_MIS,FR_BR);
        PAPI_REPORT_PCT(counters,FR_BR_MISCOMPARE,FR_BR);
    }

    else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L1) {
        PAPI_REPORT_PCT(counters,PAPI_L1_DCM,PAPI_L1_DCA);
    }

    else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L2) {
        PAPI_REPORT_PCT(counters,PAPI_L2_DCM,PAPI_L2_DCA);
    }
}

void
papi_stats_report (void)
{
    statsPrintf("  Mutator CPU counters\n");
    papi_report_event("CYCLES", mutator_cycles);
    papi_report(MutatorCounters);

    statsPrintf("\n  GC(0) CPU counters\n");
    papi_report_event("CYCLES", gc0_cycles);
    papi_report(GC0Counters);

    statsPrintf("\n  GC(1) CPU counters\n");
    papi_report_event("CYCLES", gc1_cycles);
    papi_report(GC1Counters);
}

void
papi_init_eventset (int *event_set)
{
    PAPI_register_thread();
    PAPI_CHECK( PAPI_create_eventset(event_set));
    papi_add_events(*event_set);
}

void
papi_init (void)
{
    /* Initialise the performance tracking library */
    int ver;
    if ((ver = PAPI_library_init(PAPI_VER_CURRENT)) != PAPI_VER_CURRENT) {
        if (ver > 0) {
            errorBelch("PAPI_library_init: wrong version: %x", ver);
            stg_exit(EXIT_FAILURE);
        } else {
            sysErrorBelch("PAPI_library_init");
            stg_exit(EXIT_FAILURE);
        }
    }

#ifdef THREADED_RTS
    {
        int err;
        if ((err = PAPI_thread_init(osThreadId)) < 0) {
            barf("PAPI_thread_init: %d",err);
        }

        initMutex(&papi_counter_mutex);
    }
#endif

    init_countable_events();

    papi_init_eventset(&MutatorEvents);
    papi_init_eventset(&GCEvents);
}

/* Extract the value corresponding to an event */
static long_long
papi_counter(long_long values[],int event)
{
  nat i;
  for(i=0;i<n_papi_events;i++) {
    if(papi_events[i].event_code==event) {
      return values[i];
    }
  }
  /* Passed a wrong event? */
  debugBelch("Event %d is not part of event set\n",event);
  return 0;
}

/* Add the events of papi_events into an event set */
static void
papi_add_events(int EventSet)
{
  nat i;
  for(i=0;i<n_papi_events;i++) {
    if((papi_error=PAPI_add_event(EventSet,
                                  papi_events[i].event_code))
       != PAPI_OK)
      debugBelch("Failed adding %s to event set with error code %d\n",
                 papi_events[i].event_name,papi_error);
  }
}

/* We should be using elapsed cycles
 * to be consistent with time metric chosen in Stats.c (Elapsed time).
 * This is an approximation to the cycles that the program spends.
 * Note that the counters, in contrast, are virtual and user space.
 */
#define PAPI_cycles PAPI_get_virt_cyc

void
papi_start_mutator_count(void)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_start(MutatorEvents));
    start_mutator_cycles = PAPI_cycles();
    RELEASE_LOCK(&papi_counter_mutex);
}

void
papi_stop_mutator_count(void)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    mutator_cycles += PAPI_cycles() - start_mutator_cycles;
    PAPI_CHECK( PAPI_accum(MutatorEvents,MutatorCounters));
    PAPI_CHECK( PAPI_stop(MutatorEvents,NULL));
    RELEASE_LOCK(&papi_counter_mutex);
}

void
papi_start_gc_count(void)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_start(GCEvents));
    start_gc_cycles = PAPI_cycles();
    RELEASE_LOCK(&papi_counter_mutex);
}

void
papi_stop_gc0_count(void)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_accum(GCEvents,GC0Counters));
    PAPI_CHECK( PAPI_stop(GCEvents,NULL));
    gc0_cycles += PAPI_cycles() - start_gc_cycles;
    RELEASE_LOCK(&papi_counter_mutex);
}


void
papi_stop_gc1_count(void)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_accum(GCEvents,GC1Counters));
    PAPI_CHECK( PAPI_stop(GCEvents,NULL));
    gc1_cycles += PAPI_cycles() - start_gc_cycles;
    RELEASE_LOCK(&papi_counter_mutex);
}


void
papi_thread_start_gc1_count(int event_set)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_start(event_set));
    RELEASE_LOCK(&papi_counter_mutex);
}

void
papi_thread_stop_gc1_count(int event_set)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_accum(event_set,GC1Counters));
    PAPI_CHECK( PAPI_stop(event_set,NULL));
    RELEASE_LOCK(&papi_counter_mutex);
}

#endif /* USE_PAPI */
