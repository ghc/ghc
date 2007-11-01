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

#include "Papi.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "RtsFlags.h"
#include "OSThreads.h"

// used to protect the aggregated counters
#ifdef THREADED_RTS
static Mutex papi_counter_mutex;
#endif

struct _papi_events {
  int event_code;
  char * event_name;
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


#define PAPI_CHECK(CALL) \
  if((papi_error=(CALL)) != PAPI_OK) { \
   debugBelch("PAPI function failed in module %s at line %d with error code %d\n", \
	      __FILE__,__LINE__,papi_error);				\
  }

/* While PAPI reporting is going on this flag is on */
int papi_is_reporting;

/* Event sets and counter arrays for GC and mutator */

int MutatorEvents = PAPI_NULL;
int GCEvents = PAPI_NULL;

int papi_error;

/* Arbitrary, to avoid using malloc */
#define MAX_PAPI_EVENTS 10

int n_papi_events = 0;


/* Events counted during GC and Mutator execution */
/* There's a trailing comma, do all C compilers accept that? */
static struct _papi_events papi_events[MAX_PAPI_EVENTS];
long_long MutatorCounters[MAX_PAPI_EVENTS];
long_long GCCounters[MAX_PAPI_EVENTS];

long_long start_mutator_cycles;
long_long start_gc_cycles;
long_long mutator_cycles;
long_long gc_cycles;



static long_long papi_counter(long_long values[],int event);
static void papi_add_events(int EventSet);

/* If you want to add events to count, extend the
 * init_countable_events and the papi_report function.
 * Be aware that your processor can count a limited number
 * of events simultaneously, you can turn on multiplexing
 * to increase that number, though.
 */
static void
init_countable_events(void) 
{
#define PAPI_ADD_EVENT(EVENT)                           \
    {                                                   \
        if (n_papi_events >= MAX_PAPI_EVENTS) {         \
           barf("too many PAPI events");                \
        }                                               \
	papi_events[n_papi_events].event_code = EVENT;	\
	papi_events[n_papi_events].event_name = #EVENT; \
	n_papi_events++;				\
    }

    PAPI_ADD_EVENT(PAPI_TOT_INS);
    if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_BRANCH) {
	PAPI_ADD_EVENT(FR_BR);
	PAPI_ADD_EVENT(FR_BR_MIS);
	/* Docs are wrong? Opteron does not count indirect branch misses exclusively */
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
    } else {
	PAPI_ADD_EVENT(PAPI_STL_ICY);
    }

    // We might also consider:
    //  PAPI_BR_MSP     Conditional branch instructions mispredicted
    //  PAPI_RES_STL    Cycles stalled on any resource
};


static char temp[BIG_STRING_LEN];

static void
papi_mut_cycles(void)
{
    ullong_format_string(mutator_cycles,temp,rtsTrue/*commas*/); 
    statsPrintf("  (MUT_CYCLES)  : %s\n",temp);
}

static void
papi_gc_cycles(void)
{
    ullong_format_string(gc_cycles,temp,rtsTrue/*commas*/); 
    statsPrintf("  (GC_CYCLES)  : %s\n",temp);
}

/* This function reports counters for GC and mutator */
static void
papi_report(long_long PapiCounters[])
{

/* Report the value of a counter */
#define PAPI_REPORT(EVENTSET,EVENT) \
  { \
    ullong_format_string(papi_counter(EVENTSET,EVENT),temp,rtsTrue/*commas*/); \
    statsPrintf("  (" #EVENT ")  : %s\n",temp);				\
  }

/* Report the value of a counter as a percentage of another counter */
#define PAPI_REPORT_PCT(EVENTSET,EVENT,EVENTTOT) \
  statsPrintf("  (" #EVENT ") %% of (" #EVENTTOT ") : %.1f%%\n", \
	      papi_counter(EVENTSET,EVENT)*100.0/papi_counter(EVENTSET,EVENTTOT))

  /* I need to improve formatting aesthetics */
    PAPI_REPORT(PapiCounters,PAPI_TOT_INS);

    if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_BRANCH) {
	PAPI_REPORT(PapiCounters,FR_BR);
	PAPI_REPORT(PapiCounters,FR_BR_MIS);
	PAPI_REPORT_PCT(PapiCounters,FR_BR_MIS,FR_BR);
	PAPI_REPORT_PCT(PapiCounters,FR_BR_MISCOMPARE,FR_BR);
    }

    else if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_STALLS) {
	PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS);
	PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS_BR);
	//PAPI_REPORT_PCT(PapiCounters,FR_DISPATCH_STALLS_BR,PAPI_TOT_CYC);
	PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS_FULL_LS);
	//PAPI_REPORT_PCT(PapiCounters,FR_DISPATCH_STALLS_FULL_LS,PAPI_TOT_CYC);
    }

    else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L1) {
	PAPI_REPORT(PapiCounters,PAPI_L1_DCA);
	PAPI_REPORT(PapiCounters,PAPI_L1_DCM);
	PAPI_REPORT_PCT(PapiCounters,PAPI_L1_DCM,PAPI_L1_DCA);
    }

    else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L2) {
	PAPI_REPORT(PapiCounters,PAPI_L2_DCA);
	PAPI_REPORT(PapiCounters,PAPI_L2_DCM);
	PAPI_REPORT_PCT(PapiCounters,PAPI_L2_DCM,PAPI_L2_DCA);
    }

    else if (RtsFlags.PapiFlags.eventType==PAPI_FLAG_CB_EVENTS) {
	PAPI_REPORT(PapiCounters,DC_L2_REFILL_MOES);
	PAPI_REPORT(PapiCounters,DC_SYS_REFILL_MOES);
	PAPI_REPORT(PapiCounters,FR_BR_MIS);
    }

    else {
	PAPI_REPORT(PapiCounters,PAPI_STL_ICY);
    }        
}

void
papi_stats_report (void)
{
    statsPrintf("  -- CPU Mutator counters --\n");
    papi_mut_cycles();
    papi_report(MutatorCounters);
    
    statsPrintf("\n  -- CPU GC counters --\n");
    papi_gc_cycles();
    papi_report(GCCounters);
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
  int i;
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
  int i;
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
papi_stop_gc_count(void)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_accum(GCEvents,GCCounters));
    PAPI_CHECK( PAPI_stop(GCEvents,NULL));
    gc_cycles += PAPI_cycles() - start_gc_cycles;
    RELEASE_LOCK(&papi_counter_mutex);
}


void
papi_thread_start_gc_count(int event_set)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_start(event_set));
    RELEASE_LOCK(&papi_counter_mutex);
}

void
papi_thread_stop_gc_count(int event_set)
{
    ACQUIRE_LOCK(&papi_counter_mutex);
    PAPI_CHECK( PAPI_accum(event_set,GCCounters));
    PAPI_CHECK( PAPI_stop(event_set,NULL));
    RELEASE_LOCK(&papi_counter_mutex);
}

#endif /* USE_PAPI */
