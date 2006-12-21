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

#include "Papi.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "RtsFlags.h"


struct _papi_events {
  int event_code;
  char * event_name;
};

#define PAPI_ADD_EVENT(EVENT) \
    {			      \
	ASSERT(n_papi_events<MAX_PAPI_EVENTS);	   \
	papi_events[n_papi_events].event_code = EVENT;	\
	papi_events[n_papi_events].event_name = #EVENT; \
	n_papi_events++;				\
    }

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
#define FR_DISPATCH_STALLS_BR 0x40000055
#define FR_DISPATCH_STALLS_FULL_LS 0x4000005b
#define DC_L2_REFILL_MOES 0x40001e1b
#define DC_SYS_REFILL_MOES 0x40001e1c

/* Number of counted events, computed from size of papi_events */
#define N_PAPI_EVENTS n_papi_events

/* This is bad, it should be in a header */
#define BIG_STRING_LEN 512

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



/* If you want to add events to count, extend the
 * init_countable_events and the papi_report function.
 * Be aware that your processor can count a limited number
 * of events simultaneously, you can turn on multiplexing
 * to increase that number, though.
 */
static void
init_countable_events(void) 
{
    PAPI_ADD_EVENT(PAPI_TOT_INS);
    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_BRANCH) {
	PAPI_ADD_EVENT(FR_BR);
	PAPI_ADD_EVENT(FR_BR_MIS);
	/* Docs are wrong? Opteron does not count indirect branch misses exclusively */
	PAPI_ADD_EVENT(FR_BR_MISCOMPARE);
    }
    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_STALLS) {
	PAPI_ADD_EVENT(FR_DISPATCH_STALLS_BR);
	PAPI_ADD_EVENT(FR_DISPATCH_STALLS_FULL_LS);
    }
    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L1) {
	PAPI_ADD_EVENT(PAPI_L1_DCA);
	PAPI_ADD_EVENT(PAPI_L1_DCM);
    }
    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L2) {
	PAPI_ADD_EVENT(PAPI_L2_DCA);
	PAPI_ADD_EVENT(PAPI_L2_DCM);
    }
    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_CB_EVENTS) {
	PAPI_ADD_EVENT(DC_L2_REFILL_MOES);
	PAPI_ADD_EVENT(DC_SYS_REFILL_MOES);
	PAPI_ADD_EVENT(FR_BR_MIS);
    }
};


static char temp[BIG_STRING_LEN];

void
papi_mut_cycles()
{
    ullong_format_string(mutator_cycles,temp,rtsTrue/*commas*/); 
    statsPrintf("  (MUT_CYCLES)  : %s\n",temp);
}

void
papi_gc_cycles()
{
    ullong_format_string(gc_cycles,temp,rtsTrue/*commas*/); 
    statsPrintf("  (GC_CYCLES)  : %s\n",temp);
}

/* This function reports counters for GC and mutator */
void
papi_report(long_long PapiCounters[])
{

    /* I need to improve formatting aesthetics */
    PAPI_REPORT(PapiCounters,PAPI_TOT_INS);

    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_BRANCH) {
	PAPI_REPORT(PapiCounters,FR_BR);
	PAPI_REPORT(PapiCounters,FR_BR_MIS);
	PAPI_REPORT_PCT(PapiCounters,FR_BR_MIS,FR_BR);
	PAPI_REPORT_PCT(PapiCounters,FR_BR_MISCOMPARE,FR_BR);
    }

    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_STALLS) {
	PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS_BR);
	//PAPI_REPORT_PCT(PapiCounters,FR_DISPATCH_STALLS_BR,PAPI_TOT_CYC);
	PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS_FULL_LS);
	//PAPI_REPORT_PCT(PapiCounters,FR_DISPATCH_STALLS_FULL_LS,PAPI_TOT_CYC);
    }

    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L1) {
	PAPI_REPORT(PapiCounters,PAPI_L1_DCA);
	PAPI_REPORT(PapiCounters,PAPI_L1_DCM);
	PAPI_REPORT_PCT(PapiCounters,PAPI_L1_DCM,PAPI_L1_DCA);
    }

    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_CACHE_L2) {
	PAPI_REPORT(PapiCounters,PAPI_L2_DCA);
	PAPI_REPORT(PapiCounters,PAPI_L2_DCM);
	PAPI_REPORT_PCT(PapiCounters,PAPI_L2_DCM,PAPI_L2_DCA);
    }

    if(RtsFlags.PapiFlags.eventType==PAPI_FLAG_CB_EVENTS) {
	PAPI_REPORT(PapiCounters,DC_L2_REFILL_MOES);
	PAPI_REPORT(PapiCounters,DC_SYS_REFILL_MOES);
	PAPI_REPORT(PapiCounters,FR_BR_MIS);
    }

}



void
papi_init_eventsets(void)
{

    init_countable_events();

    /* One event set for the mutator and another for the GC */
    PAPI_CHECK( PAPI_create_eventset(&MutatorEvents));
    PAPI_CHECK( PAPI_create_eventset(&GCEvents));

    /* Both sets contain the same events */
    papi_add_events(MutatorEvents);
    papi_add_events(GCEvents);

}

/* Extract the value corresponding to an event */
long_long
papi_counter(long_long values[],int event)
{
  int i;
  for(i=0;i<N_PAPI_EVENTS;i++) {
    if(papi_events[i].event_code==event) {
      return values[i];
    }
  }
  /* Passed a wrong event? */
  debugBelch("Event %d is not part of event set\n",event);
  return 0;
}

/* Add the events of papi_events into an event set */
void
papi_add_events(int EventSet)
{
  int i;
  for(i=0;i<N_PAPI_EVENTS;i++) {
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
    PAPI_CHECK( PAPI_start(MutatorEvents));
    start_mutator_cycles = PAPI_cycles();
}

void
papi_stop_mutator_count(void)
{
    mutator_cycles += PAPI_cycles() - start_mutator_cycles;
    PAPI_CHECK( PAPI_accum(MutatorEvents,MutatorCounters));
    PAPI_CHECK( PAPI_stop(MutatorEvents,NULL));
}

void
papi_start_gc_count(void)
{
      PAPI_CHECK( PAPI_start(GCEvents));
      start_gc_cycles = PAPI_cycles();
}

void
papi_stop_gc_count(void)
{
      gc_cycles += PAPI_cycles() - start_gc_cycles;
      PAPI_CHECK( PAPI_accum(GCEvents,GCCounters));
      PAPI_CHECK( PAPI_stop(GCEvents,NULL));
}


#endif /* USE_PAPI */
