

#include "Papi.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "Stats.h"


/* These constants specify which events to keep track of.
 * Probably it is better to count one set of events at a time.
 * The reason is that processors have limited counters and
 * multiplexing is not enabled (yet).
 */
#define PAPI_COUNT_BRANCHES 0
/* The one below is Opteron specific.
 */
#define PAPI_COUNT_STALLS 0
#define PAPI_COUNT_DCACHE1_MISSES 1
#define PAPI_COUNT_DCACHE2_MISSES 0

struct _papi_events {
  int event_code;
  char * event_name;
};

#define PAPI_ADD_EVENT(EVENT) { EVENT, #EVENT }

/* Beware, these counters are Opteron specific */
#define FR_BR 0x40000040
#define FR_BR_MIS 0x40000041
#define FR_BR_MISCOMPARE 0x40000048
#define DC_ACCESS 0x40000019
#define DC_MISS 0x4000001a
#define FR_DISPATCH_STALLS_BR 0x40000055
#define FR_DISPATCH_STALLS_FULL_LS 0x4000005b

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

/* Number of counted events, computed from size of papi_events */
#define N_PAPI_EVENTS ((int)(sizeof(papi_events)/sizeof(struct _papi_events)))

/* This is bad, it should be in a header */
#define BIG_STRING_LEN 512

/* While PAPI reporting is going on this flag is on */
int papi_is_reporting;

/* Event sets and counter arrays for GC and mutator */

int MutatorEvents = PAPI_NULL;
int GCEvents = PAPI_NULL;

int papi_error;


/* If you want to add events to count, extend the
 * papi_events array and the papi_report function.
 */

/* Events counted during GC and Mutator execution */
/* There's a trailing comma, do all C compilers accept that? */
static struct _papi_events papi_events[] = {
  PAPI_ADD_EVENT(PAPI_TOT_CYC),
#if PAPI_COUNT_BRANCHES
  PAPI_ADD_EVENT(FR_BR),
  PAPI_ADD_EVENT(FR_BR_MIS),
  /* Docs are wrong? Opteron does not count indirect branch misses apparently */
  PAPI_ADD_EVENT(FR_BR_MISCOMPARE),
#endif
#if PAPI_COUNT_STALLS
  PAPI_ADD_EVENT(FR_DISPATCH_STALLS_BR),
  PAPI_ADD_EVENT(FR_DISPATCH_STALLS_FULL_LS),
#endif
#if PAPI_COUNT_DCACHE1_MISSES
  PAPI_ADD_EVENT(PAPI_L1_DCA),
  PAPI_ADD_EVENT(PAPI_L1_DCM),
#endif
#if PAPI_COUNT_DCACHE2_MISSES
  PAPI_ADD_EVENT(PAPI_L2_DCA),
  PAPI_ADD_EVENT(PAPI_L2_DCM),
#endif
};

long_long MutatorCounters[N_PAPI_EVENTS];
long_long GCCounters[N_PAPI_EVENTS];


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

/* This function reports counters for GC and mutator */
void
papi_report(long_long PapiCounters[])
{
    char temp[BIG_STRING_LEN];

    /* I need to improve formatting aesthetics */
    PAPI_REPORT(PapiCounters,PAPI_TOT_CYC);
#if PAPI_COUNT_BRANCHES
    PAPI_REPORT(PapiCounters,FR_BR);
    PAPI_REPORT(PapiCounters,FR_BR_MIS);
    PAPI_REPORT_PCT(PapiCounters,FR_BR_MIS,FR_BR);
    PAPI_REPORT_PCT(PapiCounters,FR_BR_MISCOMPARE,FR_BR);
#endif
#if PAPI_COUNT_STALLS
    PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS_BR);
    PAPI_REPORT_PCT(PapiCounters,FR_DISPATCH_STALLS_BR,PAPI_TOT_CYC);
    PAPI_REPORT(PapiCounters,FR_DISPATCH_STALLS_FULL_LS);
    PAPI_REPORT_PCT(PapiCounters,FR_DISPATCH_STALLS_FULL_LS,PAPI_TOT_CYC);
#endif
#if PAPI_COUNT_DCACHE1_MISSES
    PAPI_REPORT(PapiCounters,PAPI_L1_DCA);
    PAPI_REPORT(PapiCounters,PAPI_L1_DCM);
    PAPI_REPORT_PCT(PapiCounters,PAPI_L1_DCM,PAPI_L1_DCA);
#endif
#if PAPI_COUNT_DCACHE2_MISSES
    PAPI_REPORT(PapiCounters,PAPI_L2_DCA);
    PAPI_REPORT(PapiCounters,PAPI_L2_DCM);
    PAPI_REPORT_PCT(PapiCounters,PAPI_L2_DCM,PAPI_L2_DCA);
#endif
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

void
papi_init_eventsets(void)
{

    /* One event set for the mutator and another for the GC */
    PAPI_CHECK( PAPI_create_eventset(&MutatorEvents));
    PAPI_CHECK( PAPI_create_eventset(&GCEvents));

    /* Both sets contain the same events */
    papi_add_events(MutatorEvents);
    papi_add_events(GCEvents);

}

void
papi_start_mutator_count(void)
{
    PAPI_CHECK( PAPI_start(MutatorEvents));
}

void
papi_stop_mutator_count(void)
{
    PAPI_CHECK( PAPI_accum(MutatorEvents,MutatorCounters));
    PAPI_CHECK( PAPI_stop(MutatorEvents,NULL));
}

void
papi_start_gc_count(void)
{
      PAPI_CHECK( PAPI_start(GCEvents));
}

void
papi_stop_gc_count(void)
{
      PAPI_CHECK( PAPI_accum(GCEvents,GCCounters));
      PAPI_CHECK( PAPI_stop(GCEvents,NULL));
}
