

#include <papi.h>



#define PAPI_CHECK(CALL) \
  if((papi_error=(CALL)) != PAPI_OK) { \
   debugBelch("PAPI function failed in module %s at line %d with error code %d\n", \
	      __FILE__,__LINE__,papi_error);				\
  }

/* Check the error value of a PAPI call, reporting an error, if needed */
extern int papi_error;

/* While PAPI reporting is going on this flag is on */
extern int papi_is_reporting;

/* Event sets and counter arrays for GC and mutator */

extern int MutatorEvents;
extern int GCEvents;

extern long_long MutatorCounters[];
extern long_long GCCounters[];

long_long papi_counter(long_long values[],int event);
void papi_report(long_long PapiCounters[]);
void papi_add_events(int EventSet);

void papi_init_eventsets(void);
void papi_start_mutator_count(void);
void papi_stop_mutator_count(void);
void papi_start_gc_count(void);
void papi_stop_gc_count(void);


