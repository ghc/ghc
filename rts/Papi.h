/* -----------------------------------------------------------------------------
 * (c) The GHC Team 2006
 *
 * Initialization and use of the PAPI performance monitoring library
 *
 * ---------------------------------------------------------------------------*/

#ifndef PAPI_H
#define PAPI_H

#include "BeginPrivate.h"

/* Check the error value of a PAPI call, reporting an error, if needed */
extern int papi_error;

/* While PAPI reporting is going on this flag is on */
extern int papi_is_reporting;

void papi_stats_report(void);
void papi_init_eventset(int * event_set);
void papi_init(void);
void papi_start_mutator_count(void);
void papi_stop_mutator_count(void);

void papi_start_gc_count(void);
void papi_stop_gc0_count(void);
void papi_stop_gc1_count(void);

// for multithreaded GC, each sub-thread uses these functions to count
// events and aggregate them into the main GC counters.
void papi_thread_start_gc1_count(int event_set);
void papi_thread_stop_gc1_count(int event_set);

#include "EndPrivate.h"

#endif /* PAPI_H */
