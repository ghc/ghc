/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STATS_H
#define STATS_H

#include "GetTime.h"

#include "BeginPrivate.h"

#if defined(mingw32_HOST_OS)
/* On Win64, if we say "printf" then gcc thinks we are going to use
   MS format specifiers like %I64d rather than %llu */
#define PRINTF gnu_printf
#else
/* However, on OS X, "gnu_printf" isn't recognised */
#define PRINTF printf
#endif

struct gc_thread_;

void      stat_startInit(void);
void      stat_endInit(void);

void      stat_startGC(Capability *cap, struct gc_thread_ *_gct);
void      stat_endGC  (Capability *cap, struct gc_thread_ *_gct,
                       W_ live, W_ copied, W_ slop, nat gen,
                       nat n_gc_threads, W_ par_max_copied, W_ par_tot_copied);

void stat_gcWorkerThreadStart (struct gc_thread_ *_gct);
void stat_gcWorkerThreadDone  (struct gc_thread_ *_gct);

#ifdef PROFILING
void      stat_startRP(void);
void      stat_endRP(nat, 
#ifdef DEBUG_RETAINER
                            nat, int, 
#endif
                            double);
#endif /* PROFILING */

#if defined(PROFILING) || defined(DEBUG)
void      stat_startHeapCensus(void);
void      stat_endHeapCensus(void);
#endif

void      stat_startExit(void);
void      stat_endExit(void);

void      stat_exit(void);
void      stat_workerStop(void);

void      initStats0(void);
void      initStats1(void);

double    mut_user_time_until(Time t);
double    mut_user_time(void);

#ifdef PROFILING
double    mut_user_time_during_RP(void);
double    mut_user_time_during_heap_census(void);
#endif /* PROFILING */

void      statDescribeGens( void );

Time stat_getElapsedGCTime(void);
Time stat_getElapsedTime(void);

/* Only exported for Papi.c */
void statsPrintf( char *s, ... ) 
    GNUC3_ATTRIBUTE(format (PRINTF, 1, 2));

#include "EndPrivate.h"

#endif /* STATS_H */
