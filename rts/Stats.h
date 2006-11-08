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

void      stat_startInit(void);
void      stat_endInit(void);

void      stat_startGC(void);
void      stat_endGC (lnat alloc, lnat live, 
		      lnat copied, lnat scavd_copied, lnat gen);

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

void      stat_exit(int alloc);
void      stat_workerStop(void);

void      initStats(void);

double    mut_user_time_during_GC(void);
double    mut_user_time(void);

#ifdef PROFILING
double    mut_user_time_during_RP(void);
double    mut_user_time_during_heap_census(void);
#endif /* PROFILING */

void      statDescribeGens( void );
HsInt64   getAllocations( void );

Ticks stat_getElapsedGCTime(void);
Ticks stat_getElapsedTime(void);

/* Only exported for Papi.c */
void statsPrintf( char *s, ... ) 
    GNUC3_ATTRIBUTE(format (printf, 1, 2));


#endif /* STATS_H */
