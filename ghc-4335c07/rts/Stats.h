/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

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

void      stat_startGCSync(struct gc_thread_ *_gct);
void      stat_startGC(Capability *cap, struct gc_thread_ *_gct);
void      stat_endGC  (Capability *cap, struct gc_thread_ *_gct, W_ live,
                       W_ copied, W_ slop, uint32_t gen, uint32_t n_gc_threads,
                       W_ par_max_copied, W_ par_balanced_copied);

#if defined(PROFILING)
void      stat_startRP(void);
void      stat_endRP(uint32_t,
#if defined(DEBUG_RETAINER)
                            uint32_t, int,
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

void      statDescribeGens( void );

Time      stat_getElapsedGCTime(void);
Time      stat_getElapsedTime(void);

#include "EndPrivate.h"
