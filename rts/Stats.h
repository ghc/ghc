/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "GetTime.h"
#include "sm/GC.h"
#include "Sparks.h"

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
                       W_ par_max_copied, W_ par_balanced_copied,
                       W_ gc_spin_spin, W_ gc_spin_yield, W_ mut_spin_spin,
                       W_ mut_spin_yield, W_ any_work, W_ no_work,
                       W_ scav_find_work);

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

typedef struct GenerationSummaryStats_ {
    uint32_t collections;
    uint32_t par_collections;
    Time cpu_ns;
    Time elapsed_ns;
    Time max_pause_ns;
    Time avg_pause_ns;
#if defined(THREADED_RTS) && defined(PROF_SPIN)
    uint64_t sync_spin;
    uint64_t sync_yield;
#endif
} GenerationSummaryStats;

typedef struct RTSSummaryStats_ {
    // These profiling times could potentially be in RTSStats. However, I'm not
    // confident enough to do this now, since there is some logic depending on
    // global state that I do not understand. (Or if I do understand it, it's
    // wrong)
    Time rp_cpu_ns;
    Time rp_elapsed_ns;
    Time hc_cpu_ns;
    Time hc_elapsed_ns;

    Time exit_cpu_ns;
    Time exit_elapsed_ns;

#if defined(THREADED_RTS)
    uint32_t bound_task_count;
    uint64_t sparks_count;
    SparkCounters sparks;
    double work_balance;
#else // THREADED_RTS
    double gc_cpu_percent;
    double gc_elapsed_percent;
#endif
    uint64_t fragmentation_bytes;
    uint64_t average_bytes_used; // This is not shown in the '+RTS -s' report
    uint64_t alloc_rate;
    double productivity_cpu_percent;
    double productivity_elapsed_percent;

    // one for each generation, 0 first
    GenerationSummaryStats* gc_summary_stats;
} RTSSummaryStats;

#include "EndPrivate.h"
