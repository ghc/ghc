/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Stats.h"
#include "Profiling.h"
#include "GetTime.h"
#include "sm/Storage.h"
#include "sm/GCThread.h"
#include "sm/BlockAlloc.h"

// for spin/yield counters
#include "sm/GC.h"
#include "ThreadPaused.h"
#include "Messages.h"

#include <string.h> // for memset

#if defined(THREADED_RTS)
// Protects all statistics below
Mutex stats_mutex;
#endif

static Time
    start_init_cpu, start_init_elapsed,
    end_init_cpu,   end_init_elapsed,
    start_exit_cpu, start_exit_elapsed,
    start_exit_gc_elapsed, start_exit_gc_cpu,
    end_exit_cpu,   end_exit_elapsed,
    start_nonmoving_gc_cpu, start_nonmoving_gc_elapsed,
    start_nonmoving_gc_sync_elapsed;

#if defined(PROFILING)
static Time RP_start_time  = 0, RP_tot_time  = 0;  // retainer prof user time
static Time RPe_start_time = 0, RPe_tot_time = 0;  // retainer prof elap time

static Time HC_start_time, HC_tot_time = 0;     // heap census prof user time
static Time HCe_start_time, HCe_tot_time = 0;   // heap census prof elap time
#endif

#if defined(PROF_SPIN)
volatile StgWord64 whitehole_lockClosure_spin = 0;
volatile StgWord64 whitehole_lockClosure_yield = 0;
volatile StgWord64 whitehole_threadPaused_spin = 0;
volatile StgWord64 whitehole_executeMessage_spin = 0;
#endif

//
// All the stats!
//
// This is where we accumulate all the stats during execution, and it's also
// in a convenient form that we can copy over to a caller of getRTSStats().
//
static RTSStats stats;

static W_ GC_end_faults = 0;

static Time *GC_coll_cpu = NULL;
static Time *GC_coll_elapsed = NULL;
static Time *GC_coll_max_pause = NULL;

static int statsPrintf( char *s, ... ) STG_PRINTF_ATTR(1, 2);
static void statsFlush( void );
static void statsClose( void );

/* -----------------------------------------------------------------------------
   Current elapsed time
   ------------------------------------------------------------------------- */

Time stat_getElapsedTime(void)
{
    return getProcessElapsedTime() - start_init_elapsed;
}

/* ---------------------------------------------------------------------------
   Measure the current MUT time, for profiling
   ------------------------------------------------------------------------ */

#if defined(PROFILING)
/*
  mut_user_time_during_RP() returns the MUT time during retainer profiling.
  The same is for mut_user_time_during_HC();
 */
static double
mut_user_time_during_RP( void )
{
    return TimeToSecondsDbl(RP_start_time - stats.gc_cpu_ns - RP_tot_time);
}

#endif /* PROFILING */

/* ---------------------------------------------------------------------------
   initStats0() has no dependencies, it can be called right at the beginning
   ------------------------------------------------------------------------ */

void
initStats0(void)
{
#if defined(THREADED_RTS)
    initMutex(&stats_mutex);
#endif

    start_init_cpu    = 0;
    start_init_elapsed = 0;
    end_init_cpu     = 0;
    end_init_elapsed  = 0;

    start_nonmoving_gc_cpu = 0;
    start_nonmoving_gc_elapsed = 0;
    start_nonmoving_gc_sync_elapsed = 0;

    start_exit_cpu    = 0;
    start_exit_elapsed = 0;
    start_exit_gc_cpu    = 0;
    start_exit_gc_elapsed = 0;
    end_exit_cpu     = 0;
    end_exit_elapsed  = 0;

#if defined(PROFILING)
    RP_start_time  = 0;
    RP_tot_time  = 0;
    RPe_start_time = 0;
    RPe_tot_time = 0;

    HC_start_time = 0;
    HC_tot_time = 0;
    HCe_start_time = 0;
    HCe_tot_time = 0;
#endif

    GC_end_faults = 0;

    stats = (RTSStats) {
        .gcs = 0,
        .major_gcs = 0,
        .allocated_bytes = 0,
        .max_live_bytes = 0,
        .max_large_objects_bytes = 0,
        .max_compact_bytes = 0,
        .max_slop_bytes = 0,
        .max_mem_in_use_bytes = 0,
        .cumulative_live_bytes = 0,
        .copied_bytes = 0,
        .par_copied_bytes = 0,
        .cumulative_par_max_copied_bytes = 0,
        .cumulative_par_balanced_copied_bytes = 0,
        .any_work = 0,
        .scav_find_work = 0,
        .max_n_todo_overflow = 0,
        .init_cpu_ns = 0,
        .init_elapsed_ns = 0,
        .mutator_cpu_ns = 0,
        .mutator_elapsed_ns = 0,
        .gc_cpu_ns = 0,
        .gc_elapsed_ns = 0,
        .cpu_ns = 0,
        .elapsed_ns = 0,
        .nonmoving_gc_cpu_ns = 0,
        .nonmoving_gc_elapsed_ns = 0,
        .nonmoving_gc_max_elapsed_ns = 0,
        .nonmoving_gc_sync_elapsed_ns = 0,
        .nonmoving_gc_sync_max_elapsed_ns = 0,
        .gc = {
            .gen = 0,
            .threads = 0,
            .allocated_bytes = 0,
            .live_bytes = 0,
            .large_objects_bytes = 0,
            .compact_bytes = 0,
            .slop_bytes = 0,
            .mem_in_use_bytes = 0,
            .copied_bytes = 0,
            .par_max_copied_bytes = 0,
            .par_balanced_copied_bytes = 0,
            .block_fragmentation_bytes = 0,
            .sync_elapsed_ns = 0,
            .cpu_ns = 0,
            .elapsed_ns = 0,
            .nonmoving_gc_cpu_ns = 0,
            .nonmoving_gc_elapsed_ns = 0,
            .nonmoving_gc_sync_elapsed_ns = 0,
        }
    };
}

/* ---------------------------------------------------------------------------
   initStats1() can be called after setupRtsFlags()
   ------------------------------------------------------------------------ */

void initGenerationStats(void);

void
initStats1 (void)
{
    if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
        statsPrintf("    Alloc    Copied     Live     GC     GC      TOT      TOT  Page Flts\n");
        statsPrintf("    bytes     bytes     bytes   user   elap     user     elap\n");
    }
    GC_coll_cpu =
        (Time *)stgMallocBytes(
            sizeof(Time)*RtsFlags.GcFlags.generations,
            "initStats");
    GC_coll_elapsed =
        (Time *)stgMallocBytes(
            sizeof(Time)*RtsFlags.GcFlags.generations,
            "initStats");
    GC_coll_max_pause =
        (Time *)stgMallocBytes(
            sizeof(Time)*RtsFlags.GcFlags.generations,
            "initStats");
    initGenerationStats();
}

void
initGenerationStats(void)
{
    for (uint32_t i = 0; i < RtsFlags.GcFlags.generations; i++) {
        GC_coll_cpu[i] = 0;
        GC_coll_elapsed[i] = 0;
        GC_coll_max_pause[i] = 0;
    }
}

/* ---------------------------------------------------------------------------
   Reset stats of child process after fork()
   ------------------------------------------------------------------------ */

void resetChildProcessStats(void)
{
    initStats0();
    initGenerationStats();
}

/* -----------------------------------------------------------------------------
   Initialisation time...
   -------------------------------------------------------------------------- */

void
stat_startInit(void)
{
    getProcessTimes(&start_init_cpu, &start_init_elapsed);
}

void
stat_endInit(void)
{
    getProcessTimes(&end_init_cpu, &end_init_elapsed);
    stats.init_cpu_ns = end_init_cpu - start_init_cpu;
    stats.init_elapsed_ns = end_init_elapsed - start_init_elapsed;
}

/* -----------------------------------------------------------------------------
   stat_startExit and stat_endExit

   These two measure the time taken in shutdownHaskell().
   -------------------------------------------------------------------------- */

void
stat_startExit(void)
{
    ACQUIRE_LOCK(&stats_mutex);
    getProcessTimes(&start_exit_cpu, &start_exit_elapsed);
    start_exit_gc_elapsed = stats.gc_elapsed_ns;
    start_exit_gc_cpu = stats.gc_cpu_ns;
    RELEASE_LOCK(&stats_mutex);
}

/* -----------------------------------------------------------------------------
   Nonmoving (concurrent) collector statistics

   These two measure the time taken in the concurrent mark & sweep collector.
   -------------------------------------------------------------------------- */
void
stat_endExit(void)
{
    ACQUIRE_LOCK(&stats_mutex);
    getProcessTimes(&end_exit_cpu, &end_exit_elapsed);
    RELEASE_LOCK(&stats_mutex);
}

void
stat_startGCSync (gc_thread *gct)
{
    gct->gc_sync_start_elapsed = getProcessElapsedTime();
}

void
stat_startNonmovingGc (void)
{
    ACQUIRE_LOCK(&stats_mutex);
    start_nonmoving_gc_cpu = getCurrentThreadCPUTime();
    start_nonmoving_gc_elapsed = getProcessElapsedTime();
    RELEASE_LOCK(&stats_mutex);
}

void
stat_endNonmovingGc (void)
{
    Time cpu = getCurrentThreadCPUTime();
    Time elapsed = getProcessElapsedTime();

    ACQUIRE_LOCK(&stats_mutex);
    stats.gc.nonmoving_gc_elapsed_ns = elapsed - start_nonmoving_gc_elapsed;
    stats.nonmoving_gc_elapsed_ns += stats.gc.nonmoving_gc_elapsed_ns;

    stats.gc.nonmoving_gc_cpu_ns = cpu - start_nonmoving_gc_cpu;
    stats.nonmoving_gc_cpu_ns += stats.gc.nonmoving_gc_cpu_ns;

    stats.nonmoving_gc_max_elapsed_ns =
      stg_max(stats.gc.nonmoving_gc_elapsed_ns,
              stats.nonmoving_gc_max_elapsed_ns);
    RELEASE_LOCK(&stats_mutex);
}

void
stat_startNonmovingGcSync (void)
{
    ACQUIRE_LOCK(&stats_mutex);
    start_nonmoving_gc_sync_elapsed = getProcessElapsedTime();
    RELEASE_LOCK(&stats_mutex);
    traceConcSyncBegin();
}

void
stat_endNonmovingGcSync (void)
{
    Time end_elapsed = getProcessElapsedTime();
    ACQUIRE_LOCK(&stats_mutex);
    stats.gc.nonmoving_gc_sync_elapsed_ns = end_elapsed - start_nonmoving_gc_sync_elapsed;
    stats.nonmoving_gc_sync_elapsed_ns +=  stats.gc.nonmoving_gc_sync_elapsed_ns;
    stats.nonmoving_gc_sync_max_elapsed_ns =
      stg_max(stats.gc.nonmoving_gc_sync_elapsed_ns,
              stats.nonmoving_gc_sync_max_elapsed_ns);
    Time sync_elapsed = stats.gc.nonmoving_gc_sync_elapsed_ns;
    RELEASE_LOCK(&stats_mutex);

    if (RtsFlags.GcFlags.giveStats == VERBOSE_GC_STATS) {
      statsPrintf("# sync %6.3f\n", TimeToSecondsDbl(sync_elapsed));
    }
    traceConcSyncEnd();
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each GC
   -------------------------------------------------------------------------- */

/*
 * Note [Time accounting]
 * ~~~~~~~~~~~~~~~~~~~~~~
 * In the "vanilla" configuration (using the standard copying GC) GHC keeps
 * track of a two different sinks of elapsed and CPU time:
 *
 *  - time spent synchronising to initiate garbage collection
 *  - garbage collection (per generation)
 *  - mutation
 *
 * When using the (concurrent) non-moving garbage collector (see Note
 * [Non-moving garbage collector]) we also track a few more sinks:
 *
 *  - minor GC
 *  - major GC (namly time spent in the preparatory phase)
 *  - concurrent mark
 *  - final synchronization (elapsed only)
 *  - mutation
 *
 * To keep track of these CPU times we rely on the system's per-thread CPU time
 * clock (exposed via the runtime's getCurrentThreadCPUTime utility).
 *
 * CPU time spent in the copying garbage collector is tracked in each GC
 * worker's gc_thread struct. At the beginning of scavenging each worker
 * records its OS thread's CPU time its gc_thread (by stat_startGCWorker). At
 * the end of scavenging we again record the CPU time (in stat_endGCworker).
 * The differences of these are then summed over by the thread leading the GC
 * at the end of collection in stat_endGC. By contrast, the elapsed time is
 * recorded only by the leader.
 *
 * Mutator time is derived from the process's CPU time, subtracting out
 * contributions from stop-the-world and concurrent GCs.
 *
 * Time spent in concurrent marking is recorded by stat_{start,end}NonmovingGc.
 * Likewise, elapsed time spent in the final synchronization is recorded by
 * stat_{start,end}NonmovingGcSync.
 */

void
stat_startGCWorker (Capability *cap STG_UNUSED, gc_thread *gct)
{
    bool stats_enabled =
        RtsFlags.GcFlags.giveStats != NO_GC_STATS ||
        rtsConfig.gcDoneHook != NULL;

    if (stats_enabled || RtsFlags.ProfFlags.doHeapProfile) {
        gct->gc_start_cpu = getCurrentThreadCPUTime();
    }
}

void
stat_endGCWorker (Capability *cap STG_UNUSED, gc_thread *gct)
{
    bool stats_enabled =
        RtsFlags.GcFlags.giveStats != NO_GC_STATS ||
        rtsConfig.gcDoneHook != NULL;

    if (stats_enabled || RtsFlags.ProfFlags.doHeapProfile) {
        gct->gc_end_cpu = getCurrentThreadCPUTime();
        ASSERT(gct->gc_end_cpu >= gct->gc_start_cpu);
    }
}

void
stat_startGC (Capability *cap, gc_thread *gct)
{
    if (RtsFlags.GcFlags.ringBell) {
        debugBelch("\007");
    }

    bool stats_enabled =
        RtsFlags.GcFlags.giveStats != NO_GC_STATS ||
        rtsConfig.gcDoneHook != NULL;

    if (stats_enabled || RtsFlags.ProfFlags.doHeapProfile) {
        gct->gc_start_cpu = getCurrentThreadCPUTime();
    }

    gct->gc_start_elapsed = getProcessElapsedTime();

    // Post EVENT_GC_START with the same timestamp as used for stats
    // (though converted from Time=StgInt64 to EventTimestamp=StgWord64).
    // Here, as opposed to other places, the event is emitted on the cap
    // that initiates the GC and external tools expect it to have the same
    // timestamp as used in +RTS -s calculations.
    traceEventGcStartAtT(cap,
                         TimeToNS(gct->gc_start_elapsed - start_init_elapsed));

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS)
    {
        gct->gc_start_faults = getPageFaults();
    }

    updateNurseriesStats();
}

/* -----------------------------------------------------------------------------
   Called at the end of each GC
   -------------------------------------------------------------------------- */

void
stat_endGC (Capability *cap, gc_thread *initiating_gct, W_ live, W_ copied, W_ slop,
            uint32_t gen, uint32_t par_n_threads, gc_thread **gc_threads,
            W_ par_max_copied, W_ par_balanced_copied, W_ any_work,
            W_ scav_find_work, W_ max_n_todo_overflow)
{
    ACQUIRE_LOCK(&stats_mutex);

    // -------------------------------------------------
    // Collect all the stats about this GC in stats.gc. We always do this since
    // it's relatively cheap and we need allocated_bytes to catch heap
    // overflows.

    stats.gc.gen = gen;
    stats.gc.threads = par_n_threads;

    uint64_t tot_alloc_bytes = calcTotalAllocated() * sizeof(W_);

    // allocated since the last GC
    stats.gc.allocated_bytes = tot_alloc_bytes - stats.allocated_bytes;

    stats.gc.live_bytes = live * sizeof(W_);
    stats.gc.large_objects_bytes = calcTotalLargeObjectsW() * sizeof(W_);
    stats.gc.compact_bytes = calcTotalCompactW() * sizeof(W_);
    stats.gc.slop_bytes = slop * sizeof(W_);
    stats.gc.mem_in_use_bytes = mblocks_allocated * MBLOCK_SIZE;
    stats.gc.copied_bytes = copied * sizeof(W_);
    stats.gc.par_max_copied_bytes = par_max_copied * sizeof(W_);
    stats.gc.par_balanced_copied_bytes = par_balanced_copied * sizeof(W_);
    stats.gc.block_fragmentation_bytes =
        (mblocks_allocated * BLOCKS_PER_MBLOCK
         - n_alloc_blocks) * BLOCK_SIZE;

    bool stats_enabled =
        RtsFlags.GcFlags.giveStats != NO_GC_STATS ||
        rtsConfig.gcDoneHook != NULL;

    if (stats_enabled
      || RtsFlags.ProfFlags.doHeapProfile) // heap profiling needs GC_tot_time
    {
        // We only update the times when stats are explicitly enabled since
        // getProcessTimes (e.g. requiring a system call) can be expensive on
        // some platforms.
        Time current_cpu, current_elapsed;
        getProcessTimes(&current_cpu, &current_elapsed);
        stats.cpu_ns = current_cpu - start_init_cpu;
        stats.elapsed_ns = current_elapsed - start_init_elapsed;

        stats.gc.sync_elapsed_ns =
            initiating_gct->gc_start_elapsed - initiating_gct->gc_sync_start_elapsed;
        stats.gc.elapsed_ns = current_elapsed - initiating_gct->gc_start_elapsed;
        stats.gc.cpu_ns = 0;
        // see Note [n_gc_threads]
        // par_n_threads is set to n_gc_threads at the single callsite of this
        // function
        for (unsigned int i=0; i < par_n_threads; i++) {
            gc_thread *gct = gc_threads[i];
            ASSERT(gct->gc_end_cpu >= gct->gc_start_cpu);
            stats.gc.cpu_ns += gct->gc_end_cpu - gct->gc_start_cpu;
            // Reset the start/end times to zero after the difference has been
            // added to the global total (see #21082)
            // Resetting the values here is the most direct way to avoid double counting
            // because the gc_end_cpu and gc_start_cpu is not reset to zero as
            // stat_startGC is only called on active (not idle threads).
            gct->gc_end_cpu = 0;
            gct->gc_start_cpu = 0;
        }
    }
    // -------------------------------------------------
    // Update the cumulative stats

    stats.gcs++;
    stats.allocated_bytes = tot_alloc_bytes;
    stats.max_mem_in_use_bytes = peak_mblocks_allocated * MBLOCK_SIZE;

    GC_coll_cpu[gen] += stats.gc.cpu_ns;
    GC_coll_elapsed[gen] += stats.gc.elapsed_ns;
    if (GC_coll_max_pause[gen] < stats.gc.elapsed_ns) {
        GC_coll_max_pause[gen] = stats.gc.elapsed_ns;
    }

    stats.copied_bytes += stats.gc.copied_bytes;
    if (par_n_threads > 1) {
        stats.par_copied_bytes += stats.gc.copied_bytes;
        stats.cumulative_par_max_copied_bytes +=
            stats.gc.par_max_copied_bytes;
        stats.cumulative_par_balanced_copied_bytes +=
            stats.gc.par_balanced_copied_bytes;
        stats.any_work += any_work;
        stats.scav_find_work += scav_find_work;
        stats.max_n_todo_overflow += stg_max(max_n_todo_overflow, stats.max_n_todo_overflow);
    }
    stats.gc_cpu_ns += stats.gc.cpu_ns;
    stats.gc_elapsed_ns += stats.gc.elapsed_ns;

    if (gen == RtsFlags.GcFlags.generations-1) { // major GC?
        stats.major_gcs++;
        if (stats.gc.live_bytes > stats.max_live_bytes) {
            stats.max_live_bytes = stats.gc.live_bytes;
        }
        if (stats.gc.large_objects_bytes > stats.max_large_objects_bytes) {
            stats.max_large_objects_bytes = stats.gc.large_objects_bytes;
        }
        if (stats.gc.compact_bytes > stats.max_compact_bytes) {
            stats.max_compact_bytes = stats.gc.compact_bytes;
        }
        if (stats.gc.slop_bytes > stats.max_slop_bytes) {
            stats.max_slop_bytes = stats.gc.slop_bytes;
        }
        stats.cumulative_live_bytes += stats.gc.live_bytes;
    }

    // -------------------------------------------------
    // Do the more expensive bits only when stats are enabled.

    if (stats_enabled)
    {
        // -------------------------------------------------
        // Emit events to the event log

        // Has to be emitted while all caps stopped for GC, but before GC_END.
        // See https://gitlab.haskell.org/ghc/ghc/-/wikis/RTSsummaryEvents
        // for a detailed design rationale of the current setup
        // of GC eventlog events.
        traceEventGcGlobalSync(cap);

        // Emitted before GC_END on all caps, which simplifies tools code.
        traceEventGcStats(cap,
                          CAPSET_HEAP_DEFAULT,
                          stats.gc.gen,
                          stats.gc.copied_bytes,
                          stats.gc.slop_bytes,
                          stats.gc.block_fragmentation_bytes,
                          par_n_threads,
                          stats.gc.par_max_copied_bytes,
                          stats.gc.copied_bytes,
                          stats.gc.par_balanced_copied_bytes);

        // Post EVENT_GC_END with the same timestamp as used for stats
        // (though converted from Time=StgInt64 to EventTimestamp=StgWord64).
        // Here, as opposed to other places, the event is emitted on the cap
        // that initiates the GC and external tools expect it to have the same
        // timestamp as used in +RTS -s calculations.
        traceEventGcEndAtT(cap, TimeToNS(stats.elapsed_ns));

        if (gen == RtsFlags.GcFlags.generations-1) { // major GC?
            traceEventHeapLive(cap,
                               CAPSET_HEAP_DEFAULT,
                               stats.gc.live_bytes);
        }

        // -------------------------------------------------
        // Print GC stats to stdout or a file (+RTS -S/-s)

        if (RtsFlags.GcFlags.giveStats == VERBOSE_GC_STATS) {
            W_ faults = getPageFaults();

            statsPrintf("%9" FMT_Word64 " %9" FMT_Word64 " %9" FMT_Word64,
                        stats.gc.allocated_bytes, stats.gc.copied_bytes,
                        stats.gc.live_bytes);

            statsPrintf(" %6.3f %6.3f %8.3f %8.3f %4"
                        FMT_Word " %4" FMT_Word "  (Gen: %2d)\n",
                    TimeToSecondsDbl(stats.gc.cpu_ns),
                    TimeToSecondsDbl(stats.gc.elapsed_ns),
                    TimeToSecondsDbl(stats.cpu_ns),
                    TimeToSecondsDbl(stats.elapsed_ns),
                    faults - initiating_gct->gc_start_faults,
                        initiating_gct->gc_start_faults - GC_end_faults,
                    gen);

            GC_end_faults = faults;
            statsFlush();
        }


        if (rtsConfig.gcDoneHook != NULL) {
            rtsConfig.gcDoneHook(&stats.gc);
        }

        traceEventHeapSize(cap,
                           CAPSET_HEAP_DEFAULT,
                           mblocks_allocated * MBLOCK_SIZE);
        traceEventBlocksSize(cap,
                           CAPSET_HEAP_DEFAULT,
                           n_alloc_blocks * BLOCK_SIZE);
    }
    RELEASE_LOCK(&stats_mutex);
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each Retainer Profiling
   -------------------------------------------------------------------------- */
#if defined(PROFILING)
void
stat_startRP(void)
{
    Time user, elapsed;
    getProcessTimes( &user, &elapsed );

    ACQUIRE_LOCK(&stats_mutex);
    RP_start_time = user;
    RPe_start_time = elapsed;
    RELEASE_LOCK(&stats_mutex);
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of each Retainer Profiling
   -------------------------------------------------------------------------- */

#if defined(PROFILING)
void
stat_endRP(
  uint32_t retainerGeneration,
  int maxStackSize,
  double averageNumVisit)
{
    Time user, elapsed;
    getProcessTimes( &user, &elapsed );

    ACQUIRE_LOCK(&stats_mutex);
    RP_tot_time += user - RP_start_time;
    RPe_tot_time += elapsed - RPe_start_time;
    double mut_time_during_RP = mut_user_time_during_RP();
    RELEASE_LOCK(&stats_mutex);

    fprintf(prof_file, "Retainer Profiling: %d, at %f seconds\n",
            retainerGeneration, mut_time_during_RP);
    fprintf(prof_file, "\tMax auxiliary stack size = %u\n", maxStackSize);
    fprintf(prof_file, "\tAverage number of visits per object = %f\n",
            averageNumVisit);
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the beginning of each heap census
   -------------------------------------------------------------------------- */
#if defined(PROFILING)
void
stat_startHeapCensus(void)
{
    Time user, elapsed;
    getProcessTimes( &user, &elapsed );

    ACQUIRE_LOCK(&stats_mutex);
    HC_start_time = user;
    HCe_start_time = elapsed;
    RELEASE_LOCK(&stats_mutex);
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of each heap census
   -------------------------------------------------------------------------- */
#if defined(PROFILING)
void
stat_endHeapCensus(void)
{
    Time user, elapsed;
    getProcessTimes( &user, &elapsed );

    ACQUIRE_LOCK(&stats_mutex);
    HC_tot_time += user - HC_start_time;
    HCe_tot_time += elapsed - HCe_start_time;
    RELEASE_LOCK(&stats_mutex);
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of execution

   NOTE: number of allocations is not entirely accurate: it doesn't
   take into account the few bytes at the end of the heap that
   were left unused when the heap-check failed.
   -------------------------------------------------------------------------- */

#if defined(DEBUG)
#define TICK_VAR_INI(arity) \
  StgInt SLOW_CALLS_##arity = 1; \
  StgInt RIGHT_ARITY_##arity = 1; \
  StgInt TAGGED_PTR_##arity = 0;

TICK_VAR_INI(1)
TICK_VAR_INI(2)

StgInt TOTAL_CALLS=1;
#endif

/* Report the value of a counter */
#define REPORT(counter) \
  { \
    showStgWord64(counter,temp,true/*commas*/); \
    statsPrintf("  (" #counter ")  : %s\n",temp); \
  }

/* Report the value of a counter as a percentage of another counter */
#define REPORT_PCT(counter,countertot) \
  statsPrintf("  (" #counter ") %% of (" #countertot ") : %.1f%%\n", \
              counter*100.0/countertot)

#define TICK_PRINT(arity) \
  REPORT(SLOW_CALLS_##arity); \
  REPORT_PCT(RIGHT_ARITY_##arity,SLOW_CALLS_##arity); \
  REPORT_PCT(TAGGED_PTR_##arity,RIGHT_ARITY_##arity); \
  REPORT(RIGHT_ARITY_##arity); \
  REPORT(TAGGED_PTR_##arity)

#define TICK_PRINT_TOT(arity) \
  statsPrintf("  (SLOW_CALLS_" #arity ") %% of (TOTAL_CALLS) : %.1f%%\n", \
              SLOW_CALLS_##arity * 100.0/TOTAL_CALLS)

/*
Note [RTS Stats Reporting]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are currently three reporting functions:
  * report_summary:
      Responsible for producing '+RTS -s' output.
      Will report internal counters if the RTS flag --internal-counters is
      passed. See [Internal Counters Stats]
  * report_machine_readable:
      Responsible for producing '+RTS -t --machine-readable' output.
  * report_one_line:
      Responsible for producing '+RTS -t' output

Stats are accumulated into the global variable 'stats' as the program runs, then
in 'stat_exit' we do the following:
  * Finalise 'stats'. This involves setting final running times and allocations
    that have not yet been accounted for.
  * Create a RTSSummaryStats. This contains all data for reports that is not
    included in stats (because they do not make sense before the program has
    completed) or in a global variable.
  * call the appropriate report_* function, passing the newly constructed
    RTSSummaryStats.

To ensure that the data in the different reports is kept consistent, the
report_* functions should not do any calculation, excepting unit changes and
formatting. If you need to add a new calculated field, add it to
RTSSummaryStats.

*/


static void init_RTSSummaryStats(RTSSummaryStats* sum)
{
    const size_t sizeof_gc_summary_stats =
      RtsFlags.GcFlags.generations * sizeof(GenerationSummaryStats);

    memset(sum, 0, sizeof(RTSSummaryStats));
    sum->gc_summary_stats =
      stgMallocBytes(sizeof_gc_summary_stats,
                     "alloc_RTSSummaryStats.gc_summary_stats");
    memset(sum->gc_summary_stats, 0, sizeof_gc_summary_stats);
}

static void free_RTSSummaryStats(RTSSummaryStats * sum)
{
    stgFree(sum->gc_summary_stats);
    sum->gc_summary_stats = NULL;
}

// Must hold stats_mutex.
static void report_summary(const RTSSummaryStats* sum)
{
    // We should do no calculation, other than unit changes and formatting, and
    // we should not use any data from outside of globals, sum and stats
    // here. See Note [RTS Stats Reporting]

    uint32_t g;
    char temp[512];
    showStgWord64(stats.allocated_bytes, temp, true/*commas*/);
    statsPrintf("%16s bytes allocated in the heap\n", temp);

    showStgWord64(stats.copied_bytes, temp, true/*commas*/);
    statsPrintf("%16s bytes copied during GC\n", temp);

    if ( stats.major_gcs > 0 ) {
        showStgWord64(stats.max_live_bytes, temp, true/*commas*/);
        statsPrintf("%16s bytes maximum residency (%" FMT_Word32
                    " sample(s))\n",
                    temp, stats.major_gcs);
    }

    showStgWord64(stats.max_slop_bytes, temp, true/*commas*/);
    statsPrintf("%16s bytes maximum slop\n", temp);

    statsPrintf("%16" FMT_Word64 " MiB total memory in use (%"
                FMT_Word64 " MiB lost due to fragmentation)\n\n",
                stats.max_mem_in_use_bytes  / (1024 * 1024),
                sum->fragmentation_bytes / (1024 * 1024));

    /* Print garbage collections in each gen */
    statsPrintf("                                     Tot time (elapsed)  Avg pause  Max pause\n");
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        const GenerationSummaryStats * gen_stats =
            &sum->gc_summary_stats[g];
        statsPrintf("  Gen %2d     %5d colls"
                    ", %5d par   %6.3fs  %6.3fs     %3.4fs    %3.4fs\n",
                    g, // REVIEWERS: this used to be gen->no
                    //, this can't ever be different right?
                    gen_stats->collections,
                    gen_stats->par_collections,
                    TimeToSecondsDbl(gen_stats->cpu_ns),
                    TimeToSecondsDbl(gen_stats->elapsed_ns),
                    TimeToSecondsDbl(gen_stats->avg_pause_ns),
                    TimeToSecondsDbl(gen_stats->max_pause_ns));
    }
    if (RtsFlags.GcFlags.useNonmoving) {
        const uint32_t nonmoving_gen = RtsFlags.GcFlags.generations-1;
        const int n_major_colls = sum->gc_summary_stats[nonmoving_gen].collections;
        statsPrintf("  Gen %2d     %5d syncs"
                    ",                      %6.3fs     %3.4fs    %3.4fs\n",
                    nonmoving_gen,
                    n_major_colls,
                    TimeToSecondsDbl(stats.nonmoving_gc_sync_elapsed_ns),
                    TimeToSecondsDbl(stats.nonmoving_gc_sync_elapsed_ns) / n_major_colls,
                    TimeToSecondsDbl(stats.nonmoving_gc_sync_max_elapsed_ns));
        statsPrintf("  Gen %2d      concurrent"
                    ",             %6.3fs  %6.3fs     %3.4fs    %3.4fs\n",
                    nonmoving_gen,
                    TimeToSecondsDbl(stats.nonmoving_gc_cpu_ns),
                    TimeToSecondsDbl(stats.nonmoving_gc_elapsed_ns),
                    TimeToSecondsDbl(stats.nonmoving_gc_elapsed_ns) / n_major_colls,
                    TimeToSecondsDbl(stats.nonmoving_gc_max_elapsed_ns));
    }

    statsPrintf("\n");

#if defined(THREADED_RTS)
    if (RtsFlags.ParFlags.parGcEnabled && sum->work_balance > 0) {
        // See Note [Work Balance]
        statsPrintf("  Parallel GC work balance: "
                    "%.2f%% (serial 0%%, perfect 100%%)\n\n",
                    sum->work_balance * 100);
    }

    statsPrintf("  TASKS: %d "
                "(%d bound, %d peak workers (%d total), using -N%d)\n\n",
                taskCount, sum->bound_task_count,
                peakWorkerCount, workerCount,
                getNumCapabilities());

    statsPrintf("  SPARKS: %" FMT_Word64
                " (%" FMT_Word " converted, %" FMT_Word " overflowed, %"
                FMT_Word " dud, %" FMT_Word " GC'd, %" FMT_Word " fizzled)\n\n",
                sum->sparks_count,
                sum->sparks.converted, sum->sparks.overflowed,
                sum->sparks.dud, sum->sparks.gcd,
                sum->sparks.fizzled);
#endif

    statsPrintf("  INIT    time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(stats.init_cpu_ns),
                TimeToSecondsDbl(stats.init_elapsed_ns));

    statsPrintf("  MUT     time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(stats.mutator_cpu_ns),
                TimeToSecondsDbl(stats.mutator_elapsed_ns));
    statsPrintf("  GC      time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(stats.gc_cpu_ns),
                TimeToSecondsDbl(stats.gc_elapsed_ns));
    if (RtsFlags.GcFlags.useNonmoving) {
        statsPrintf(
                "  CONC GC time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(stats.nonmoving_gc_cpu_ns),
                TimeToSecondsDbl(stats.nonmoving_gc_elapsed_ns));
    }

#if defined(PROFILING)
    statsPrintf("  RP      time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(sum->rp_cpu_ns),
                TimeToSecondsDbl(sum->rp_elapsed_ns));
    statsPrintf("  PROF    time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(sum->hc_cpu_ns),
                TimeToSecondsDbl(sum->hc_elapsed_ns));
#endif
    statsPrintf("  EXIT    time  %7.3fs  (%7.3fs elapsed)\n",
                TimeToSecondsDbl(sum->exit_cpu_ns),
                TimeToSecondsDbl(sum->exit_elapsed_ns));
    statsPrintf("  Total   time  %7.3fs  (%7.3fs elapsed)\n\n",
                TimeToSecondsDbl(stats.cpu_ns),
                TimeToSecondsDbl(stats.elapsed_ns));
#if !defined(THREADED_RTS)
    statsPrintf("  %%GC     time     %5.1f%%  (%.1f%% elapsed)\n\n",
                sum->gc_cpu_percent * 100,
                sum->gc_elapsed_percent * 100);
#endif

    showStgWord64(sum->alloc_rate, temp, true/*commas*/);

    statsPrintf("  Alloc rate    %s bytes per MUT second\n\n", temp);

    statsPrintf("  Productivity %5.1f%% of total user, "
                "%.1f%% of total elapsed\n\n",
                sum->productivity_cpu_percent * 100,
                sum->productivity_elapsed_percent * 100);

    // See Note [Internal Counters Stats] for a description of the
    // following counters. If you add a counter here, please remember
    // to update the Note.
    if (RtsFlags.MiscFlags.internalCounters) {
#if defined(THREADED_RTS) && defined(PROF_SPIN)
        const int32_t col_width[] = {4, -30, 14, 14};
        statsPrintf("Internal Counters:\n");
        statsPrintf("%*s" "%*s" "%*s" "%*s" "\n"
                    , col_width[0], ""
                    , col_width[1], "SpinLock"
                    , col_width[2], "Spins"
                    , col_width[3], "Yields");
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "%*" FMT_Word64 "\n"
                    , col_width[0], ""
                    , col_width[1], "gc_alloc_block_sync"
                    , col_width[2], gc_alloc_block_sync.spin
                    , col_width[3], gc_alloc_block_sync.yield);
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "%*s\n"
                    , col_width[0], ""
                    , col_width[1], "whitehole_gc"
                    , col_width[2], whitehole_gc_spin
                    , col_width[3], "n/a");
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "%*s\n"
                    , col_width[0], ""
                    , col_width[1], "whitehole_threadPaused"
                    , col_width[2], whitehole_threadPaused_spin
                    , col_width[3], "n/a");
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "%*s\n"
                    , col_width[0], ""
                    , col_width[1], "whitehole_executeMessage"
                    , col_width[2], whitehole_executeMessage_spin
                    , col_width[3], "n/a");
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "%*" FMT_Word64 "\n"
                    , col_width[0], ""
                    , col_width[1], "whitehole_lockClosure"
                    , col_width[2], whitehole_lockClosure_spin
                    , col_width[3], whitehole_lockClosure_yield);
        // waitForGcThreads isn't really spin-locking(see the function)
        // but these numbers still seem useful.
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "%*" FMT_Word64 "\n"
                    , col_width[0], ""
                    , col_width[1], "waitForGcThreads"
                    , col_width[2], waitForGcThreads_spin
                    , col_width[3], waitForGcThreads_yield);

        for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
            int prefix_length = 0;
            prefix_length = statsPrintf("%*s" "gen[%" FMT_Word32,
                        col_width[0], "", g);
            if (prefix_length < 0)
                prefix_length = 0;
            prefix_length -= col_width[0];
            int suffix_length = col_width[1] + prefix_length;
            suffix_length =
                  suffix_length > 0 ? col_width[1] : suffix_length;

            statsPrintf("%*s" "%*" FMT_Word64 "%*" FMT_Word64 "\n"
                        , suffix_length, "].sync"
                        , col_width[2], generations[g].sync.spin
                        , col_width[3], generations[g].sync.yield);
        }
        statsPrintf("\n");
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "\n"
                    , col_width[0], ""
                    , col_width[1], "any_work"
                    , col_width[2], stats.any_work);
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "\n"
                    , col_width[0], ""
                    , col_width[1], "scav_find_work"
                    , col_width[2], stats.scav_find_work);
        statsPrintf("%*s" "%*s" "%*" FMT_Word64 "\n"
                    , col_width[0], ""
                    , col_width[1], "max_n_todo_overflow"
                    , col_width[2], stats.max_n_todo_overflow);
#elif defined(THREADED_RTS) // THREADED_RTS && PROF_SPIN
        statsPrintf("Internal Counters require the RTS to be built "
                "with PROF_SPIN"); // PROF_SPIN is not #defined here
#else // THREADED_RTS
        statsPrintf("Internal Counters require the threaded RTS");
#endif
    }
}

static void report_machine_readable (const RTSSummaryStats * sum)
{
    // We should do no calculation, other than unit changes and formatting, and
    // we should not use any data from outside of globals, sum and stats
    // here. See Note [RTS Stats Reporting]
    uint32_t g;

#define MR_STAT(field_name,format,value) \
    statsPrintf(" ,(\"" field_name "\", \"%" format "\")\n", value)
#define MR_STAT_GEN(gen,field_name,format,value) \
    statsPrintf(" ,(\"gen_%" FMT_Word32 "_" field_name "\", \"%" \
      format "\")\n", g, value)

    // These first values are for backwards compatibility.
    // Some of these first fields are duplicated with more machine-readable
    // names, or to match the name in RtsStats.

    // we don't use for the first field helper macro here because the prefix is
    // different
    statsPrintf(" [(\"%s\", \"%" FMT_Word64 "\")\n", "bytes allocated",
                stats.allocated_bytes);
    MR_STAT("num_GCs", FMT_Word32, stats.gcs);
    MR_STAT("average_bytes_used", FMT_Word64, sum->average_bytes_used);
    MR_STAT("max_bytes_used", FMT_Word64, stats.max_live_bytes);
    MR_STAT("num_byte_usage_samples", FMT_Word32, stats.major_gcs);
    MR_STAT("peak_megabytes_allocated", FMT_Word64,
      stats.max_mem_in_use_bytes / (1024 * 1024));

    MR_STAT("init_cpu_seconds", "f", TimeToSecondsDbl(stats.init_cpu_ns));
    MR_STAT("init_wall_seconds", "f", TimeToSecondsDbl(stats.init_elapsed_ns));
    MR_STAT("mut_cpu_seconds", "f", TimeToSecondsDbl(stats.mutator_cpu_ns));
    MR_STAT("mut_wall_seconds", "f",
            TimeToSecondsDbl(stats.mutator_elapsed_ns));
    MR_STAT("GC_cpu_seconds", "f", TimeToSecondsDbl(stats.gc_cpu_ns));
    MR_STAT("GC_wall_seconds", "f", TimeToSecondsDbl(stats.gc_elapsed_ns));

    // end backward compatibility

    // First, the rest of the times

    MR_STAT("exit_cpu_seconds", "f", TimeToSecondsDbl(sum->exit_cpu_ns));
    MR_STAT("exit_wall_seconds", "f", TimeToSecondsDbl(sum->exit_elapsed_ns));
#if defined(PROFILING)
    MR_STAT("rp_cpu_seconds", "f", TimeToSecondsDbl(sum->rp_cpu_ns));
    MR_STAT("rp_wall_seconds", "f", TimeToSecondsDbl(sum->rp_elapsed_ns));
    MR_STAT("hc_cpu_seconds", "f", TimeToSecondsDbl(sum->hc_cpu_ns));
    MR_STAT("hc_wall_seconds", "f", TimeToSecondsDbl(sum->hc_elapsed_ns));
#endif
    MR_STAT("total_cpu_seconds", "f", TimeToSecondsDbl(stats.cpu_ns));
    MR_STAT("total_wall_seconds", "f",
            TimeToSecondsDbl(stats.elapsed_ns));

    // next, the remainder of the fields of RTSStats, except internal counters

    // The first two are duplicates of those above, but have more machine
    // readable names that match the field names in RTSStats.


    // gcs has been done as num_GCs above
    MR_STAT("major_gcs", FMT_Word32, stats.major_gcs);
    MR_STAT("allocated_bytes", FMT_Word64, stats.allocated_bytes);
    MR_STAT("max_live_bytes", FMT_Word64, stats.max_live_bytes);
    MR_STAT("max_large_objects_bytes", FMT_Word64,
            stats.max_large_objects_bytes);
    MR_STAT("max_compact_bytes", FMT_Word64, stats.max_compact_bytes);
    MR_STAT("max_slop_bytes", FMT_Word64, stats.max_slop_bytes);
    // This duplicates, except for unit, peak_megabytes_allocated above
    MR_STAT("max_mem_in_use_bytes", FMT_Word64, stats.max_mem_in_use_bytes);
    MR_STAT("cumulative_live_bytes", FMT_Word64, stats.cumulative_live_bytes);
    MR_STAT("copied_bytes", FMT_Word64, stats.copied_bytes);
    MR_STAT("par_copied_bytes", FMT_Word64, stats.par_copied_bytes);
    MR_STAT("cumulative_par_max_copied_bytes", FMT_Word64,
            stats.cumulative_par_max_copied_bytes);
    MR_STAT("cumulative_par_balanced_copied_bytes", FMT_Word64,
            stats.cumulative_par_balanced_copied_bytes);

    // next, the computed fields in RTSSummaryStats
#if !defined(THREADED_RTS) // THREADED_RTS
    MR_STAT("gc_cpu_percent", "f", sum->gc_cpu_percent);
    MR_STAT("gc_wall_percent", "f", sum->gc_cpu_percent);
#endif
    MR_STAT("fragmentation_bytes", FMT_Word64, sum->fragmentation_bytes);
    // average_bytes_used is done above
    MR_STAT("alloc_rate", FMT_Word64, sum->alloc_rate);
    MR_STAT("productivity_cpu_percent", "f", sum->productivity_cpu_percent);
    MR_STAT("productivity_wall_percent", "f",
            sum->productivity_elapsed_percent);

    // next, the THREADED_RTS fields in RTSSummaryStats

#if defined(THREADED_RTS)
    MR_STAT("bound_task_count", FMT_Word32, sum->bound_task_count);
    MR_STAT("sparks_count", FMT_Word64, sum->sparks_count);
    MR_STAT("sparks_converted", FMT_Word, sum->sparks.converted);
    MR_STAT("sparks_overflowed", FMT_Word, sum->sparks.overflowed);
    MR_STAT("sparks_dud ", FMT_Word, sum->sparks.dud);
    MR_STAT("sparks_gcd", FMT_Word, sum->sparks.gcd);
    MR_STAT("sparks_fizzled", FMT_Word, sum->sparks.fizzled);
    MR_STAT("work_balance", "f", sum->work_balance);

    // next, globals (other than internal counters)
    MR_STAT("n_capabilities", FMT_Word32, getNumCapabilities());
    MR_STAT("task_count", FMT_Word32, taskCount);
    MR_STAT("peak_worker_count", FMT_Word32, peakWorkerCount);
    MR_STAT("worker_count", FMT_Word32, workerCount);

    // next, internal counters
#if defined(PROF_SPIN)
    MR_STAT("gc_alloc_block_sync_spin", FMT_Word64, gc_alloc_block_sync.spin);
    MR_STAT("gc_alloc_block_sync_yield", FMT_Word64,
            gc_alloc_block_sync.yield);
    MR_STAT("gc_alloc_block_sync_spin", FMT_Word64, gc_alloc_block_sync.spin);
    MR_STAT("waitForGcThreads_spin", FMT_Word64, waitForGcThreads_spin);
    MR_STAT("waitForGcThreads_yield", FMT_Word64,
            waitForGcThreads_yield);
    MR_STAT("whitehole_gc_spin", FMT_Word64, whitehole_gc_spin);
    MR_STAT("whitehole_lockClosure_spin", FMT_Word64,
            whitehole_lockClosure_spin);
    MR_STAT("whitehole_lockClosure_yield", FMT_Word64,
            whitehole_lockClosure_yield);
    MR_STAT("whitehole_executeMessage_spin", FMT_Word64,
            whitehole_executeMessage_spin);
    MR_STAT("whitehole_threadPaused_spin", FMT_Word64,
            whitehole_threadPaused_spin);
    MR_STAT("any_work", FMT_Word64,
            stats.any_work);
    MR_STAT("scav_find_work", FMT_Word64,
            stats.scav_find_work);
    MR_STAT("max_n_todo_overflow", FMT_Word64,
            stats.max_n_todo_overflow);
#endif // PROF_SPIN
#endif // THREADED_RTS

    // finally, per-generation stats. Named as, for example for generation 0,
    // gen_0_collections
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        const GenerationSummaryStats* gc_sum = &sum->gc_summary_stats[g];
        MR_STAT_GEN(g, "collections", FMT_Word32, gc_sum->collections);
        MR_STAT_GEN(g, "par_collections", FMT_Word32, gc_sum->par_collections);
        MR_STAT_GEN(g, "cpu_seconds", "f", TimeToSecondsDbl(gc_sum->cpu_ns));
        MR_STAT_GEN(g, "wall_seconds", "f",
                    TimeToSecondsDbl(gc_sum->elapsed_ns));
        MR_STAT_GEN(g, "max_pause_seconds", "f",
                    TimeToSecondsDbl(gc_sum->max_pause_ns));
        MR_STAT_GEN(g, "avg_pause_seconds", "f",
                    TimeToSecondsDbl(gc_sum->avg_pause_ns));
#if defined(THREADED_RTS) && defined(PROF_SPIN)
        MR_STAT_GEN(g, "sync_spin", FMT_Word64, gc_sum->sync_spin);
        MR_STAT_GEN(g, "sync_yield", FMT_Word64, gc_sum->sync_yield);
#endif
    }
    // non-moving collector statistics
    if (RtsFlags.GcFlags.useNonmoving) {
        const int n_major_colls = sum->gc_summary_stats[RtsFlags.GcFlags.generations-1].collections;
        MR_STAT("nonmoving_sync_wall_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_sync_elapsed_ns));
        MR_STAT("nonmoving_sync_max_pause_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_sync_max_elapsed_ns));
        MR_STAT("nonmoving_sync_avg_pause_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_sync_elapsed_ns) / n_major_colls);

        MR_STAT("nonmoving_concurrent_cpu_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_cpu_ns));
        MR_STAT("nonmoving_concurrent_wall_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_elapsed_ns));
        MR_STAT("nonmoving_concurrent_max_pause_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_max_elapsed_ns));
        MR_STAT("nonmoving_concurrent_avg_pause_seconds", "f",
                TimeToSecondsDbl(stats.nonmoving_gc_elapsed_ns) / n_major_colls);
    }


    statsPrintf(" ]\n");
}

// Must hold stats_mutex.
static void report_one_line(const RTSSummaryStats * sum)
{
    // We should do no calculation, other than unit changes and formatting, and
    // we should not use any data from outside of globals, sum and stats
    // here. See Note [RTS Stats Reporting]

    /* print the long long separately to avoid bugginess on mingwin (2001-07-02,
    mingw-0.5) */
    statsPrintf("<<ghc: %" FMT_Word64 " bytes, "
                "%" FMT_Word32 " GCs, "
                "%" FMT_Word64 "/%" FMT_Word64 " avg/max bytes residency "
                "(%" FMT_Word32 " samples), "
                "%" FMT_Word64 "M in use, "
                "%.3f INIT (%.3f elapsed), "
                "%.3f MUT (%.3f elapsed), "
                "%.3f GC (%.3f elapsed) :ghc>>\n",
                stats.allocated_bytes,
                stats.gcs,
                sum->average_bytes_used,
                stats.max_live_bytes,
                stats.major_gcs,
                stats.max_mem_in_use_bytes / (1024 * 1024),
                TimeToSecondsDbl(stats.init_cpu_ns),
                TimeToSecondsDbl(stats.init_elapsed_ns),
                TimeToSecondsDbl(stats.mutator_cpu_ns),
                TimeToSecondsDbl(stats.mutator_elapsed_ns),
                TimeToSecondsDbl(stats.gc_cpu_ns),
                TimeToSecondsDbl(stats.gc_elapsed_ns));
}

void
stat_exitReport (void)
{
    RTSSummaryStats sum;
    init_RTSSummaryStats(&sum);
    // We'll need to refer to task counters later
    ACQUIRE_LOCK(&all_tasks_mutex);

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
        // First we tidy the times in stats, and populate the times in sum.
        // In particular, we adjust the gc_* time in stats to remove
        // profiling times.
        {
            Time now_cpu_ns, now_elapsed_ns;
            getProcessTimes(&now_cpu_ns, &now_elapsed_ns);

            ACQUIRE_LOCK(&stats_mutex);
            stats.cpu_ns = now_cpu_ns - start_init_cpu;
            stats.elapsed_ns = now_elapsed_ns - start_init_elapsed;
            /* avoid divide by zero if stats.total_cpu_ns is measured as 0.00
               seconds -- SDM */
            if (stats.cpu_ns <= 0) { stats.cpu_ns = 1; }
            if (stats.elapsed_ns <= 0) { stats.elapsed_ns = 1; }

#if defined(PROFILING)
            sum.rp_cpu_ns = RP_tot_time;
            sum.rp_elapsed_ns = RPe_tot_time;
            sum.hc_cpu_ns = HC_tot_time;
            sum.hc_elapsed_ns = HCe_tot_time;
#endif // PROFILING

            // We do a GC during the EXIT phase. We'll attribute the cost of
            // that to GC instead of EXIT, so carefully subtract it from the
            // EXIT time.
            // Note that exit_gc includes RP and HC for the exit GC too.
            Time exit_gc_cpu     = stats.gc_cpu_ns - start_exit_gc_cpu;
            Time exit_gc_elapsed = stats.gc_elapsed_ns - start_exit_gc_elapsed;

            WARN(exit_gc_elapsed > 0);

            sum.exit_cpu_ns     = end_exit_cpu
                                      - start_exit_cpu
                                      - exit_gc_cpu;
            sum.exit_elapsed_ns = end_exit_elapsed
                                       - start_exit_elapsed
                                       - exit_gc_elapsed;

            WARN(sum.exit_elapsed_ns >= 0);

            stats.mutator_cpu_ns     = start_exit_cpu
                                 - end_init_cpu
                                 - (stats.gc_cpu_ns - exit_gc_cpu)
                                 - stats.nonmoving_gc_cpu_ns;
            stats.mutator_elapsed_ns = start_exit_elapsed
                                 - end_init_elapsed
                                 - (stats.gc_elapsed_ns - exit_gc_elapsed);

            WARN(stats.mutator_elapsed_ns >= 0);

            if (stats.mutator_cpu_ns < 0) { stats.mutator_cpu_ns = 0; }

            // The subdivision of runtime into INIT/EXIT/GC/MUT is just adding
            // and subtracting, so the parts should add up to the total exactly.
            // Note that stats->total_ns is captured a tiny bit later than
            // end_exit_elapsed, so we don't use it here.
            WARN(stats.init_elapsed_ns // INIT
                   + stats.mutator_elapsed_ns // MUT
                   + stats.gc_elapsed_ns // GC
                   + sum.exit_elapsed_ns // EXIT
                   == end_exit_elapsed - start_init_elapsed);

            // heapCensus() is called by the GC, so RP and HC time are
            // included in the GC stats.  We therefore subtract them to
            // obtain the actual GC cpu time.
            Time prof_cpu     = sum.rp_cpu_ns + sum.hc_cpu_ns;
            Time prof_elapsed = sum.rp_elapsed_ns + sum.hc_elapsed_ns;

            stats.gc_cpu_ns      -=  prof_cpu;
            stats.gc_elapsed_ns  -=  prof_elapsed;

            // This assertion is probably not necessary; make sure the
            // subdivision with PROF also makes sense
            WARN(stats.init_elapsed_ns // INIT
                   + stats.mutator_elapsed_ns // MUT
                   + stats.gc_elapsed_ns // GC
                   + sum.exit_elapsed_ns // EXIT
                   + (sum.rp_elapsed_ns + sum.hc_elapsed_ns) // PROF
                   == end_exit_elapsed - start_init_elapsed);
        }

        // REVIEWERS: it's not clear to me why the following isn't done in
        // stat_endGC of the last garbage collection?

        // We account for the last garbage collection
        {
            uint64_t tot_alloc_bytes = calcTotalAllocated() * sizeof(W_);
            stats.gc.allocated_bytes = tot_alloc_bytes - stats.allocated_bytes;
            stats.allocated_bytes = tot_alloc_bytes;
            if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
                statsPrintf("%9" FMT_Word " %9.9s %9.9s",
                            (W_)stats.gc.allocated_bytes, "", "");
                statsPrintf(" %6.3f %6.3f\n\n", 0.0, 0.0);
            }
        }

        // We populate the remainder (non-time elements) of sum
        {
    #if defined(THREADED_RTS)
            sum.bound_task_count = taskCount - workerCount;

            for (uint32_t i = 0; i < getNumCapabilities(); i++) {
                sum.sparks.created   += getCapability(i)->spark_stats.created;
                sum.sparks.dud       += getCapability(i)->spark_stats.dud;
                sum.sparks.overflowed+=
                  getCapability(i)->spark_stats.overflowed;
                sum.sparks.converted +=
                  getCapability(i)->spark_stats.converted;
                sum.sparks.gcd       += getCapability(i)->spark_stats.gcd;
                sum.sparks.fizzled   += getCapability(i)->spark_stats.fizzled;
            }

            sum.sparks_count = sum.sparks.created
                + sum.sparks.dud
                + sum.sparks.overflowed;

            if (RtsFlags.ParFlags.parGcEnabled && stats.par_copied_bytes > 0) {
                // See Note [Work Balance]
                sum.work_balance =
                    (double)stats.cumulative_par_balanced_copied_bytes
                    / (double)stats.par_copied_bytes;
            } else {
                sum.work_balance = 0;
            }


    #else // THREADED_RTS
            sum.gc_cpu_percent     = stats.gc_cpu_ns
                                  / stats.cpu_ns;
            sum.gc_elapsed_percent = stats.gc_elapsed_ns
                                  / stats.elapsed_ns;
    #endif // THREADED_RTS

            sum.fragmentation_bytes =
                (uint64_t)(peak_mblocks_allocated
                         * BLOCKS_PER_MBLOCK
                         * BLOCK_SIZE_W
                         - hw_alloc_blocks * BLOCK_SIZE_W)
                * (uint64_t)sizeof(W_);

            sum.average_bytes_used = stats.major_gcs == 0 ? 0 :
                 stats.cumulative_live_bytes/stats.major_gcs,

            sum.alloc_rate = stats.mutator_cpu_ns == 0 ? 0 :
                (uint64_t)((double)stats.allocated_bytes
                / TimeToSecondsDbl(stats.mutator_cpu_ns));

            // REVIEWERS: These two values didn't used to include the exit times
            sum.productivity_cpu_percent =
                TimeToSecondsDbl(stats.cpu_ns
                                - stats.gc_cpu_ns
                                - stats.init_cpu_ns
                                - sum.exit_cpu_ns)
                / TimeToSecondsDbl(stats.cpu_ns);

            WARN(sum.productivity_cpu_percent >= 0);

            sum.productivity_elapsed_percent =
                TimeToSecondsDbl(stats.elapsed_ns
                                - stats.gc_elapsed_ns
                                - stats.init_elapsed_ns
                                - sum.exit_elapsed_ns)
                / TimeToSecondsDbl(stats.elapsed_ns);

            WARN(sum.productivity_elapsed_percent >= 0);

            for(uint32_t g = 0; g < RtsFlags.GcFlags.generations; ++g) {
                const generation* gen = &generations[g];
                GenerationSummaryStats* gen_stats = &sum.gc_summary_stats[g];
                gen_stats->collections = gen->collections;
                gen_stats->par_collections = gen->par_collections;
                gen_stats->cpu_ns = GC_coll_cpu[g];
                gen_stats->elapsed_ns = GC_coll_elapsed[g];
                gen_stats->max_pause_ns = GC_coll_max_pause[g];
                gen_stats->avg_pause_ns = gen->collections == 0 ?
                    0 : (GC_coll_elapsed[g] / gen->collections);
    #if defined(THREADED_RTS) && defined(PROF_SPIN)
                gen_stats->sync_spin = gen->sync.spin;
                gen_stats->sync_yield = gen->sync.yield;
    #endif // PROF_SPIN
            }
        }

        // Now we generate the report
        if (RtsFlags.GcFlags.giveStats >= SUMMARY_GC_STATS) {
            report_summary(&sum);
        }

        if (RtsFlags.GcFlags.giveStats == ONELINE_GC_STATS) {
            if (RtsFlags.MiscFlags.machineReadable) {
                report_machine_readable(&sum);
            }
            else {
                report_one_line(&sum);
            }
        }
        RELEASE_LOCK(&stats_mutex);

        statsFlush();
        statsClose();
    }

    free_RTSSummaryStats(&sum);

    if (GC_coll_cpu) {
      stgFree(GC_coll_cpu);
      GC_coll_cpu = NULL;
    }
    if (GC_coll_elapsed) {
      stgFree(GC_coll_elapsed);
      GC_coll_elapsed = NULL;
    }
    if (GC_coll_max_pause) {
      stgFree(GC_coll_max_pause);
      GC_coll_max_pause = NULL;
    }

    RELEASE_LOCK(&all_tasks_mutex);
}

void stat_exit(void)
{
#if defined(THREADED_RTS)
        closeMutex(&stats_mutex);
#endif
}

/* Note [Work Balance]
~~~~~~~~~~~~~~~~~~~~~~
Work balance is a measure of how evenly the work done during parallel garbage
collection is spread across threads. To compute work balance we must take care
to account for the number of GC threads changing between GCs. The statistics we
track must have the number of GC threads "integrated out".

We accumulate two values from each garbage collection:
* par_copied: is a measure of the total work done during parallel garbage
  collection
* par_balanced_copied: is a measure of the balanced work done
  during parallel garbage collection.

par_copied is simple to compute, but par_balanced_copied_bytes is somewhat more
complicated:

For a given garbage collection:
Let gc_copied := total copies during the gc
    gc_copied_i := copies by the ith thread during the gc
    num_gc_threads := the number of threads participating in the gc
    balance_limit := (gc_copied / num_gc_threads)

If we were to graph gc_copied_i, sorted from largest to smallest we would see
something like:

       |X
  ^    |X X
  |    |X X X            X: unbalanced copies
copies |-----------      Y: balanced copies by the busiest GC thread
       |Y Z Z            Z: other balanced copies
       |Y Z Z Z
       |Y Z Z Z Z
       |Y Z Z Z Z Z
       |===========
       |1 2 3 4 5 6
          i ->

where the --- line is at balance_limit. Balanced copies are those under the ---
line, i.e. the area of the Ys and Zs. Note that the area occupied by the Ys will
always equal balance_limit. Completely balanced gc has every thread copying
balance_limit and a completely unbalanced gc has a single thread copying
gc_copied.

One could define par_balance_copied as the areas of the Ys and Zs in the graph
above, however we would like the ratio of (par_balance_copied / gc_copied) to
range from 0 to 1, so that work_balance will be a nice percentage, also ranging
from 0 to 1. We therefore define par_balanced_copied as:

                                                        (  num_gc_threads  )
{Sum[Min(gc_copied_i,balance_limit)] - balance_limit} * (------------------)
  i                                                     (num_gc_threads - 1)
                                          vvv                  vvv
                                           S                    T

Where the S and T terms serve to remove the area of the Ys, and
to normalize the result to lie between 0 and gc_copied.

Note that the implementation orders these operations differently to minimize
error due to integer rounding.

Then cumulative work balance is computed as
(cumulative_par_balanced_copied_bytes / par_copied_byes)

Previously, cumulative work balance was computed as:

(cumulative_par_max_copied_bytes)
(-------------------------------) - 1
(       par_copied_bytes        )
-------------------------------------
        (n_capabilities - 1)

This was less accurate than the current method, and invalid whenever a garbage
collection had occurred with num_gc_threads /= n_capabilities; which can happen
when setNumCapabilities is called, when -qn is passed as an RTS option, or when
the number of gc threads is limited to the number of cores.
See #13830
*/

/*
Note [Internal Counters Stats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What do the counts at the end of a '+RTS -s --internal-counters' report mean?
They are detailed below. Most of these counters are used by multiple threads
with no attempt at synchronisation. This means that reported values  may be
lower than the true value and this becomes more likely and more severe as
contention increases.

The first counters are for various SpinLock-like constructs in the RTS. See
Spinlock.h for the definition of a SpinLock. We maintain up two counters per
SpinLock:
* spin: The number of busy-spins over the length of the program.
* yield: The number of times the SpinLock spun SPIN_COUNT times without success
         and called yieldThread().
Not all of these are actual SpinLocks, see the details below.

Actual SpinLocks:
* gc_alloc_block:
    This SpinLock protects the block allocator and free list manager. See
    BlockAlloc.c.
* gen[g].sync:
    These SpinLocks, one per generation, protect the generations[g] data
    structure during garbage collection.

waitForGcThreads:
  These counters are incremented while we wait for all threads to be ready
  for a parallel garbage collection. We yield more than we spin in this case.

In several places in the runtime we must take a lock on a closure. To do this,
we replace its info table with stg_WHITEHOLE_info, spinning if it is already
a white-hole. Sometimes we yieldThread() if we spin too long, sometimes we
don't. We count these white-hole spins and include them in the SpinLocks table.
If a particular loop does not yield, we put "n/a" in the table. They are named
for the function that has the spinning loop except that several loops in the
garbage collector accumulate into whitehole_gc.

white-hole spin counters:
* whitehole_gc
* whitehole_lockClosure
* whitehole_executeMessage
* whitehole_threadPaused

We have several stats allowing us to observe the internals of the parallel
garbage collector:

Parallel garbage collector counters:
* any_work:
    Incremented whenever a parallel GC looks for work to steal.
* scav_find_work:
    Counts iterations of scavenge loop
* max_n_todo_overflow:
    Tracks the maximum length of todo_overflow lists in the gc_thread structure.
    See comment in grab_local_todo_block.
*/

/* -----------------------------------------------------------------------------
   stat_describe_gens

   Produce some detailed info on the state of the generational GC.
   -------------------------------------------------------------------------- */
void
statDescribeGens(void)
{
  uint32_t g, mut, lge, compacts, i;
  W_ gen_slop;
  W_ tot_live, tot_slop;
  W_ gen_live, gen_blocks;
  bdescr *bd;
  generation *gen;

  debugBelch(
"----------------------------------------------------------------------\n"
"  Gen     Max  Mut-list  Blocks    Large  Compacts      Live      Slop\n"
"       Blocks     Bytes          Objects                              \n"
"----------------------------------------------------------------------\n");

  tot_live = 0;
  tot_slop = 0;

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      gen = &generations[g];

      for (bd = gen->large_objects, lge = 0; bd; bd = bd->link) {
          lge++;
      }

      for (bd = gen->compact_objects, compacts = 0; bd; bd = bd->link) {
          compacts++;
      }

      gen_live   = genLiveWords(gen);
      gen_blocks = genLiveBlocks(gen);

      mut = 0;
      for (i = 0; i < getNumCapabilities(); i++) {
          mut += countOccupied(getCapability(i)->mut_lists[g]);

          // Add the pinned object block.
          bd = getCapability(i)->pinned_object_block;
          if (bd != NULL) {
              gen_live   += bd->free_off / sizeof(W_);
              gen_blocks += bd->blocks;
          }

          gen_live   += gcThreadLiveWords(i,g);
          gen_blocks += gcThreadLiveBlocks(i,g);
      }

      debugBelch("%5d %7" FMT_Word " %9d", g, (W_)gen->max_blocks, mut);

      gen_slop = gen_blocks * BLOCK_SIZE_W - gen_live;

      debugBelch("%8" FMT_Word " %8d  %8d %9" FMT_Word " %9" FMT_Word "\n",
                 gen_blocks, lge, compacts, gen_live*(W_)sizeof(W_),
                 gen_slop*(W_)sizeof(W_));
      tot_live += gen_live;
      tot_slop += gen_slop;
  }
  debugBelch("----------------------------------------------------------------------\n");
  debugBelch("%51s%9" FMT_Word " %9" FMT_Word "\n",
             "",tot_live*(W_)sizeof(W_),tot_slop*(W_)sizeof(W_));
  debugBelch("----------------------------------------------------------------------\n");
  debugBelch("\n");
}

/* -----------------------------------------------------------------------------
   Stats available via a programmatic interface, so eg. GHCi can time
   each compilation and expression evaluation.
   -------------------------------------------------------------------------- */

uint64_t getAllocations( void )
{
    ACQUIRE_LOCK(&stats_mutex);
    StgWord64 n = stats.allocated_bytes;
    RELEASE_LOCK(&stats_mutex);
    return n;
}

int getRTSStatsEnabled( void )
{
    return RtsFlags.GcFlags.giveStats != NO_GC_STATS;
}

void getRTSStats( RTSStats *s )
{
    Time current_elapsed = 0;
    Time current_cpu = 0;

    ACQUIRE_LOCK(&stats_mutex);
    *s = stats;
    RELEASE_LOCK(&stats_mutex);

    getProcessTimes(&current_cpu, &current_elapsed);
    s->cpu_ns = current_cpu - end_init_cpu;
    s->elapsed_ns = current_elapsed - end_init_elapsed;

    s->mutator_cpu_ns = current_cpu - end_init_cpu - stats.gc_cpu_ns -
        stats.nonmoving_gc_cpu_ns;
    s->mutator_elapsed_ns = current_elapsed - end_init_elapsed -
        stats.gc_elapsed_ns;
}

/* -----------------------------------------------------------------------------
   Dumping stuff in the stats file, or via the debug message interface
   -------------------------------------------------------------------------- */

int
statsPrintf( char *s, ... )
{
    int ret = 0;
    FILE *sf = RtsFlags.GcFlags.statsFile;
    va_list ap;

    va_start(ap,s);
    if (sf == NULL) {
        ret = vdebugBelch(s,ap);
    } else {
        ret = vfprintf(sf, s, ap);
    }
    va_end(ap);
    return ret;
}

static void
statsFlush( void )
{
    FILE *sf = RtsFlags.GcFlags.statsFile;
    if (sf != NULL) {
        fflush(sf);
    }
}

static void
statsClose( void )
{
    FILE *sf = RtsFlags.GcFlags.statsFile;
    if (sf != NULL) {
        fclose(sf);
    }
}
