/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Stats.h"
#include "Profiling.h"
#include "GetTime.h"
#include "sm/Storage.h"
#include "sm/GC.h" // gc_alloc_block_sync, whitehole_gc_spin
#include "sm/GCThread.h"
#include "sm/BlockAlloc.h"

#define TimeToSecondsDbl(t) ((double)(t) / TIME_RESOLUTION)

static Time
    start_init_cpu, start_init_elapsed,
    end_init_cpu,   end_init_elapsed,
    start_exit_cpu, start_exit_elapsed,
    start_exit_gc_elapsed, start_exit_gc_cpu,
    end_exit_cpu,   end_exit_elapsed;

#if defined(PROFILING)
static Time RP_start_time  = 0, RP_tot_time  = 0;  // retainer prof user time
static Time RPe_start_time = 0, RPe_tot_time = 0;  // retainer prof elap time

static Time HC_start_time, HC_tot_time = 0;     // heap census prof user time
static Time HCe_start_time, HCe_tot_time = 0;   // heap census prof elap time
#endif

#if defined(PROFILING)
#define PROF_VAL(x)   (x)
#else
#define PROF_VAL(x)   0
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

static void statsPrintf( char *s, ... ) GNUC3_ATTRIBUTE(format (PRINTF, 1, 2));
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

double
mut_user_time_until( Time t )
{
    return TimeToSecondsDbl(t - stats.gc_cpu_ns);
    // heapCensus() time is included in GC_tot_cpu, so we don't need
    // to subtract it here.
}

double
mut_user_time( void )
{
    Time cpu;
    cpu = getProcessCPUTime();
    return mut_user_time_until(cpu);
}

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
    start_init_cpu    = 0;
    start_init_elapsed = 0;
    end_init_cpu     = 0;
    end_init_elapsed  = 0;

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
        .mutator_cpu_ns = 0,
        .mutator_elapsed_ns = 0,
        .gc_cpu_ns = 0,
        .gc_elapsed_ns = 0,
        .cpu_ns = 0,
        .elapsed_ns = 0,
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
            .sync_elapsed_ns = 0,
            .cpu_ns = 0,
            .elapsed_ns = 0
        }
    };
}

/* ---------------------------------------------------------------------------
   initStats1() can be called after setupRtsFlags()
   ------------------------------------------------------------------------ */

void
initStats1 (void)
{
    uint32_t i;

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
    for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
        GC_coll_cpu[i] = 0;
        GC_coll_elapsed[i] = 0;
        GC_coll_max_pause[i] = 0;
    }
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
}

/* -----------------------------------------------------------------------------
   stat_startExit and stat_endExit

   These two measure the time taken in shutdownHaskell().
   -------------------------------------------------------------------------- */

void
stat_startExit(void)
{
    getProcessTimes(&start_exit_cpu, &start_exit_elapsed);
    start_exit_gc_elapsed = stats.gc_elapsed_ns;
    start_exit_gc_cpu = stats.gc_cpu_ns;
}

void
stat_endExit(void)
{
    getProcessTimes(&end_exit_cpu, &end_exit_elapsed);
}

void
stat_startGCSync (gc_thread *gct)
{
    gct->gc_sync_start_elapsed = getProcessElapsedTime();
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each GC
   -------------------------------------------------------------------------- */

void
stat_startGC (Capability *cap, gc_thread *gct)
{
    if (RtsFlags.GcFlags.ringBell) {
        debugBelch("\007");
    }

    getProcessTimes(&gct->gc_start_cpu, &gct->gc_start_elapsed);

    // Post EVENT_GC_START with the same timestamp as used for stats
    // (though converted from Time=StgInt64 to EventTimestamp=StgWord64).
    // Here, as opposed to other places, the event is emitted on the cap
    // that initiates the GC and external tools expect it to have the same
    // timestamp as used in +RTS -s calculcations.
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
stat_endGC (Capability *cap, gc_thread *gct,
            W_ live, W_ copied, W_ slop, uint32_t gen,
            uint32_t par_n_threads, W_ par_max_copied,
            W_ par_balanced_copied)
{
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
            gct->gc_start_elapsed - gct->gc_sync_start_elapsed;
        stats.gc.elapsed_ns = current_elapsed - gct->gc_start_elapsed;
        stats.gc.cpu_ns = current_cpu - gct->gc_start_cpu;
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
        // See trac.haskell.org/ThreadScope/wiki/RTSsummaryEvents
        // for a detailed design rationale of the current setup
        // of GC eventlog events.
        traceEventGcGlobalSync(cap);

        // Emitted before GC_END on all caps, which simplifies tools code.
        traceEventGcStats(cap,
                          CAPSET_HEAP_DEFAULT,
                          stats.gc.gen,
                          stats.gc.copied_bytes,
                          stats.gc.slop_bytes,
                          /* current loss due to fragmentation */
                          (mblocks_allocated * BLOCKS_PER_MBLOCK - n_alloc_blocks)
                                 * BLOCK_SIZE,
                          par_n_threads,
                          stats.gc.par_max_copied_bytes,
                          stats.gc.copied_bytes,
                          stats.gc.par_balanced_copied_bytes);

        // Post EVENT_GC_END with the same timestamp as used for stats
        // (though converted from Time=StgInt64 to EventTimestamp=StgWord64).
        // Here, as opposed to other places, the event is emitted on the cap
        // that initiates the GC and external tools expect it to have the same
        // timestamp as used in +RTS -s calculcations.
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
                    faults - gct->gc_start_faults,
                        gct->gc_start_faults - GC_end_faults,
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
    }
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each Retainer Profiliing
   -------------------------------------------------------------------------- */
#if defined(PROFILING)
void
stat_startRP(void)
{
    Time user, elapsed;
    getProcessTimes( &user, &elapsed );

    RP_start_time = user;
    RPe_start_time = elapsed;
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of each Retainer Profiliing
   -------------------------------------------------------------------------- */

#if defined(PROFILING)
void
stat_endRP(
  uint32_t retainerGeneration,
#if defined(DEBUG_RETAINER)
  uint32_t maxCStackSize,
  int maxStackSize,
#endif
  double averageNumVisit)
{
    Time user, elapsed;
    getProcessTimes( &user, &elapsed );

    RP_tot_time += user - RP_start_time;
    RPe_tot_time += elapsed - RPe_start_time;

  fprintf(prof_file, "Retainer Profiling: %d, at %f seconds\n",
    retainerGeneration, mut_user_time_during_RP());
#if defined(DEBUG_RETAINER)
  fprintf(prof_file, "\tMax C stack size = %u\n", maxCStackSize);
  fprintf(prof_file, "\tMax auxiliary stack size = %u\n", maxStackSize);
#endif
  fprintf(prof_file, "\tAverage number of visits per object = %f\n", averageNumVisit);
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

    HC_start_time = user;
    HCe_start_time = elapsed;
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

    HC_tot_time += user - HC_start_time;
    HCe_tot_time += elapsed - HCe_start_time;
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
    statsPrintf("  (" #counter ")  : %s\n",temp);                               \
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

STATIC_INLINE Time get_init_cpu(void) {
    return end_init_cpu - start_init_cpu;
}

STATIC_INLINE Time get_init_elapsed(void) {
    return end_init_elapsed - start_init_elapsed;
}


void
stat_exit (void)
{
    generation *gen;
    Time gc_cpu = 0;
    Time gc_elapsed = 0;
    Time init_cpu = 0;
    Time init_elapsed = 0;
    Time mut_cpu = 0;
    Time mut_elapsed = 0;
    Time exit_cpu = 0;
    Time exit_elapsed = 0;
    Time exit_gc_cpu = 0;
    Time exit_gc_elapsed = 0;

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {

        char temp[512];
        Time tot_cpu;
        Time tot_elapsed;
        uint32_t g;

        getProcessTimes( &tot_cpu, &tot_elapsed );
        tot_cpu -= start_init_cpu;
        tot_elapsed -= start_init_elapsed;

        uint64_t tot_alloc_bytes = calcTotalAllocated() * sizeof(W_);

        // allocated since the last GC
        stats.gc.allocated_bytes = tot_alloc_bytes - stats.allocated_bytes;
        stats.allocated_bytes = tot_alloc_bytes;

        /* avoid divide by zero if tot_cpu is measured as 0.00 seconds -- SDM */
        if (tot_cpu <= 0)  tot_cpu = 1;
        if (tot_elapsed <= 0) tot_elapsed = 1;

        if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
            statsPrintf("%9" FMT_Word " %9.9s %9.9s",
                        (W_)stats.gc.allocated_bytes, "", "");
            statsPrintf(" %6.3f %6.3f\n\n", 0.0, 0.0);
        }

        // heapCensus() is called by the GC, so RP and HC time are
        // included in the GC stats.  We therefore subtract them to
        // obtain the actual GC cpu time.
        gc_cpu     = stats.gc_cpu_ns - PROF_VAL(RP_tot_time + HC_tot_time);
        gc_elapsed = stats.gc_elapsed_ns - PROF_VAL(RPe_tot_time + HCe_tot_time);

        init_cpu     = get_init_cpu();
        init_elapsed = get_init_elapsed();

        // We do a GC during the EXIT phase.  We'll attribute the cost of that
        // to GC instead of EXIT, so carefully subtract it from the EXIT time.
        exit_gc_cpu  = stats.gc_cpu_ns - start_exit_gc_cpu;
        exit_gc_elapsed = stats.gc_elapsed_ns - start_exit_gc_elapsed;
        exit_cpu     = end_exit_cpu - start_exit_cpu - exit_gc_cpu;
        exit_elapsed = end_exit_elapsed - start_exit_elapsed - exit_gc_elapsed;

        mut_elapsed = start_exit_elapsed - end_init_elapsed -
            (gc_elapsed - exit_gc_elapsed) -
            PROF_VAL(RPe_tot_time + HCe_tot_time);

        mut_cpu = start_exit_cpu - end_init_cpu - (gc_cpu - exit_gc_cpu)
            - PROF_VAL(RP_tot_time + HC_tot_time);
        if (mut_cpu < 0) { mut_cpu = 0; }

        // The subdivision of runtime into INIT/EXIT/GC/MUT is just adding and
        // subtracting, so the parts should add up to the total exactly.  Note
        // that tot_elapsed is captured a tiny bit later than end_exit_elapsed,
        // so we don't use it here.
        ASSERT(init_elapsed + mut_elapsed + gc_elapsed + exit_elapsed
               == end_exit_elapsed - start_init_elapsed);


        if (RtsFlags.GcFlags.giveStats >= SUMMARY_GC_STATS) {
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

            statsPrintf("%16" FMT_SizeT " MB total memory in use (%"
                        FMT_SizeT " MB lost due to fragmentation)\n\n",
                        (size_t)(peak_mblocks_allocated * MBLOCK_SIZE_W) / (1024 * 1024 / sizeof(W_)),
                        (size_t)(peak_mblocks_allocated * BLOCKS_PER_MBLOCK * BLOCK_SIZE_W - hw_alloc_blocks * BLOCK_SIZE_W) / (1024 * 1024 / sizeof(W_)));

            /* Print garbage collections in each gen */
            statsPrintf("                                     Tot time (elapsed)  Avg pause  Max pause\n");
            for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
                gen = &generations[g];
                statsPrintf("  Gen %2d     %5d colls, %5d par   %6.3fs  %6.3fs     %3.4fs    %3.4fs\n",
                            gen->no,
                            gen->collections,
                            gen->par_collections,
                            TimeToSecondsDbl(GC_coll_cpu[g]),
                            TimeToSecondsDbl(GC_coll_elapsed[g]),
                            gen->collections == 0 ? 0 : TimeToSecondsDbl(GC_coll_elapsed[g] / gen->collections),
                            TimeToSecondsDbl(GC_coll_max_pause[g]));
            }

#if defined(THREADED_RTS)
            if (RtsFlags.ParFlags.parGcEnabled && stats.par_copied_bytes > 0) {
                // See Note [Work Balance]
                statsPrintf("\n  Parallel GC work balance: %.2f%% (serial 0%%, perfect 100%%)\n",
                    100 * (double)stats.cumulative_par_balanced_copied_bytes /
                          (double)stats.par_copied_bytes);
            }
#endif
            statsPrintf("\n");

#if defined(THREADED_RTS)
            statsPrintf("  TASKS: %d (%d bound, %d peak workers (%d total), using -N%d)\n",
                        taskCount, taskCount - workerCount,
                        peakWorkerCount, workerCount,
                        n_capabilities);

            statsPrintf("\n");

            {
                uint32_t i;
                SparkCounters sparks = { 0, 0, 0, 0, 0, 0};
                for (i = 0; i < n_capabilities; i++) {
                    sparks.created   += capabilities[i]->spark_stats.created;
                    sparks.dud       += capabilities[i]->spark_stats.dud;
                    sparks.overflowed+= capabilities[i]->spark_stats.overflowed;
                    sparks.converted += capabilities[i]->spark_stats.converted;
                    sparks.gcd       += capabilities[i]->spark_stats.gcd;
                    sparks.fizzled   += capabilities[i]->spark_stats.fizzled;
                }

                statsPrintf("  SPARKS: %" FMT_Word " (%" FMT_Word " converted, %" FMT_Word " overflowed, %" FMT_Word " dud, %" FMT_Word " GC'd, %" FMT_Word " fizzled)\n\n",
                            sparks.created + sparks.dud + sparks.overflowed,
                            sparks.converted, sparks.overflowed, sparks.dud,
                            sparks.gcd, sparks.fizzled);
            }
#endif

            statsPrintf("  INIT    time  %7.3fs  (%7.3fs elapsed)\n",
                        TimeToSecondsDbl(init_cpu), TimeToSecondsDbl(init_elapsed));

            statsPrintf("  MUT     time  %7.3fs  (%7.3fs elapsed)\n",
                        TimeToSecondsDbl(mut_cpu), TimeToSecondsDbl(mut_elapsed));
            statsPrintf("  GC      time  %7.3fs  (%7.3fs elapsed)\n",
                        TimeToSecondsDbl(gc_cpu), TimeToSecondsDbl(gc_elapsed));

#if defined(PROFILING)
            statsPrintf("  RP      time  %7.3fs  (%7.3fs elapsed)\n",
                    TimeToSecondsDbl(RP_tot_time), TimeToSecondsDbl(RPe_tot_time));
            statsPrintf("  PROF    time  %7.3fs  (%7.3fs elapsed)\n",
                    TimeToSecondsDbl(HC_tot_time), TimeToSecondsDbl(HCe_tot_time));
#endif
            statsPrintf("  EXIT    time  %7.3fs  (%7.3fs elapsed)\n",
                    TimeToSecondsDbl(exit_cpu), TimeToSecondsDbl(exit_elapsed));
            statsPrintf("  Total   time  %7.3fs  (%7.3fs elapsed)\n\n",
                    TimeToSecondsDbl(tot_cpu), TimeToSecondsDbl(tot_elapsed));
#if !defined(THREADED_RTS)
            statsPrintf("  %%GC     time     %5.1f%%  (%.1f%% elapsed)\n\n",
                    TimeToSecondsDbl(gc_cpu)*100/TimeToSecondsDbl(tot_cpu),
                    TimeToSecondsDbl(gc_elapsed)*100/TimeToSecondsDbl(tot_elapsed));
#endif

            if (mut_cpu == 0) {
                showStgWord64(0, temp, true/*commas*/);
            } else {
                showStgWord64(
                    (StgWord64)((double)stats.allocated_bytes /
                                TimeToSecondsDbl(mut_cpu)),
                    temp, true/*commas*/);
            }

            statsPrintf("  Alloc rate    %s bytes per MUT second\n\n", temp);

            statsPrintf("  Productivity %5.1f%% of total user, %.1f%% of total elapsed\n\n",
                    TimeToSecondsDbl(tot_cpu - gc_cpu -
                                PROF_VAL(RP_tot_time + HC_tot_time) - init_cpu) * 100
                    / TimeToSecondsDbl(tot_cpu),
                    TimeToSecondsDbl(tot_elapsed - gc_elapsed -
                                PROF_VAL(RPe_tot_time + HCe_tot_time) - init_elapsed) * 100
                    / TimeToSecondsDbl(tot_elapsed));

#if defined(THREADED_RTS) && defined(PROF_SPIN)
            {
                uint32_t g;

                statsPrintf("gc_alloc_block_sync: %"FMT_Word64"\n", gc_alloc_block_sync.spin);
                statsPrintf("whitehole_gc_spin: %"FMT_Word64"\n"
                            , whitehole_gc_spin);
                for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
                    statsPrintf("gen[%d].sync: %"FMT_Word64"\n", g, generations[g].sync.spin);
                }
            }
#endif
        }

        if (RtsFlags.GcFlags.giveStats == ONELINE_GC_STATS) {
          char *fmt;
          if (RtsFlags.MiscFlags.machineReadable) {
            fmt =
                " [(\"bytes allocated\", \"%" FMT_Word64 "\")\n"
                " ,(\"num_GCs\", \"%" FMT_Word32 "\")\n"
                " ,(\"average_bytes_used\", \"%" FMT_Word64 "\")\n"
                " ,(\"max_bytes_used\", \"%" FMT_Word64 "\")\n"
                " ,(\"num_byte_usage_samples\", \"%" FMT_Word32 "\")\n"
                " ,(\"peak_megabytes_allocated\", \"%" FMT_Word64 "\")\n"
                " ,(\"init_cpu_seconds\", \"%.3f\")\n"
                " ,(\"init_wall_seconds\", \"%.3f\")\n"
                " ,(\"mutator_cpu_seconds\", \"%.3f\")\n"
                " ,(\"mutator_wall_seconds\", \"%.3f\")\n"
                " ,(\"GC_cpu_seconds\", \"%.3f\")\n"
                " ,(\"GC_wall_seconds\", \"%.3f\")\n"
                " ]\n";
          }
          else {
            fmt =
                "<<ghc: %" FMT_Word64 " bytes, "
                "%" FMT_Word32 " GCs, "
                "%" FMT_Word64 "/%" FMT_Word64 " avg/max bytes residency (%" FMT_Word32 " samples), "
                "%" FMT_Word64 "M in use, "
                "%.3f INIT (%.3f elapsed), "
                "%.3f MUT (%.3f elapsed), "
                "%.3f GC (%.3f elapsed) :ghc>>\n";
          }
          /* print the long long separately to avoid bugginess on mingwin (2001-07-02, mingw-0.5) */
          statsPrintf(fmt,
                    stats.allocated_bytes,
                    stats.gcs,
                     (uint64_t)
                      (stats.major_gcs == 0 ? 0 :
                       stats.cumulative_live_bytes/stats.major_gcs),
                    stats.max_live_bytes,
                    stats.major_gcs,
                    (uint64_t) (peak_mblocks_allocated * MBLOCK_SIZE / (1024L * 1024L)),
                    TimeToSecondsDbl(init_cpu), TimeToSecondsDbl(init_elapsed),
                    TimeToSecondsDbl(mut_cpu), TimeToSecondsDbl(mut_elapsed),
                    TimeToSecondsDbl(gc_cpu), TimeToSecondsDbl(gc_elapsed));
        }

        statsFlush();
        statsClose();
    }

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
}

/* Note [Work Balance]
----------------------
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
      for (i = 0; i < n_capabilities; i++) {
          mut += countOccupied(capabilities[i]->mut_lists[g]);

          // Add the pinned object block.
          bd = capabilities[i]->pinned_object_block;
          if (bd != NULL) {
              gen_live   += bd->free - bd->start;
              gen_blocks += bd->blocks;
          }

          gen_live   += gcThreadLiveWords(i,g);
          gen_blocks += gcThreadLiveBlocks(i,g);
      }

      debugBelch("%5d %7" FMT_Word " %9d", g, (W_)gen->max_blocks, mut);

      gen_slop = gen_blocks * BLOCK_SIZE_W - gen_live;

      debugBelch("%8" FMT_Word " %8d  %8d %9" FMT_Word " %9" FMT_Word "\n", gen_blocks, lge, compacts,
                 gen_live*(W_)sizeof(W_), gen_slop*(W_)sizeof(W_));
      tot_live += gen_live;
      tot_slop += gen_slop;
  }
  debugBelch("----------------------------------------------------------------------\n");
  debugBelch("%51s%9" FMT_Word " %9" FMT_Word "\n",
             "",tot_live*sizeof(W_),tot_slop*sizeof(W_));
  debugBelch("----------------------------------------------------------------------\n");
  debugBelch("\n");
}

/* -----------------------------------------------------------------------------
   Stats available via a programmatic interface, so eg. GHCi can time
   each compilation and expression evaluation.
   -------------------------------------------------------------------------- */

uint64_t getAllocations( void )
{
    return stats.allocated_bytes;
}

int getRTSStatsEnabled( void )
{
    return RtsFlags.GcFlags.giveStats != NO_GC_STATS;
}

void getRTSStats( RTSStats *s )
{
    Time current_elapsed = 0;
    Time current_cpu = 0;

    *s = stats;

    getProcessTimes(&current_cpu, &current_elapsed);
    s->cpu_ns = current_cpu - end_init_cpu;
    s->elapsed_ns = current_elapsed - end_init_elapsed;

    s->mutator_cpu_ns = current_cpu - end_init_cpu - stats.gc_cpu_ns;
    s->mutator_elapsed_ns = current_elapsed - end_init_elapsed -
        stats.gc_elapsed_ns;
}

/* -----------------------------------------------------------------------------
   Dumping stuff in the stats file, or via the debug message interface
   -------------------------------------------------------------------------- */

void
statsPrintf( char *s, ... )
{
    FILE *sf = RtsFlags.GcFlags.statsFile;
    va_list ap;

    va_start(ap,s);
    if (sf == NULL) {
        vdebugBelch(s,ap);
    } else {
        vfprintf(sf, s, ap);
    }
    va_end(ap);
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
