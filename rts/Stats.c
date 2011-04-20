/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Schedule.h"
#include "Stats.h"
#include "Profiling.h"
#include "GetTime.h"
#include "sm/Storage.h"
#include "sm/GC.h" // gc_alloc_block_sync, whitehole_spin
#include "sm/GCThread.h"
#include "sm/BlockAlloc.h"

#if USE_PAPI
#include "Papi.h"
#endif

/* huh? */
#define BIG_STRING_LEN              512

#define TICK_TO_DBL(t) ((double)(t) / TICKS_PER_SECOND)

static Ticks
    start_init_cpu, start_init_elapsed,
    end_init_cpu,   end_init_elapsed,
    start_exit_cpu, start_exit_elapsed,
    end_exit_cpu,   end_exit_elapsed;

static Ticks GC_tot_cpu  = 0;

static StgWord64 GC_tot_alloc      = 0;
static StgWord64 GC_tot_copied     = 0;

static StgWord64 GC_par_max_copied = 0;
static StgWord64 GC_par_avg_copied = 0;

#ifdef PROFILING
static Ticks RP_start_time  = 0, RP_tot_time  = 0;  // retainer prof user time
static Ticks RPe_start_time = 0, RPe_tot_time = 0;  // retainer prof elap time

static Ticks HC_start_time, HC_tot_time = 0;     // heap census prof user time
static Ticks HCe_start_time, HCe_tot_time = 0;   // heap census prof elap time
#endif

#ifdef PROFILING
#define PROF_VAL(x)   (x)
#else
#define PROF_VAL(x)   0
#endif

static lnat max_residency     = 0; // in words; for stats only
static lnat avg_residency     = 0;
static lnat residency_samples = 0; // for stats only
static lnat max_slop          = 0;

static lnat GC_end_faults = 0;

static Ticks *GC_coll_cpu = NULL;
static Ticks *GC_coll_elapsed = NULL;
static Ticks *GC_coll_max_pause = NULL;

static void statsFlush( void );
static void statsClose( void );

/* -----------------------------------------------------------------------------
   Current elapsed time
   ------------------------------------------------------------------------- */

Ticks stat_getElapsedTime(void)
{
    return getProcessElapsedTime() - start_init_elapsed;
}

/* ---------------------------------------------------------------------------
   Measure the current MUT time, for profiling
   ------------------------------------------------------------------------ */

double
mut_user_time( void )
{
    Ticks cpu;
    cpu = getProcessCPUTime();
    return TICK_TO_DBL(cpu - GC_tot_cpu - PROF_VAL(RP_tot_time + HC_tot_time));
}

#ifdef PROFILING
/*
  mut_user_time_during_RP() returns the MUT time during retainer profiling.
  The same is for mut_user_time_during_HC();
 */
double
mut_user_time_during_RP( void )
{
  return TICK_TO_DBL(RP_start_time - GC_tot_cpu - RP_tot_time - HC_tot_time);
}

double
mut_user_time_during_heap_census( void )
{
  return TICK_TO_DBL(HC_start_time - GC_tot_cpu - RP_tot_time - HC_tot_time);
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
    end_exit_cpu     = 0;
    end_exit_elapsed  = 0;

    GC_tot_alloc     = 0;
    GC_tot_copied    = 0;
    GC_par_max_copied = 0;
    GC_par_avg_copied = 0;
    GC_tot_cpu  = 0;

#ifdef PROFILING
    RP_start_time  = 0;
    RP_tot_time  = 0;
    RPe_start_time = 0;
    RPe_tot_time = 0;

    HC_start_time = 0;
    HC_tot_time = 0;
    HCe_start_time = 0;
    HCe_tot_time = 0;
#endif

    max_residency = 0;
    avg_residency = 0;
    residency_samples = 0;
    max_slop = 0;

    GC_end_faults = 0;
}    

/* ---------------------------------------------------------------------------
   initStats1() can be called after setupRtsFlags()
   ------------------------------------------------------------------------ */

void
initStats1 (void)
{
    nat i;
  
    if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
	statsPrintf("    Alloc    Copied     Live    GC    GC     TOT     TOT  Page Flts\n");
	statsPrintf("    bytes     bytes     bytes  user  elap    user    elap\n");
    }
    GC_coll_cpu = 
	(Ticks *)stgMallocBytes(
            sizeof(Ticks)*RtsFlags.GcFlags.generations,
	    "initStats");
    GC_coll_elapsed = 
	(Ticks *)stgMallocBytes(
	    sizeof(Ticks)*RtsFlags.GcFlags.generations,
	    "initStats");
    GC_coll_max_pause =
	(Ticks *)stgMallocBytes(
	    sizeof(Ticks)*RtsFlags.GcFlags.generations,
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

#if USE_PAPI
    /* We start counting events for the mutator
     * when garbage collection starts
     * we switch to the GC event set. */
    papi_start_mutator_count();

    /* This flag is needed to avoid counting the last GC */
    papi_is_reporting = 1;

#endif
}

/* -----------------------------------------------------------------------------
   stat_startExit and stat_endExit
   
   These two measure the time taken in shutdownHaskell().
   -------------------------------------------------------------------------- */

void
stat_startExit(void)
{
    getProcessTimes(&start_exit_cpu, &start_exit_elapsed);

#if USE_PAPI
    /* We stop counting mutator events
     * GC events are not being counted at this point */
    papi_stop_mutator_count();

    /* This flag is needed, because GC is run once more after this function */
    papi_is_reporting = 0;
#endif
}

void
stat_endExit(void)
{
    getProcessTimes(&end_exit_cpu, &end_exit_elapsed);
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each GC
   -------------------------------------------------------------------------- */

static nat rub_bell = 0;

void
stat_startGC (gc_thread *gct)
{
    nat bell = RtsFlags.GcFlags.ringBell;

    if (bell) {
	if (bell > 1) {
	    debugBelch(" GC ");
	    rub_bell = 1;
	} else {
	    debugBelch("\007");
	}
    }

#if USE_PAPI
    if(papi_is_reporting) {
      /* Switch to counting GC events */
      papi_stop_mutator_count();
      papi_start_gc_count();
    }
#endif

    getProcessTimes(&gct->gc_start_cpu, &gct->gc_start_elapsed);
    gct->gc_start_thread_cpu  = getThreadCPUTime();

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS)
    {
        gct->gc_start_faults = getPageFaults();
    }
}

void
stat_gcWorkerThreadStart (gc_thread *gct)
{
    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS)
    {
        getProcessTimes(&gct->gc_start_cpu, &gct->gc_start_elapsed);
        gct->gc_start_thread_cpu  = getThreadCPUTime();
    }
}

void
stat_gcWorkerThreadDone (gc_thread *gct)
{
    Ticks thread_cpu, elapsed, gc_cpu, gc_elapsed;

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS)
    {
        elapsed    = getProcessElapsedTime();
        thread_cpu = getThreadCPUTime();

        gc_cpu     = thread_cpu - gct->gc_start_thread_cpu;
        gc_elapsed = elapsed    - gct->gc_start_elapsed;
    
        taskDoneGC(gct->cap->running_task, gc_cpu, gc_elapsed);
    }
}

/* -----------------------------------------------------------------------------
   Called at the end of each GC
   -------------------------------------------------------------------------- */

void
stat_endGC (gc_thread *gct,
            lnat alloc, lnat live, lnat copied, nat gen,
            lnat max_copied, lnat avg_copied, lnat slop)
{
    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS ||
        RtsFlags.ProfFlags.doHeapProfile)
        // heap profiling needs GC_tot_time
    {
        Ticks cpu, elapsed, thread_gc_cpu, gc_cpu, gc_elapsed;
	
        getProcessTimes(&cpu, &elapsed);
        gc_elapsed    = elapsed - gct->gc_start_elapsed;

        thread_gc_cpu = getThreadCPUTime() - gct->gc_start_thread_cpu;

        gc_cpu = cpu - gct->gc_start_cpu;

        taskDoneGC(gct->cap->running_task, thread_gc_cpu, gc_elapsed);

        if (RtsFlags.GcFlags.giveStats == VERBOSE_GC_STATS) {
	    nat faults = getPageFaults();
	    
	    statsPrintf("%9ld %9ld %9ld",
		    alloc*sizeof(W_), copied*sizeof(W_), 
			live*sizeof(W_));
            statsPrintf(" %5.2f %5.2f %7.2f %7.2f %4ld %4ld  (Gen: %2d)\n",
                    TICK_TO_DBL(gc_cpu),
		    TICK_TO_DBL(gc_elapsed),
		    TICK_TO_DBL(cpu),
		    TICK_TO_DBL(elapsed - start_init_elapsed),
		    faults - gct->gc_start_faults,
                        gct->gc_start_faults - GC_end_faults,
                    gen);

            GC_end_faults = faults;
	    statsFlush();
	}

        GC_coll_cpu[gen] += gc_cpu;
        GC_coll_elapsed[gen] += gc_elapsed;
        if (GC_coll_max_pause[gen] < gc_elapsed) {
            GC_coll_max_pause[gen] = gc_elapsed;
        }

	GC_tot_copied += (StgWord64) copied;
	GC_tot_alloc  += (StgWord64) alloc;
        GC_par_max_copied += (StgWord64) max_copied;
        GC_par_avg_copied += (StgWord64) avg_copied;
	GC_tot_cpu   += gc_cpu;

	if (gen == RtsFlags.GcFlags.generations-1) { /* major GC? */
	    if (live > max_residency) {
		max_residency = live;
	    }
	    residency_samples++;
	    avg_residency += live;
	}

        if (slop > max_slop) max_slop = slop;
    }

    if (rub_bell) {
	debugBelch("\b\b\b  \b\b\b");
	rub_bell = 0;
    }

#if USE_PAPI
    if(papi_is_reporting) {
      /* Switch to counting mutator events */
      if (gen == 0) {
          papi_stop_gc0_count();
      } else {
          papi_stop_gc1_count();
      }
      papi_start_mutator_count();
    }
#endif
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each Retainer Profiliing
   -------------------------------------------------------------------------- */
#ifdef PROFILING
void
stat_startRP(void)
{
    Ticks user, elapsed;
    getProcessTimes( &user, &elapsed );

    RP_start_time = user;
    RPe_start_time = elapsed;
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of each Retainer Profiliing
   -------------------------------------------------------------------------- */

#ifdef PROFILING
void
stat_endRP(
  nat retainerGeneration,
#ifdef DEBUG_RETAINER
  nat maxCStackSize,
  int maxStackSize,
#endif
  double averageNumVisit)
{
    Ticks user, elapsed;
    getProcessTimes( &user, &elapsed );

    RP_tot_time += user - RP_start_time;
    RPe_tot_time += elapsed - RPe_start_time;

  fprintf(prof_file, "Retainer Profiling: %d, at %f seconds\n", 
    retainerGeneration, mut_user_time_during_RP());
#ifdef DEBUG_RETAINER
  fprintf(prof_file, "\tMax C stack size = %u\n", maxCStackSize);
  fprintf(prof_file, "\tMax auxiliary stack size = %u\n", maxStackSize);
#endif
  fprintf(prof_file, "\tAverage number of visits per object = %f\n", averageNumVisit);
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the beginning of each heap census
   -------------------------------------------------------------------------- */
#ifdef PROFILING
void
stat_startHeapCensus(void)
{
    Ticks user, elapsed;
    getProcessTimes( &user, &elapsed );

    HC_start_time = user;
    HCe_start_time = elapsed;
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of each heap census
   -------------------------------------------------------------------------- */
#ifdef PROFILING
void
stat_endHeapCensus(void) 
{
    Ticks user, elapsed;
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

#ifdef DEBUG
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
    showStgWord64(counter,temp,rtsTrue/*commas*/); \
    statsPrintf("  (" #counter ")  : %s\n",temp);				\
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

void
stat_exit(int alloc)
{
    generation *gen;
    Ticks gc_cpu = 0;
    Ticks gc_elapsed = 0;
    Ticks init_cpu = 0;
    Ticks init_elapsed = 0;
    Ticks mut_cpu = 0;
    Ticks mut_elapsed = 0;
    Ticks exit_cpu = 0;
    Ticks exit_elapsed = 0;

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {

	char temp[BIG_STRING_LEN];
	Ticks tot_cpu;
	Ticks tot_elapsed;
	nat i, g, total_collections = 0;

	getProcessTimes( &tot_cpu, &tot_elapsed );
	tot_elapsed -= start_init_elapsed;

	GC_tot_alloc += alloc;

	/* Count total garbage collections */
	for (g = 0; g < RtsFlags.GcFlags.generations; g++)
	    total_collections += generations[g].collections;

	/* avoid divide by zero if tot_cpu is measured as 0.00 seconds -- SDM */
	if (tot_cpu  == 0.0)  tot_cpu = 1;
	if (tot_elapsed == 0.0) tot_elapsed = 1;
	
	if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
	    statsPrintf("%9ld %9.9s %9.9s", (lnat)alloc*sizeof(W_), "", "");
	    statsPrintf(" %5.2f %5.2f\n\n", 0.0, 0.0);
	}

        for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
            gc_cpu     += GC_coll_cpu[i];
            gc_elapsed += GC_coll_elapsed[i];
        }

	if (RtsFlags.GcFlags.giveStats >= SUMMARY_GC_STATS) {
	    showStgWord64(GC_tot_alloc*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%16s bytes allocated in the heap\n", temp);

	    showStgWord64(GC_tot_copied*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%16s bytes copied during GC\n", temp);

            if ( residency_samples > 0 ) {
		showStgWord64(max_residency*sizeof(W_), 
				     temp, rtsTrue/*commas*/);
		statsPrintf("%16s bytes maximum residency (%ld sample(s))\n",
			temp, residency_samples);
	    }

	    showStgWord64(max_slop*sizeof(W_), temp, rtsTrue/*commas*/);
	    statsPrintf("%16s bytes maximum slop\n", temp);

	    statsPrintf("%16ld MB total memory in use (%ld MB lost due to fragmentation)\n\n", 
                        peak_mblocks_allocated * MBLOCK_SIZE_W / (1024 * 1024 / sizeof(W_)),
                        (peak_mblocks_allocated * BLOCKS_PER_MBLOCK * BLOCK_SIZE_W - hw_alloc_blocks * BLOCK_SIZE_W) / (1024 * 1024 / sizeof(W_)));

	    /* Print garbage collections in each gen */
            statsPrintf("                                    Tot time (elapsed)  Avg pause  Max pause\n");
            for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
                gen = &generations[g];
                statsPrintf("  Gen %2d     %5d colls, %5d par   %5.2fs   %5.2fs     %3.4fs    %3.4fs\n",
                            gen->no,
                            gen->collections,
                            gen->par_collections,
                            TICK_TO_DBL(GC_coll_cpu[g]),
                            TICK_TO_DBL(GC_coll_elapsed[g]),
                            gen->collections == 0 ? 0 : TICK_TO_DBL(GC_coll_elapsed[g] / gen->collections),
                            TICK_TO_DBL(GC_coll_max_pause[g]));
            }

#if defined(THREADED_RTS)
            if (RtsFlags.ParFlags.parGcEnabled) {
                statsPrintf("\n  Parallel GC work balance: %.2f (%ld / %ld, ideal %d)\n", 
                            (double)GC_par_avg_copied / (double)GC_par_max_copied,
                            (lnat)GC_par_avg_copied, (lnat)GC_par_max_copied,
                            RtsFlags.ParFlags.nNodes
                    );
            }
#endif
            statsPrintf("\n");

#if defined(THREADED_RTS)
	    {
		nat i;
		Task *task;
                statsPrintf("                        MUT time (elapsed)       GC time  (elapsed)\n");
		for (i = 0, task = all_tasks; 
		     task != NULL; 
		     i++, task = task->all_link) {
		    statsPrintf("  Task %2d %-8s :  %6.2fs    (%6.2fs)     %6.2fs    (%6.2fs)\n",
				i,
				(task->worker) ? "(worker)" : "(bound)",
				TICK_TO_DBL(task->mut_time),
				TICK_TO_DBL(task->mut_etime),
				TICK_TO_DBL(task->gc_time),
				TICK_TO_DBL(task->gc_etime));
		}
	    }

	    statsPrintf("\n");

            {
                nat i;
                lnat sparks_created   = 0;
                lnat sparks_dud       = 0;
                lnat sparks_converted = 0;
                lnat sparks_gcd       = 0;
                lnat sparks_fizzled   = 0;
                for (i = 0; i < n_capabilities; i++) {
                    sparks_created   += capabilities[i].sparks_created;
                    sparks_dud       += capabilities[i].sparks_dud;
                    sparks_converted += capabilities[i].sparks_converted;
                    sparks_gcd       += capabilities[i].sparks_gcd;
                    sparks_fizzled   += capabilities[i].sparks_fizzled;
                }

                statsPrintf("  SPARKS: %ld (%ld converted, %ld dud, %ld GC'd, %ld fizzled)\n\n",
                            sparks_created + sparks_dud, sparks_converted, sparks_dud, sparks_gcd, sparks_fizzled);
            }
#endif

            init_cpu     = end_init_cpu - start_init_cpu;
            init_elapsed = end_init_elapsed - start_init_elapsed;

            exit_cpu     = end_exit_cpu - start_exit_cpu;
            exit_elapsed = end_exit_elapsed - start_exit_elapsed;

	    statsPrintf("  INIT    time  %6.2fs  (%6.2fs elapsed)\n",
                        TICK_TO_DBL(init_cpu), TICK_TO_DBL(init_elapsed));

            mut_elapsed = start_exit_elapsed - end_init_elapsed - gc_elapsed;

            mut_cpu = start_exit_cpu - end_init_cpu - gc_cpu
                - PROF_VAL(RP_tot_time + HC_tot_time);
            if (mut_cpu < 0) { mut_cpu = 0; }

            statsPrintf("  MUT     time  %6.2fs  (%6.2fs elapsed)\n",
                        TICK_TO_DBL(mut_cpu), TICK_TO_DBL(mut_elapsed));
            statsPrintf("  GC      time  %6.2fs  (%6.2fs elapsed)\n",
                        TICK_TO_DBL(gc_cpu), TICK_TO_DBL(gc_elapsed));

#ifdef PROFILING
	    statsPrintf("  RP      time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(RP_tot_time), TICK_TO_DBL(RPe_tot_time));
	    statsPrintf("  PROF    time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(HC_tot_time), TICK_TO_DBL(HCe_tot_time));
#endif 
	    statsPrintf("  EXIT    time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(exit_cpu), TICK_TO_DBL(exit_elapsed));
	    statsPrintf("  Total   time  %6.2fs  (%6.2fs elapsed)\n\n",
		    TICK_TO_DBL(tot_cpu), TICK_TO_DBL(tot_elapsed));
#ifndef THREADED_RTS
	    statsPrintf("  %%GC     time     %5.1f%%  (%.1f%% elapsed)\n\n",
		    TICK_TO_DBL(gc_cpu)*100/TICK_TO_DBL(tot_cpu),
		    TICK_TO_DBL(gc_elapsed)*100/TICK_TO_DBL(tot_elapsed));
#endif

	    if (tot_cpu - GC_tot_cpu - PROF_VAL(RP_tot_time + HC_tot_time) == 0)
		showStgWord64(0, temp, rtsTrue/*commas*/);
	    else
		showStgWord64(
		    (StgWord64)((GC_tot_alloc*sizeof(W_))/
			     TICK_TO_DBL(tot_cpu - GC_tot_cpu - 
					 PROF_VAL(RP_tot_time + HC_tot_time))),
		    temp, rtsTrue/*commas*/);
	    
	    statsPrintf("  Alloc rate    %s bytes per MUT second\n\n", temp);
	
	    statsPrintf("  Productivity %5.1f%% of total user, %.1f%% of total elapsed\n\n",
		    TICK_TO_DBL(tot_cpu - GC_tot_cpu - 
				PROF_VAL(RP_tot_time + HC_tot_time) - init_cpu) * 100 
		    / TICK_TO_DBL(tot_cpu), 
		    TICK_TO_DBL(tot_cpu - GC_tot_cpu - 
				PROF_VAL(RP_tot_time + HC_tot_time) - init_cpu) * 100 
		    / TICK_TO_DBL(tot_elapsed));

            /*
            TICK_PRINT(1);
            TICK_PRINT(2);
	    REPORT(TOTAL_CALLS);
            TICK_PRINT_TOT(1);
            TICK_PRINT_TOT(2);
            */

#if USE_PAPI
            papi_stats_report();
#endif
#if defined(THREADED_RTS) && defined(PROF_SPIN)
            {
                nat g;
                
                statsPrintf("gc_alloc_block_sync: %"FMT_Word64"\n", gc_alloc_block_sync.spin);
                statsPrintf("whitehole_spin: %"FMT_Word64"\n", whitehole_spin);
                for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
                    statsPrintf("gen[%d].sync: %"FMT_Word64"\n", g, generations[g].sync.spin);
                }
            }
#endif
	}

	if (RtsFlags.GcFlags.giveStats == ONELINE_GC_STATS) {
      char *fmt1, *fmt2;
      if (RtsFlags.MiscFlags.machineReadable) {
          fmt1 = " [(\"bytes allocated\", \"%llu\")\n";
          fmt2 = " ,(\"num_GCs\", \"%d\")\n"
                 " ,(\"average_bytes_used\", \"%ld\")\n"
                 " ,(\"max_bytes_used\", \"%ld\")\n"
                 " ,(\"num_byte_usage_samples\", \"%ld\")\n"
                 " ,(\"peak_megabytes_allocated\", \"%lu\")\n"
                 " ,(\"init_cpu_seconds\", \"%.2f\")\n"
                 " ,(\"init_wall_seconds\", \"%.2f\")\n"
                 " ,(\"mutator_cpu_seconds\", \"%.2f\")\n"
                 " ,(\"mutator_wall_seconds\", \"%.2f\")\n"
                 " ,(\"GC_cpu_seconds\", \"%.2f\")\n"
                 " ,(\"GC_wall_seconds\", \"%.2f\")\n"
                 " ]\n";
      }
      else {
          fmt1 = "<<ghc: %llu bytes, ";
          fmt2 = "%d GCs, %ld/%ld avg/max bytes residency (%ld samples), %luM in use, %.2f INIT (%.2f elapsed), %.2f MUT (%.2f elapsed), %.2f GC (%.2f elapsed) :ghc>>\n";
      }
	  /* print the long long separately to avoid bugginess on mingwin (2001-07-02, mingw-0.5) */
	  statsPrintf(fmt1, GC_tot_alloc*(StgWord64)sizeof(W_));
	  statsPrintf(fmt2,
		    total_collections,
		    residency_samples == 0 ? 0 : 
		        avg_residency*sizeof(W_)/residency_samples, 
		    max_residency*sizeof(W_), 
		    residency_samples,
		    (unsigned long)(peak_mblocks_allocated * MBLOCK_SIZE / (1024L * 1024L)),
		    TICK_TO_DBL(init_cpu), TICK_TO_DBL(init_elapsed),
		    TICK_TO_DBL(mut_cpu), TICK_TO_DBL(mut_elapsed),
		    TICK_TO_DBL(gc_cpu), TICK_TO_DBL(gc_elapsed));
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

/* -----------------------------------------------------------------------------
   stat_describe_gens

   Produce some detailed info on the state of the generational GC.
   -------------------------------------------------------------------------- */
void
statDescribeGens(void)
{
  nat g, mut, lge, i;
  lnat gen_slop;
  lnat tot_live, tot_slop;
  lnat gen_live, gen_blocks;
  bdescr *bd;
  generation *gen;
  
  debugBelch(
"----------------------------------------------------------\n"
"  Gen     Max  Mut-list  Blocks    Large     Live     Slop\n"
"       Blocks     Bytes          Objects                  \n"
"----------------------------------------------------------\n");

  tot_live = 0;
  tot_slop = 0;

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      gen = &generations[g];

      for (bd = gen->large_objects, lge = 0; bd; bd = bd->link) {
          lge++;
      }

      gen_live   = genLiveWords(gen);
      gen_blocks = genLiveBlocks(gen);

      mut = 0;
      for (i = 0; i < n_capabilities; i++) {
          mut += countOccupied(capabilities[i].mut_lists[g]);

          // Add the pinned object block.
          bd = capabilities[i].pinned_object_block;
          if (bd != NULL) {
              gen_live   += bd->free - bd->start;
              gen_blocks += bd->blocks;
          }

          gen_live   += gcThreadLiveWords(i,g);
          gen_live   += gcThreadLiveWords(i,g);
          gen_blocks += gcThreadLiveBlocks(i,g);
      }

      debugBelch("%5d %7d %9d", g, gen->max_blocks, mut);

      gen_slop = gen_blocks * BLOCK_SIZE_W - gen_live;

      debugBelch("%8ld %8d %8ld %8ld\n", gen_blocks, lge,
                 gen_live*sizeof(W_), gen_slop*sizeof(W_));
      tot_live += gen_live;
      tot_slop += gen_slop;
  }
  debugBelch("----------------------------------------------------------\n");
  debugBelch("%41s%8ld %8ld\n","",tot_live*sizeof(W_),tot_slop*sizeof(W_));
  debugBelch("----------------------------------------------------------\n");
  debugBelch("\n");
}

/* -----------------------------------------------------------------------------
   Stats available via a programmatic interface, so eg. GHCi can time
   each compilation and expression evaluation.
   -------------------------------------------------------------------------- */

extern HsInt64 getAllocations( void ) 
{ return (HsInt64)GC_tot_alloc * sizeof(W_); }

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
