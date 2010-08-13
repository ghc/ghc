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

#if USE_PAPI
#include "Papi.h"
#endif

/* huh? */
#define BIG_STRING_LEN              512

#define TICK_TO_DBL(t) ((double)(t) / TICKS_PER_SECOND)

static Ticks ElapsedTimeStart = 0;

static Ticks InitUserTime     = 0;
static Ticks InitElapsedTime  = 0;
static Ticks InitElapsedStamp = 0;

static Ticks MutUserTime      = 0;
static Ticks MutElapsedTime   = 0;
static Ticks MutElapsedStamp  = 0;

static Ticks ExitUserTime     = 0;
static Ticks ExitElapsedTime  = 0;

static StgWord64 GC_tot_alloc        = 0;
static StgWord64 GC_tot_copied       = 0;

static StgWord64 GC_par_max_copied = 0;
static StgWord64 GC_par_avg_copied = 0;

static Ticks GC_start_time = 0,  GC_tot_time  = 0;  /* User GC Time */
static Ticks GCe_start_time = 0, GCe_tot_time = 0;  /* Elapsed GC time */

#ifdef PROFILING
static Ticks RP_start_time  = 0, RP_tot_time  = 0;  /* retainer prof user time */
static Ticks RPe_start_time = 0, RPe_tot_time = 0;  /* retainer prof elap time */

static Ticks HC_start_time, HC_tot_time = 0;     // heap census prof user time
static Ticks HCe_start_time, HCe_tot_time = 0;   // heap census prof elap time
#endif

#ifdef PROFILING
#define PROF_VAL(x)   (x)
#else
#define PROF_VAL(x)   0
#endif

static lnat MaxResidency = 0;     // in words; for stats only
static lnat AvgResidency = 0;
static lnat ResidencySamples = 0; // for stats only
static lnat MaxSlop = 0;

static lnat GC_start_faults = 0, GC_end_faults = 0;

static Ticks *GC_coll_times = NULL;
static Ticks *GC_coll_etimes = NULL;

static void statsFlush( void );
static void statsClose( void );

Ticks stat_getElapsedGCTime(void)
{
    return GCe_tot_time;
}

Ticks stat_getElapsedTime(void)
{
    return getProcessElapsedTime() - ElapsedTimeStart;
}

/* mut_user_time_during_GC() and mut_user_time()
 *
 * The former function can be used to get the current mutator time
 * *during* a GC, i.e. between stat_startGC and stat_endGC.  This is
 * used in the heap profiler for accurately time stamping the heap
 * sample.  
 *
 * ATTENTION: mut_user_time_during_GC() relies on GC_start_time being 
 *	      defined in stat_startGC() - to minimise system calls, 
 *	      GC_start_time is, however, only defined when really needed (check
 *	      stat_startGC() for details)
 */
double
mut_user_time_during_GC( void )
{
  return TICK_TO_DBL(GC_start_time - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time));
}

double
mut_user_time( void )
{
    Ticks user;
    user = getProcessCPUTime();
    return TICK_TO_DBL(user - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time));
}

#ifdef PROFILING
/*
  mut_user_time_during_RP() is similar to mut_user_time_during_GC();
  it returns the MUT time during retainer profiling.
  The same is for mut_user_time_during_HC();
 */
double
mut_user_time_during_RP( void )
{
  return TICK_TO_DBL(RP_start_time - GC_tot_time - RP_tot_time - HC_tot_time);
}

double
mut_user_time_during_heap_census( void )
{
  return TICK_TO_DBL(HC_start_time - GC_tot_time - RP_tot_time - HC_tot_time);
}
#endif /* PROFILING */

// initStats0() has no dependencies, it can be called right at the beginning
void
initStats0(void)
{
    ElapsedTimeStart = 0;

    InitUserTime     = 0;
    InitElapsedTime  = 0;
    InitElapsedStamp = 0;

    MutUserTime      = 0;
    MutElapsedTime   = 0;
    MutElapsedStamp  = 0;

    ExitUserTime     = 0;
    ExitElapsedTime  = 0;

    GC_tot_alloc     = 0;
    GC_tot_copied    = 0;
    GC_par_max_copied = 0;
    GC_par_avg_copied = 0;
    GC_start_time = 0;
    GC_tot_time  = 0;
    GCe_start_time = 0;
    GCe_tot_time = 0;

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

    MaxResidency = 0;
    AvgResidency = 0;
    ResidencySamples = 0;
    MaxSlop = 0;

    GC_start_faults = 0;
    GC_end_faults = 0;
}    

// initStats1() can be called after setupRtsFlags()
void
initStats1 (void)
{
    nat i;
  
    if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
	statsPrintf("    Alloc    Copied     Live    GC    GC     TOT     TOT  Page Flts\n");
	statsPrintf("    bytes     bytes     bytes  user  elap    user    elap\n");
    }
    GC_coll_times = 
	(Ticks *)stgMallocBytes(
	    sizeof(Ticks)*RtsFlags.GcFlags.generations,
	    "initStats");
    GC_coll_etimes = 
	(Ticks *)stgMallocBytes(
	    sizeof(Ticks)*RtsFlags.GcFlags.generations,
	    "initStats");
    for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
	GC_coll_times[i] = 0;
	GC_coll_etimes[i] = 0;
    }
}

/* -----------------------------------------------------------------------------
   Initialisation time...
   -------------------------------------------------------------------------- */

void
stat_startInit(void)
{
    Ticks elapsed;

    elapsed = getProcessElapsedTime();
    ElapsedTimeStart = elapsed;
}

void 
stat_endInit(void)
{
    Ticks user, elapsed;

    getProcessTimes(&user, &elapsed);

    InitUserTime = user;
    InitElapsedStamp = elapsed; 
    if (ElapsedTimeStart > elapsed) {
	InitElapsedTime = 0;
    } else {
	InitElapsedTime = elapsed - ElapsedTimeStart;
    }
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
    Ticks user, elapsed;

    getProcessTimes(&user, &elapsed);

    MutElapsedStamp = elapsed;
    MutElapsedTime = elapsed - GCe_tot_time -
	PROF_VAL(RPe_tot_time + HCe_tot_time) - InitElapsedStamp;
    if (MutElapsedTime < 0) { MutElapsedTime = 0; }	/* sometimes -0.00 */

    MutUserTime = user - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time) - InitUserTime;
    if (MutUserTime < 0) { MutUserTime = 0; }

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
    Ticks user, elapsed;

    getProcessTimes(&user, &elapsed);

    ExitUserTime = user - MutUserTime - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time) - InitUserTime;
    ExitElapsedTime = elapsed - MutElapsedStamp;
    if (ExitUserTime < 0) {
	ExitUserTime = 0;
    }
    if (ExitElapsedTime < 0) {
	ExitElapsedTime = 0;
    }
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each GC
   -------------------------------------------------------------------------- */

static nat rub_bell = 0;

/*  initialise global variables needed during GC
 *
 *  * GC_start_time is read in mut_user_time_during_GC(), which in turn is 
 *    needed if either PROFILING or DEBUGing is enabled
 */
void
stat_startGC(void)
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

#if defined(PROFILING) || defined(DEBUG)
    GC_start_time = getProcessCPUTime();  // needed in mut_user_time_during_GC()
#endif

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
#if !defined(PROFILING) && !defined(DEBUG)
        GC_start_time = getProcessCPUTime();
#endif
	GCe_start_time = getProcessElapsedTime();
	if (RtsFlags.GcFlags.giveStats) {
	    GC_start_faults = getPageFaults();
	}
    }

#if USE_PAPI
    if(papi_is_reporting) {
      /* Switch to counting GC events */
      papi_stop_mutator_count();
      papi_start_gc_count();
    }
#endif

}

/* -----------------------------------------------------------------------------
   Called at the end of each GC
   -------------------------------------------------------------------------- */

void
stat_endGC (lnat alloc, lnat live, lnat copied, lnat gen,
            lnat max_copied, lnat avg_copied, lnat slop)
{
    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
	Ticks time, etime, gc_time, gc_etime;
	
	getProcessTimes(&time, &etime);
	gc_time  = time - GC_start_time;
	gc_etime = etime - GCe_start_time;
	
	if (RtsFlags.GcFlags.giveStats == VERBOSE_GC_STATS) {
	    nat faults = getPageFaults();
	    
	    statsPrintf("%9ld %9ld %9ld",
		    alloc*sizeof(W_), copied*sizeof(W_), 
			live*sizeof(W_));
	    statsPrintf(" %5.2f %5.2f %7.2f %7.2f %4ld %4ld  (Gen: %2ld)\n", 
		    TICK_TO_DBL(gc_time),
		    TICK_TO_DBL(gc_etime),
		    TICK_TO_DBL(time),
		    TICK_TO_DBL(etime - ElapsedTimeStart),
		    faults - GC_start_faults,
		    GC_start_faults - GC_end_faults,
		    gen);

	    GC_end_faults = faults;
	    statsFlush();
	}

	GC_coll_times[gen] += gc_time;
	GC_coll_etimes[gen] += gc_etime;

	GC_tot_copied += (StgWord64) copied;
	GC_tot_alloc  += (StgWord64) alloc;
        GC_par_max_copied += (StgWord64) max_copied;
        GC_par_avg_copied += (StgWord64) avg_copied;
	GC_tot_time   += gc_time;
	GCe_tot_time  += gc_etime;
	
#if defined(THREADED_RTS)
	{
	    Task *task;
	    if ((task = myTask()) != NULL) {
		task->gc_time += gc_time;
		task->gc_etime += gc_etime;
	    }
	}
#endif

	if (gen == RtsFlags.GcFlags.generations-1) { /* major GC? */
	    if (live > MaxResidency) {
		MaxResidency = live;
	    }
	    ResidencySamples++;
	    AvgResidency += live;
	}

        if (slop > MaxSlop) MaxSlop = slop;
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

extern lnat hw_alloc_blocks;

void
stat_exit(int alloc)
{
    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {

	char temp[BIG_STRING_LEN];
	Ticks time;
	Ticks etime;
	nat g, total_collections = 0;

	getProcessTimes( &time, &etime );
	etime -= ElapsedTimeStart;

	GC_tot_alloc += alloc;

	/* Count total garbage collections */
	for (g = 0; g < RtsFlags.GcFlags.generations; g++)
	    total_collections += generations[g].collections;

	/* avoid divide by zero if time is measured as 0.00 seconds -- SDM */
	if (time  == 0.0)  time = 1;
	if (etime == 0.0) etime = 1;
	
	if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
	    statsPrintf("%9ld %9.9s %9.9s", (lnat)alloc*sizeof(W_), "", "");
	    statsPrintf(" %5.2f %5.2f\n\n", 0.0, 0.0);
	}

	if (RtsFlags.GcFlags.giveStats >= SUMMARY_GC_STATS) {
	    showStgWord64(GC_tot_alloc*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%16s bytes allocated in the heap\n", temp);

	    showStgWord64(GC_tot_copied*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%16s bytes copied during GC\n", temp);

	    if ( ResidencySamples > 0 ) {
		showStgWord64(MaxResidency*sizeof(W_), 
				     temp, rtsTrue/*commas*/);
		statsPrintf("%16s bytes maximum residency (%ld sample(s))\n",
			temp, ResidencySamples);
	    }

	    showStgWord64(MaxSlop*sizeof(W_), temp, rtsTrue/*commas*/);
	    statsPrintf("%16s bytes maximum slop\n", temp);

	    statsPrintf("%16ld MB total memory in use (%ld MB lost due to fragmentation)\n\n", 
                        mblocks_allocated * MBLOCK_SIZE_W / (1024 * 1024 / sizeof(W_)),
                        (mblocks_allocated * MBLOCK_SIZE_W - hw_alloc_blocks * BLOCK_SIZE_W) / (1024 * 1024 / sizeof(W_)));

	    /* Print garbage collections in each gen */
	    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
		statsPrintf("  Generation %d: %5d collections, %5d parallel, %5.2fs, %5.2fs elapsed\n", 
                            g, generations[g].collections, 
                            generations[g].par_collections,
                        TICK_TO_DBL(GC_coll_times[g]),
                        TICK_TO_DBL(GC_coll_etimes[g]));
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
                lnat sparks_converted = 0;
                lnat sparks_pruned    = 0;
                for (i = 0; i < n_capabilities; i++) {
                    sparks_created   += capabilities[i].sparks_created;
                    sparks_converted += capabilities[i].sparks_converted;
                    sparks_pruned    += capabilities[i].sparks_pruned;
                }

                statsPrintf("  SPARKS: %ld (%ld converted, %ld pruned)\n\n",
                            sparks_created, sparks_converted, sparks_pruned);
            }
#endif

	    statsPrintf("  INIT  time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(InitUserTime), TICK_TO_DBL(InitElapsedTime));
	    statsPrintf("  MUT   time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(MutUserTime), TICK_TO_DBL(MutElapsedTime));
	    statsPrintf("  GC    time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(GC_tot_time), TICK_TO_DBL(GCe_tot_time));
#ifdef PROFILING
	    statsPrintf("  RP    time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(RP_tot_time), TICK_TO_DBL(RPe_tot_time));
	    statsPrintf("  PROF  time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(HC_tot_time), TICK_TO_DBL(HCe_tot_time));
#endif 
	    statsPrintf("  EXIT  time  %6.2fs  (%6.2fs elapsed)\n",
		    TICK_TO_DBL(ExitUserTime), TICK_TO_DBL(ExitElapsedTime));
	    statsPrintf("  Total time  %6.2fs  (%6.2fs elapsed)\n\n",
		    TICK_TO_DBL(time), TICK_TO_DBL(etime));
	    statsPrintf("  %%GC time     %5.1f%%  (%.1f%% elapsed)\n\n",
		    TICK_TO_DBL(GC_tot_time)*100/TICK_TO_DBL(time),
		    TICK_TO_DBL(GCe_tot_time)*100/TICK_TO_DBL(etime));

	    if (time - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time) == 0)
		showStgWord64(0, temp, rtsTrue/*commas*/);
	    else
		showStgWord64(
		    (StgWord64)((GC_tot_alloc*sizeof(W_))/
			     TICK_TO_DBL(time - GC_tot_time - 
					 PROF_VAL(RP_tot_time + HC_tot_time))),
		    temp, rtsTrue/*commas*/);
	    
	    statsPrintf("  Alloc rate    %s bytes per MUT second\n\n", temp);
	
	    statsPrintf("  Productivity %5.1f%% of total user, %.1f%% of total elapsed\n\n",
		    TICK_TO_DBL(time - GC_tot_time - 
				PROF_VAL(RP_tot_time + HC_tot_time) - InitUserTime) * 100 
		    / TICK_TO_DBL(time), 
		    TICK_TO_DBL(time - GC_tot_time - 
				PROF_VAL(RP_tot_time + HC_tot_time) - InitUserTime) * 100 
		    / TICK_TO_DBL(etime));

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
                    statsPrintf("gen[%d].sync_large_objects: %"FMT_Word64"\n", g, generations[g].sync_large_objects.spin);
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
		    ResidencySamples == 0 ? 0 : 
		        AvgResidency*sizeof(W_)/ResidencySamples, 
		    MaxResidency*sizeof(W_), 
		    ResidencySamples,
		    (unsigned long)(peak_mblocks_allocated * MBLOCK_SIZE / (1024L * 1024L)),
		    TICK_TO_DBL(InitUserTime), TICK_TO_DBL(InitElapsedTime),
		    TICK_TO_DBL(MutUserTime), TICK_TO_DBL(MutElapsedTime),
		    TICK_TO_DBL(GC_tot_time), TICK_TO_DBL(GCe_tot_time));
	}

	statsFlush();
	statsClose();
    }

    if (GC_coll_times)
      stgFree(GC_coll_times);
    GC_coll_times = NULL;
    if (GC_coll_etimes)
      stgFree(GC_coll_etimes);
    GC_coll_etimes = NULL;
}

/* -----------------------------------------------------------------------------
   stat_describe_gens

   Produce some detailed info on the state of the generational GC.
   -------------------------------------------------------------------------- */
void
statDescribeGens(void)
{
  nat g, mut, lge;
  lnat live, slop;
  lnat tot_live, tot_slop;
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
      mut = 0;
      for (bd = generations[g].mut_list; bd != NULL; bd = bd->link) {
	  mut += (bd->free - bd->start) * sizeof(W_);
      }

      gen = &generations[g];

      debugBelch("%5d %7d %9d", g, gen->max_blocks, mut);

      for (bd = gen->large_objects, lge = 0; bd; bd = bd->link) {
          lge++;
      }
      live = gen->n_words + countOccupied(gen->large_objects);
      slop = (gen->n_blocks + gen->n_large_blocks) * BLOCK_SIZE_W - live;
      debugBelch("%8d %8d %8ld %8ld\n", gen->n_blocks, lge,
                 live*sizeof(W_), slop*sizeof(W_));
      tot_live += live;
      tot_slop += slop;
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
