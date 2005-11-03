/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "MBlock.h"
#include "Schedule.h"
#include "Stats.h"
#include "ParTicky.h"                       /* ToDo: move into Rts.h */
#include "Profiling.h"
#include "Storage.h"
#include "GetTime.h"

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

static ullong GC_tot_alloc        = 0;
static ullong GC_tot_copied       = 0;
static ullong GC_tot_scavd_copied = 0;

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

static lnat GC_start_faults = 0, GC_end_faults = 0;

static Ticks *GC_coll_times;

static void statsPrintf( char *s, ... ) 
    GNUC3_ATTRIBUTE(format (printf, 1, 2));

static void statsFlush( void );
static void statsClose( void );

Ticks stat_getElapsedGCTime(void)
{
    return GCe_tot_time;
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

void
initStats(void)
{
    nat i;
  
    if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
	statsPrintf("    Alloc    Collect    Live    GC    GC     TOT     TOT  Page Flts\n");
	statsPrintf("    bytes     bytes     bytes  user  elap    user    elap\n");
    }
    GC_coll_times = 
	(Ticks *)stgMallocBytes(
	    sizeof(Ticks)*RtsFlags.GcFlags.generations,
	    "initStats");
    for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
	GC_coll_times[i] = 0;
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
}

/* -----------------------------------------------------------------------------
   Called at the end of each GC
   -------------------------------------------------------------------------- */

void
stat_endGC (lnat alloc, lnat collect, lnat live, lnat copied, 
	    lnat scavd_copied, lnat gen)
{
    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
	Ticks time, etime, gc_time, gc_etime;
	
	getProcessTimes(&time, &etime);
	gc_time  = time - GC_start_time;
	gc_etime = etime - GCe_start_time;
	
	if (RtsFlags.GcFlags.giveStats == VERBOSE_GC_STATS) {
	    nat faults = getPageFaults();
	    
	    statsPrintf("%9ld %9ld %9ld",
		    alloc*sizeof(W_), collect*sizeof(W_), live*sizeof(W_));
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

	GC_tot_copied += (ullong) copied;
	GC_tot_scavd_copied += (ullong) scavd_copied;
	GC_tot_alloc  += (ullong) alloc;
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
    }

    if (rub_bell) {
	debugBelch("\b\b\b  \b\b\b");
	rub_bell = 0;
    }
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
	    ullong_format_string(GC_tot_alloc*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%11s bytes allocated in the heap\n", temp);

	    ullong_format_string(GC_tot_copied*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%11s bytes copied during GC (scavenged)\n", temp);

	    ullong_format_string(GC_tot_scavd_copied*sizeof(W_), 
				 temp, rtsTrue/*commas*/);
	    statsPrintf("%11s bytes copied during GC (not scavenged)\n", temp);
  
	    if ( ResidencySamples > 0 ) {
		ullong_format_string(MaxResidency*sizeof(W_), 
				     temp, rtsTrue/*commas*/);
		statsPrintf("%11s bytes maximum residency (%ld sample(s))\n",
			temp, ResidencySamples);
	    }
	    statsPrintf("\n");

	    /* Print garbage collections in each gen */
	    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
		statsPrintf("%11d collections in generation %d (%6.2fs)\n", 
			generations[g].collections, g, 
			TICK_TO_DBL(GC_coll_times[g]));
	    }

	    statsPrintf("\n%11ld Mb total memory in use\n\n", 
		    mblocks_allocated * MBLOCK_SIZE / (1024 * 1024));

#if defined(THREADED_RTS)
	    {
		nat i;
		Task *task;
		for (i = 0, task = all_tasks; 
		     task != NULL; 
		     i++, task = task->all_link) {
		    statsPrintf("  Task %2d %-8s :  MUT time: %6.2fs  (%6.2fs elapsed)\n"
			    "                      GC  time: %6.2fs  (%6.2fs elapsed)\n\n", 
				i,
				(task->tso == NULL) ? "(worker)" : "(bound)",
				TICK_TO_DBL(task->mut_time),
				TICK_TO_DBL(task->mut_etime),
				TICK_TO_DBL(task->gc_time),
				TICK_TO_DBL(task->gc_etime));
		}
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
		ullong_format_string(0, temp, rtsTrue/*commas*/);
	    else
		ullong_format_string(
		    (ullong)((GC_tot_alloc*sizeof(W_))/
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
	}

	if (RtsFlags.GcFlags.giveStats == ONELINE_GC_STATS) {
	  /* print the long long separately to avoid bugginess on mingwin (2001-07-02, mingw-0.5) */
	  statsPrintf("<<ghc: %llu bytes, ", GC_tot_alloc*(ullong)sizeof(W_));
	  statsPrintf("%d GCs, %ld/%ld avg/max bytes residency (%ld samples), %luM in use, %.2f INIT (%.2f elapsed), %.2f MUT (%.2f elapsed), %.2f GC (%.2f elapsed) :ghc>>\n",
		    total_collections,
		    ResidencySamples == 0 ? 0 : 
		        AvgResidency*sizeof(W_)/ResidencySamples, 
		    MaxResidency*sizeof(W_), 
		    ResidencySamples,
		    (unsigned long)(mblocks_allocated * MBLOCK_SIZE / (1024L * 1024L)),
		    TICK_TO_DBL(InitUserTime), TICK_TO_DBL(InitElapsedTime),
		    TICK_TO_DBL(MutUserTime), TICK_TO_DBL(MutElapsedTime),
		    TICK_TO_DBL(GC_tot_time), TICK_TO_DBL(GCe_tot_time));
	}

	statsFlush();
	statsClose();
    }
}

/* -----------------------------------------------------------------------------
   stat_describe_gens

   Produce some detailed info on the state of the generational GC.
   -------------------------------------------------------------------------- */
#ifdef DEBUG
void
statDescribeGens(void)
{
  nat g, s, mut, lge, live;
  bdescr *bd;
  step *step;

  debugBelch(
"     Gen    Steps      Max   Mutable  Step   Blocks     Live    Large\n"
"                     Blocks Closures                          Objects\n");

  mut = 0;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (bd = generations[g].mut_list; bd != NULL; bd = bd->link) {
	  mut += bd->free - bd->start;
      }

    debugBelch("%8d %8d %8d %9d", g, generations[g].n_steps,
	    generations[g].max_blocks, mut);

    for (s = 0; s < generations[g].n_steps; s++) {
      step = &generations[g].steps[s];
      for (bd = step->large_objects, lge = 0; bd; bd = bd->link)
	lge++;
      live = 0;
      bd = step->blocks;
      for (; bd; bd = bd->link) {
	live += (bd->free - bd->start) * sizeof(W_);
      }
      if (s != 0) {
	debugBelch("%36s","");
      }
      debugBelch("%6d %8d %8d %8d\n", s, step->n_blocks,
	      live, lge);
    }
  }
  debugBelch("\n");
}
#endif

/* -----------------------------------------------------------------------------
   Stats available via a programmatic interface, so eg. GHCi can time
   each compilation and expression evaluation.
   -------------------------------------------------------------------------- */

extern HsInt64 getAllocations( void ) 
{ return (HsInt64)total_allocated * sizeof(W_); }

/* -----------------------------------------------------------------------------
   Dumping stuff in the stats file, or via the debug message interface
   -------------------------------------------------------------------------- */

static void
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
