/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

/* Alas, no.  This source is non-posix.
   #include "PosixSource.h" 
*/

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "MBlock.h"
#include "Schedule.h"
#include "Stats.h"
#include "ParTicky.h"                       /* ToDo: move into Rts.h */
#include "Profiling.h"
#include "Storage.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef mingw32_HOST_OS
# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
# endif
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef __CYGWIN32__
# ifdef HAVE_TIME_H
#  include <time.h>
# endif
#endif

#if ! irix_HOST_OS && ! defined(mingw32_HOST_OS)
# if defined(HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
# endif
#endif

#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if HAVE_WINDOWS_H
#include <windows.h>
#endif

#if defined(PAR) || !(!defined(HAVE_GETRUSAGE) || irix_HOST_OS || defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS))
#include <sys/resource.h>
#endif

/* huh? */
#define BIG_STRING_LEN              512

/* We're not trying to be terribly accurate here, using the 
 * basic times() function to get a resolution of about 100ths of a 
 * second, depending on the OS.  A long int will do fine for holding
 * these values.
 */
#define TICK_TYPE long int
#define TICK_TO_DBL(t) ((double)(t) / TicksPerSecond)

static int TicksPerSecond = 0;

static TICK_TYPE ElapsedTimeStart = 0;
static TICK_TYPE CurrentElapsedTime = 0;
static TICK_TYPE CurrentUserTime    = 0;

static TICK_TYPE InitUserTime     = 0;
static TICK_TYPE InitElapsedTime  = 0;
static TICK_TYPE InitElapsedStamp = 0;

static TICK_TYPE MutUserTime      = 0;
static TICK_TYPE MutElapsedTime   = 0;
static TICK_TYPE MutElapsedStamp  = 0;

static TICK_TYPE ExitUserTime     = 0;
static TICK_TYPE ExitElapsedTime  = 0;

static ullong GC_tot_alloc        = 0;
static ullong GC_tot_copied       = 0;

static TICK_TYPE GC_start_time = 0,  GC_tot_time  = 0;  /* User GC Time */
static TICK_TYPE GCe_start_time = 0, GCe_tot_time = 0;  /* Elapsed GC time */

#ifdef PROFILING
static TICK_TYPE RP_start_time  = 0, RP_tot_time  = 0;  /* retainer prof user time */
static TICK_TYPE RPe_start_time = 0, RPe_tot_time = 0;  /* retainer prof elap time */

static TICK_TYPE HC_start_time, HC_tot_time = 0;     // heap census prof user time
static TICK_TYPE HCe_start_time, HCe_tot_time = 0;   // heap census prof elap time
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

static TICK_TYPE *GC_coll_times;

static void  getTimes(void);
static nat   pageFaults(void);

static void statsPrintf( char *s, ... ) 
    GNUC3_ATTRIBUTE(format (printf, 1, 2));

static void statsFlush( void );
static void statsClose( void );

/* elapsedtime() -- The current elapsed time in seconds */

#if defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS)
#define HNS_PER_SEC 10000000LL /* FILETIMES are in units of 100ns */
/* Convert FILETIMEs into secs */
#define FT2longlong(ll,ft)    \
    (ll)=(ft).dwHighDateTime; \
    (ll) <<= 32;              \
    (ll) |= (ft).dwLowDateTime; \
    (ll) /= (unsigned long long) (HNS_PER_SEC / CLOCKS_PER_SEC)
#endif

#if defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS)
/* cygwin32 or mingw32 version */
static void
getTimes(void)
{
    static int is_win9x = -1;

    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};
    long long int kT, uT;
    
    if (is_win9x < 0) {
      /* figure out whether we're on a Win9x box or not. */
      OSVERSIONINFO oi;
      BOOL b;

      /* Need to init the size field first.*/
      oi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
      b = GetVersionEx(&oi);
      
      is_win9x = ( (b && (oi.dwPlatformId & VER_PLATFORM_WIN32_WINDOWS)) ? 1 : 0);
    }
 
    if (is_win9x) {
      /* On Win9x, just attribute all running time to the user. */
      SYSTEMTIME st;

      GetSystemTime(&st);
      SystemTimeToFileTime(&st,&userTime);
    } else {
      /* ToDo: pin down elapsed times to just the OS thread(s) that
	 are evaluating/managing Haskell code.
      */
      if (!GetProcessTimes (GetCurrentProcess(), &creationTime,
		          &exitTime, &kernelTime, &userTime)) {
	/* Probably on a Win95 box..*/
	CurrentElapsedTime = 0;
	CurrentUserTime = 0;
	return;
      }
    }

    FT2longlong(kT,kernelTime);
    FT2longlong(uT,userTime);
    CurrentElapsedTime = uT + kT;
    CurrentUserTime = uT;

    if (is_win9x) {
      /* Adjust for the fact that we're using system time & not
	 process time on Win9x. */
      CurrentUserTime    -= ElapsedTimeStart;
      CurrentElapsedTime -= ElapsedTimeStart;
    }
}

#else /* !win32 */

static void
getTimes(void)
{

#ifndef HAVE_TIMES
    /* We will #ifdef around the fprintf for machines
       we *know* are unsupported. (WDP 94/05)
    */
    debugBelch("NOTE: `getTimes' does nothing!\n");
    return 0.0;

#else /* not stumped */
    struct tms t;
    clock_t r = times(&t);

    CurrentElapsedTime = r;
    CurrentUserTime = t.tms_utime;
#endif

}
#endif /* !win32 */

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
    getTimes();
    return TICK_TO_DBL(CurrentUserTime - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time));
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

static nat
pageFaults(void)
{
  /* ToDo (on NT): better, get this via the performance data
     that's stored in the registry. */
# if !defined(HAVE_GETRUSAGE) || irix_HOST_OS || defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS)
    return 0;
# else
    struct rusage t;

    getrusage(RUSAGE_SELF, &t);
    return(t.ru_majflt);
# endif
}

void
initStats(void)
{
    nat i;
  
    if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
	statsPrintf("    Alloc    Collect    Live    GC    GC     TOT     TOT  Page Flts\n");
	statsPrintf("    bytes     bytes     bytes  user  elap    user    elap\n");
    }
    GC_coll_times = 
	(TICK_TYPE *)stgMallocBytes(
	    sizeof(TICK_TYPE)*RtsFlags.GcFlags.generations,
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
    /* Determine TicksPerSecond ... */
#if defined(CLK_TCK)		/* defined by POSIX */
    TicksPerSecond = CLK_TCK;

#elif defined(HAVE_SYSCONF)
    long ticks;

    ticks = sysconf(_SC_CLK_TCK);
    if ( ticks == -1 ) {
	debugBelch("stat_init: bad call to 'sysconf'!\n");
    	stg_exit(EXIT_FAILURE);
    }
    TicksPerSecond = ticks;

/* no "sysconf" or CLK_TCK; had better guess */
#elif defined(HZ)
    TicksPerSecond = HZ;

#elif defined(CLOCKS_PER_SEC)
    TicksPerSecond = CLOCKS_PER_SEC;

#else /* had better guess wildly */
    /* We will #ifdef around the fprintf for machines
       we *know* are unsupported. (WDP 94/05)
    */
    debugBelch("NOTE: Guessing `TicksPerSecond = 60'!\n");
    TicksPerSecond = 60;
#endif

    getTimes();
    ElapsedTimeStart = CurrentElapsedTime;
}

void 
stat_endInit(void)
{
    getTimes();
    InitUserTime = CurrentUserTime;
    InitElapsedStamp = CurrentElapsedTime; 
    if (ElapsedTimeStart > CurrentElapsedTime) {
	InitElapsedTime = 0;
    } else {
	InitElapsedTime = CurrentElapsedTime - ElapsedTimeStart;
    }
}

/* -----------------------------------------------------------------------------
   stat_startExit and stat_endExit
   
   These two measure the time taken in shutdownHaskell().
   -------------------------------------------------------------------------- */

void
stat_startExit(void)
{
    getTimes();
    MutElapsedStamp = CurrentElapsedTime;
    MutElapsedTime = CurrentElapsedTime - GCe_tot_time -
	PROF_VAL(RPe_tot_time + HCe_tot_time) - InitElapsedStamp;
    if (MutElapsedTime < 0) { MutElapsedTime = 0; }	/* sometimes -0.00 */

    /* for SMP, we don't know the mutator time yet, we have to inspect
     * all the running threads to find out, and they haven't stopped
     * yet.  So we just timestamp MutUserTime at this point so we can
     * calculate the EXIT time.  The real MutUserTime is calculated
     * in stat_exit below.
     */
#ifdef SMP
    MutUserTime = CurrentUserTime;
#else
    MutUserTime = CurrentUserTime - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time) - InitUserTime;
    if (MutUserTime < 0) { MutUserTime = 0; }
#endif
}

void
stat_endExit(void)
{
    getTimes();
#ifdef SMP
    ExitUserTime = CurrentUserTime - MutUserTime;
#else
    ExitUserTime = CurrentUserTime - MutUserTime - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time) - InitUserTime;
#endif
    ExitElapsedTime = CurrentElapsedTime - MutElapsedStamp;
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
    getTimes();
    GC_start_time = CurrentUserTime;  /* needed in mut_user_time_during_GC() */
#endif

    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
#if !defined(PROFILING) && !defined(DEBUG)
	getTimes();
        GC_start_time = CurrentUserTime;
#endif
	GCe_start_time = CurrentElapsedTime;
	if (RtsFlags.GcFlags.giveStats) {
	    GC_start_faults = pageFaults();
	}
    }
}

/* -----------------------------------------------------------------------------
   Called at the end of each GC
   -------------------------------------------------------------------------- */

void
stat_endGC(lnat alloc, lnat collect, lnat live, lnat copied, lnat gen)
{
    if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
	TICK_TYPE time, etime, gc_time, gc_etime;
	
	getTimes();
	time     = CurrentUserTime;
	etime    = CurrentElapsedTime;
	gc_time  = time - GC_start_time;
	gc_etime = etime - GCe_start_time;
	
	if (RtsFlags.GcFlags.giveStats == VERBOSE_GC_STATS) {
	    nat faults = pageFaults();
	    
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
	GC_tot_alloc  += (ullong) alloc;
	GC_tot_time   += gc_time;
	GCe_tot_time  += gc_etime;
	
#ifdef SMP
	{
	    nat i;
	    pthread_t me = pthread_self();

	    for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
		if (me == task_ids[i].id) {
		    task_ids[i].gc_time += gc_time;
		    task_ids[i].gc_etime += gc_etime;
		    break;
		}
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
  getTimes();
  RP_start_time = CurrentUserTime;
  RPe_start_time = CurrentElapsedTime;
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
  getTimes();
  RP_tot_time += CurrentUserTime - RP_start_time;
  RPe_tot_time += CurrentElapsedTime - RPe_start_time;

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
  getTimes();
  HC_start_time = CurrentUserTime;
  HCe_start_time = CurrentElapsedTime;
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Called at the end of each heap census
   -------------------------------------------------------------------------- */
#ifdef PROFILING
void
stat_endHeapCensus(void) 
{
  getTimes();
  HC_tot_time += CurrentUserTime - HC_start_time;
  HCe_tot_time += CurrentElapsedTime - HCe_start_time;
}
#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   stat_workerStop

   Called under SMP when a worker thread finishes.  We drop the timing
   stats for this thread into the task_ids struct for that thread.
   -------------------------------------------------------------------------- */

#if defined(SMP)
void
stat_workerStop(void)
{
    nat i;
    pthread_t me = pthread_self();

    getTimes();

    for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
	if (task_ids[i].id == me) {
	    task_ids[i].mut_time = CurrentUserTime - task_ids[i].gc_time;
	    task_ids[i].mut_etime = CurrentElapsedTime
		- GCe_tot_time
		- task_ids[i].elapsedtimestart;
	    if (task_ids[i].mut_time < 0.0)  { task_ids[i].mut_time = 0.0;  }
	    if (task_ids[i].mut_etime < 0.0) { task_ids[i].mut_etime = 0.0; }
	}
    }
}
#endif

#if defined(SMP)
long int stat_getElapsedTime ()
{
  getTimes();
  return CurrentElapsedTime;
}
#endif

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
	TICK_TYPE time;
	TICK_TYPE etime;
	nat g, total_collections = 0;

	getTimes();
	time = CurrentUserTime;
	etime = CurrentElapsedTime - ElapsedTimeStart;

	GC_tot_alloc += alloc;

	/* avoid divide by zero if time is measured as 0.00 seconds -- SDM */
	if (time  == 0.0)  time = 1;
	if (etime == 0.0) etime = 1;
	
	/* Count total garbage collections */
	for (g = 0; g < RtsFlags.GcFlags.generations; g++)
	    total_collections += generations[g].collections;

	/* For SMP, we have to get the user time from each thread
	 * and try to work out the total time.
	 */
#ifdef SMP
	{   nat i;
	    MutUserTime = 0.0;
	    for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
		MutUserTime += task_ids[i].mut_time;
	    }
	}
	time = MutUserTime + GC_tot_time + InitUserTime + ExitUserTime;
	if (MutUserTime < 0) { MutUserTime = 0; }
#endif

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
	    statsPrintf("%11s bytes copied during GC\n", temp);

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

#ifdef SMP
	    {
		nat i;
		for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
		    statsPrintf("  Task %2d:  MUT time: %6.2fs  (%6.2fs elapsed)\n"
			    "            GC  time: %6.2fs  (%6.2fs elapsed)\n\n", 
			    i, 
			    TICK_TO_DBL(task_ids[i].mut_time),
			    TICK_TO_DBL(task_ids[i].mut_etime),
			    TICK_TO_DBL(task_ids[i].gc_time),
			    TICK_TO_DBL(task_ids[i].gc_etime));
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

  debugBelch("     Gen    Steps      Max   Mutable  Step   Blocks     Live    Large\n                    Blocks  Closures  Closures                          Objects\n");

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
      if (RtsFlags.GcFlags.generations == 1) {
	bd = step->to_blocks;
      } else {
	bd = step->blocks;
      }
      for (; bd; bd = bd->link) {
	live += (bd->free - bd->start) * sizeof(W_);
      }
      if (s != 0) {
	debugBelch("%46s","");
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
