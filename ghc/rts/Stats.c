/* -----------------------------------------------------------------------------
 * $Id: Stats.c,v 1.6 1999/02/05 16:02:56 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

#define NON_POSIX_SOURCE

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "StoragePriv.h"
#include "MBlock.h"

/**
 *  Ian: For the moment we just want to ignore
 * these on Nemesis
 **/
#ifdef _NEMESIS_OS_
#ifdef HAVE_SYS_TIMES_H
#undef HAVE_SYS_TIMES_H /* <sys/times.h> */
#endif
#ifdef HAVE_SYS_RESOURCE_H /* <sys/resource.h> */
#undef HAVE_SYS_RESOURCE_H
#endif
#ifdef HAVE_SYS_TIME_H  /* <sys/time.h> */
#undef HAVE_SYS_TIME_H
#endif
#ifdef HAVE_SYS_TIMEB_H 
#undef HAVE_SYS_TIMEB_H /* <sys/timeb.h> */
#endif
#ifdef HAVE_UNISTD_H
#undef HAVE_UNISTD_H    /* <unistd.h> */
#endif
#ifdef HAVE_TIMES
#undef HAVE_TIMES
#endif 
#ifdef HAVE_FTIME
#undef HAVE_FTIME
#endif
#ifdef HAVE_GETRUSAGE
#undef HAVE_GETRUSAGE
#endif
#ifdef HAVE_SYSCONF
#undef HAVE_SYSCONF
#endif
#endif /* _NEMESIS_OS_ */

#include "Stats.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if defined(HAVE_SYS_RESOURCE_H) && ! irix_TARGET_OS
#include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* huh? */
#define BIG_STRING_LEN              512

static StgDouble ElapsedTimeStart = 0.0;
static StgDouble TicksPerSecond   = 0.0;

static StgDouble InitUserTime = 0.0;
static StgDouble InitElapsedTime = 0.0;

static ullong GC_tot_alloc = 0;

static StgDouble GC_start_time,  GC_tot_time = 0;  /* User GC Time */
static StgDouble GCe_start_time, GCe_tot_time = 0; /* Elapsed GC time */

lnat MaxResidency = 0;     /* in words; for stats only */
lnat ResidencySamples = 0; /* for stats only */

static lnat GC_start_faults = 0, GC_end_faults = 0;

/* ToDo: convert this to use integers? --SDM */

/* elapsedtime() -- The current elapsed time in seconds */

StgDouble
elapsedtime(void)
{
#if ! (defined(HAVE_TIMES) || defined(HAVE_FTIME))
    /* We will #ifdef around the fprintf for machines
       we *know* are unsupported. (WDP 94/05)
    */
    fprintf(stderr, "NOTE: `elapsedtime' does nothing!\n");
    return 0.0;

#else /* not stumped */

/* "ftime" may be nicer, but "times" is more standard;
   but, on a Sun, if you do not get the SysV one, you are *hosed*...
 */

# if defined(HAVE_TIMES) && ! sunos4_TARGET_OS
    struct tms t;
    clock_t r = times(&t);

    return (((StgDouble)r)/TicksPerSecond - ElapsedTimeStart);

# else /* HAVE_FTIME */
    struct timeb t;

    ftime(&t);
    return (fabs(t.time + 1e-3*t.millitm - ElapsedTimeStart));

# endif /* HAVE_FTIME */
#endif /* not stumped */
}

static nat
pagefaults(void)
{
# if !defined(HAVE_GETRUSAGE) || irix_TARGET_OS
    return 0;
# else
    struct rusage t;

    getrusage(RUSAGE_SELF, &t);
    return(t.ru_majflt);
# endif
}

/* ToDo: use gettimeofday on systems that support it (u-sec accuracy) */

void
start_time(void)
{
    long ticks;
    /* Determine TicksPerSecond ... */
#ifdef HAVE_SYSCONF
    ticks = sysconf(_SC_CLK_TCK);
    if ( ticks == -1 ) {
	fprintf(stderr, "stat_init: bad call to 'sysconf'!\n");
    	stg_exit(EXIT_FAILURE);
    }
    TicksPerSecond = (StgDouble) ticks;

#else /* no "sysconf"; had better guess */
# ifdef HZ
    TicksPerSecond = (StgDouble) (HZ);

# else /* had better guess wildly */
    /* We will #ifdef around the fprintf for machines
       we *know* are unsupported. (WDP 94/05)
    */
    fprintf(stderr, "NOTE: Guessing `TicksPerSecond = 60'!\n");
    TicksPerSecond = 60.0;
# endif
#endif

    ElapsedTimeStart = elapsedtime();
}


void
initStats(void)
{
  FILE *sf = RtsFlags.GcFlags.statsFile;
  
  if (RtsFlags.GcFlags.giveStats) {
    fprintf(sf, "    Alloc    Collect    Live    GC    GC     TOT     TOT  Page Flts\n");
    fprintf(sf, "    bytes     bytes     bytes  user  elap    user    elap\n");
  }
}    


StgDouble
usertime(void)
{
#if ! (defined(HAVE_GETRUSAGE) || defined(HAVE_TIMES))
    /* We will #ifdef around the fprintf for machines
       we *know* are unsupported. (WDP 94/05)
    */
    fprintf(stderr, "NOTE: `usertime' does nothing!\n");
    return 0.0;

#else /* not stumped */

# if defined(HAVE_TIMES) 
    struct tms t;

    times(&t);
    return(((StgDouble)(t.tms_utime))/TicksPerSecond);

#else /* HAVE_GETRUSAGE */
    struct rusage t;

    getrusage(RUSAGE_SELF, &t);
    return(t.ru_utime.tv_sec + 1e-6*t.ru_utime.tv_usec);

# endif /* HAVE_GETRUSAGE */
#endif /* not stumped */
}

void 
end_init(void)
{
  InitUserTime = usertime();
  InitElapsedTime = elapsedtime();
  if (InitElapsedTime < 0.0) {
    InitElapsedTime = 0.0;
  }
}

/* -----------------------------------------------------------------------------
   Called at the beginning of each GC
   -------------------------------------------------------------------------- */

static nat rub_bell = 0;

void
stat_startGC(void)
{
    FILE *sf = RtsFlags.GcFlags.statsFile;

    nat bell = RtsFlags.GcFlags.ringBell;

    if (bell) {
	if (bell > 1) {
	    fprintf(stderr, " GC ");
	    rub_bell = 1;
	} else {
	    fprintf(stderr, "\007");
	}
    }

    if (sf != NULL) {
	GC_start_time = usertime();
	GCe_start_time = elapsedtime();
	if (RtsFlags.GcFlags.giveStats) {
	  GC_start_faults = pagefaults();
	}
    }
}

/* -----------------------------------------------------------------------------
   Called at the end of each GC
   -------------------------------------------------------------------------- */

void
stat_endGC(lnat alloc, lnat collect, lnat live, lnat gen)
{
    FILE *sf = RtsFlags.GcFlags.statsFile;

    if (sf != NULL) {
	StgDouble time = usertime();
	StgDouble etime = elapsedtime();

	if (RtsFlags.GcFlags.giveStats) {
	    nat faults = pagefaults();

	    fprintf(sf, "%9ld %9ld %9ld",
		    alloc*sizeof(W_), collect*sizeof(W_), live*sizeof(W_));
	    fprintf(sf, " %5.2f %5.2f %7.2f %7.2f %4ld %4ld  (Gen: %2ld)\n", 
		    (time-GC_start_time), 
		    (etime-GCe_start_time), 
		    time,
		    etime,
		    faults - GC_start_faults,
		    GC_start_faults - GC_end_faults,
		    gen);

	    GC_end_faults = faults;
	    fflush(sf);
	}

	GC_tot_alloc += (ullong) alloc;
	GC_tot_time  += time-GC_start_time;
	GCe_tot_time += etime-GCe_start_time;

	if (gen == RtsFlags.GcFlags.generations-1) { /* major GC? */
	  if (live > MaxResidency) {
	    MaxResidency = live;
	  }
	  ResidencySamples++;
	}
    }

    if (rub_bell) {
	fprintf(stderr, "\b\b\b  \b\b\b");
	rub_bell = 0;
    }
}

/* -----------------------------------------------------------------------------
   Called at the end of execution

   NOTE: number of allocations is not entirely accurate: it doesn't
   take into account the few bytes at the end of the heap that
   were left unused when the heap-check failed.
   -------------------------------------------------------------------------- */

void
stat_exit(int alloc)
{
    FILE *sf = RtsFlags.GcFlags.statsFile;

    if (sf != NULL){
	char temp[BIG_STRING_LEN];
	StgDouble time = usertime();
	StgDouble etime = elapsedtime();
	StgDouble MutTime, MutElapsedTime;

	/* avoid divide by zero if time is measured as 0.00 seconds -- SDM */
	if (time  == 0.0)  time = 0.0001;
	if (etime == 0.0) etime = 0.0001;
	

	fprintf(sf, "%9ld %9.9s %9.9s",
		(lnat)alloc*sizeof(W_), "", "");
	fprintf(sf, " %5.2f %5.2f\n\n", 0.0, 0.0);

	GC_tot_alloc += alloc;

	ullong_format_string(GC_tot_alloc*sizeof(W_), temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s bytes allocated in the heap\n", temp);

	if ( ResidencySamples > 0 ) {
	    ullong_format_string(MaxResidency*sizeof(W_), temp, rtsTrue/*commas*/);
	    fprintf(sf, "%11s bytes maximum residency (%ld sample(s))\n",
			      temp,
			      ResidencySamples);
	}
	fprintf(sf,"\n");

	{ /* Count garbage collections */
	  nat g;
	  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    fprintf(sf, "%11d collections in generation %d\n", 
		    generations[g].collections, g);
	  }
	}
	fprintf(sf,"\n%11ld Mb total memory in use\n\n", 
		mblocks_allocated * MBLOCK_SIZE / (1024 * 1024));

	MutTime = time - GC_tot_time - InitUserTime;
	if (MutTime < 0) { MutTime = 0; }
	MutElapsedTime = etime - GCe_tot_time - InitElapsedTime;
	if (MutElapsedTime < 0) { MutElapsedTime = 0; }	/* sometimes -0.00 */

	fprintf(sf, "  INIT  time  %6.2fs  (%6.2fs elapsed)\n",
		InitUserTime, InitElapsedTime);
	fprintf(sf, "  MUT   time  %6.2fs  (%6.2fs elapsed)\n",
		MutTime, MutElapsedTime);
	fprintf(sf, "  GC    time  %6.2fs  (%6.2fs elapsed)\n",
		GC_tot_time, GCe_tot_time);
	fprintf(sf, "  Total time  %6.2fs  (%6.2fs elapsed)\n\n",
		time, etime);

	fprintf(sf, "  %%GC time     %5.1f%%  (%.1f%% elapsed)\n\n",
		GC_tot_time*100./time, GCe_tot_time*100./etime);

	if (time - GC_tot_time == 0.0)
		ullong_format_string(0, temp, rtsTrue/*commas*/);
	else
		ullong_format_string((ullong)(GC_tot_alloc*sizeof(W_)/
					      (time - GC_tot_time)),
				     temp, rtsTrue/*commas*/);

	fprintf(sf, "  Alloc rate    %s bytes per MUT second\n\n", temp);

	fprintf(sf, "  Productivity %5.1f%% of total user, %.1f%% of total elapsed\n\n",
		(time - GC_tot_time - InitUserTime) * 100. / time, 
                (time - GC_tot_time - InitUserTime) * 100. / etime);
	fflush(sf);
	fclose(sf);
    }
}

/* -----------------------------------------------------------------------------
   stat_describe_gens

   Produce some detailed info on the state of the generational GC.
   -------------------------------------------------------------------------- */
void
stat_describe_gens(void)
{
  nat g, s, mut, mut_once, lge, live;
  StgMutClosure *m;
  bdescr *bd;
  step *step;

  fprintf(stderr, "     Gen    Steps      Max   Mutable  Mut-Once  Step  Blocks     Live    Large\n                    Blocks  Closures  Closures                         Objects\n");

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (m = generations[g].mut_list, mut = 0; m != END_MUT_LIST; 
	 m = m->mut_link) 
      mut++;
    for (m = generations[g].mut_once_list, mut_once = 0; m != END_MUT_LIST; 
	 m = m->mut_link) 
      mut_once++;
    fprintf(stderr, "%8d %8d %8d %9d %9d", g, generations[g].n_steps,
	    generations[g].max_blocks, mut, mut_once);

    for (s = 0; s < generations[g].n_steps; s++) {
      step = &generations[g].steps[s];
      for (bd = step->large_objects, lge = 0; bd; bd = bd->link)
	lge++;
      live = 0;
      for (bd = step->blocks; bd; bd = bd->link) {
	live += (bd->free - bd->start) * sizeof(W_);
      }
      if (s != 0) {
	fprintf(stderr,"%46s","");
      }
      fprintf(stderr,"%6d %8d %8d %8d\n", s, step->n_blocks,
	      live, lge);
    }
  }
  fprintf(stderr,"\n");
}
