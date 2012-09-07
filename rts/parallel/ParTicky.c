/* -------------------------------------------------------------------------
 *
 * (c) Hans-Wolfgang Loidl, 2000-
 *
 * Parallel ticky profiling, monitoring basic RTS operations in GUM.
 * Similar in structure to TICKY_TICKY profiling, but doesn't need a 
 * separate way of building GHC.
 *-------------------------------------------------------------------------- */

#if defined(PAR) && defined(PAR_TICKY)

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
//#include "StoragePriv.h"
//#include "MBlock.h"
//#include "Schedule.h"
#include "GC.h"
#include "Stats.h"
#include "ParTicky.h"                       // ToDo: move into Rts.h
#include "ParallelRts.h"

#if defined(PAR) && defined(HAVE_GETRUSAGE)
#include <sys/resource.h>
#endif

/* external data */
extern double ElapsedTimeStart;

extern StgWord64 GC_tot_alloc;
extern StgWord64 GC_tot_copied;

extern W_ MaxResidency;     /* in words; for stats only */
extern W_ ResidencySamples; /* for stats only */

/* ngIplu' {Stats.c}vo' */
#define BIG_STRING_LEN              512

/* ngIplu' {Ticky.c}vo' */
#define INTAVG(a,b) ((b == 0) ? 0.0 : ((double) (a) / (double) (b)))
#define PC(a)	    (100.0 * a)

#define AVG(thing) \
	StgDouble avg##thing  = INTAVG(tot##thing,ctr##thing)


#if 0
void
set_foo_time(double *x) {
  *x = usertime();
}

double
get_foo_time(double x) {
  fprintf(stderr, "get_foo_time: %7.2f (%7.5f,%7.5f) \n", 
	  usertime()-x,usertime(),x);
  return (usertime()-x);
}
#endif

static double start_time_GA = 0.0;
static double start_mark = 0.0;
static double start_pack = 0.0;
static double start_unpack = 0.0;

void
par_ticky_Par_start (void) {
# if !defined(HAVE_GETRUSAGE) || irix_HOST_OS || defined(_WIN32)
    fprintf(stderr, "|| sorry don't have RUSAGE\n");
    return ;
# else
    FILE *sf = RtsFlags.GcFlags.statsFile;
    struct rusage t;
    double utime, stime;

    if (RtsFlags.GcFlags.giveStats>1 && sf != NULL) {
      getrusage(RUSAGE_SELF, &t);
      
      utime = t.ru_utime.tv_sec + 1e-6*t.ru_utime.tv_usec;
      stime = t.ru_stime.tv_sec + 1e-6*t.ru_stime.tv_usec;
      
      fprintf(stderr, "|| user time: %5.2f; system time: %5.2f\n",
	      utime, stime);
      fprintf(stderr, "|| max RSS: %ld; int SM size: %ld; int USM data size: %ld; int USS size: %ld\n",
	      t.ru_maxrss, t.ru_ixrss, t.ru_idrss, t.ru_isrss);
    }
#endif
}

#if 0	    
FYI:
            struct rusage
            {
                 struct timeval ru_utime; /* user time used */
                 struct timeval ru_stime; /* system time used */
                 long ru_maxrss;          /* maximum resident set size */
                 long ru_ixrss;      /* integral shared memory size */
                 long ru_idrss;      /* integral unshared data size */
                 long ru_isrss;      /* integral unshared stack size */
                 long ru_minflt;          /* page reclaims */
                 long ru_majflt;          /* page faults */
                 long ru_nswap;      /* swaps */
                 long ru_inblock;         /* block input operations */
                 long ru_oublock;         /* block output operations */
                 long ru_msgsnd;          /* messages sent */
                 long ru_msgrcv;          /* messages received */
                 long ru_nsignals;        /* signals received */
                 long ru_nvcsw;      /* voluntary context switches */
                 long ru_nivcsw;          /* involuntary context switches */
            };
#endif


void
par_ticky_rebuildGAtables_start(void) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    //set_foo_time(&start_time_GA);
    start_time_GA = usertime();
  }
}

void
par_ticky_rebuildGAtables_end(nat n, nat size_GA) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    static double foo = 0.0; 
    foo = usertime() - start_time_GA; // get_foo_time(start_time_GA);
    globalParStats.cnt_rebuild_GA++;
    globalParStats.tot_rebuild_GA += n;
    if ( n > globalParStats.res_rebuild_GA ) 
      globalParStats.res_rebuild_GA = n;
    // fprintf(stderr, "rebuildGAtables: footime=%7.2f (%11.5f, %11.5f)\n", 
    //    foo, usertime(), start_time_GA);
    globalParStats.time_rebuild_GA += foo;
    globalParStats.tot_size_GA += size_GA;
    if ( size_GA > globalParStats.res_size_GA ) 
      globalParStats.res_size_GA = size_GA;
  }
  // fprintf(stderr, ">> n: %d; size: %d;; tot: %d;  res: %d\n",
  //	  n, size_GA, globalParStats.tot_size_GA, globalParStats.res_size_GA);
}

void
par_ticky_markLocalGAs_start(void) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    start_time_GA = usertime();
  }
}

void
par_ticky_markLocalGAs_end(nat n) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.cnt_mark_GA++;
    globalParStats.tot_mark_GA += n;
    if ( n > globalParStats.res_mark_GA ) 
      globalParStats.res_mark_GA = n;
    globalParStats.time_mark_GA += usertime() - start_time_GA;
  }
}

void
par_ticky_markSparkQueue_start(void) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    start_mark=usertime();
  }
}

void
par_ticky_markSparkQueue_end(nat n) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.time_sparks += usertime() - start_mark;

    globalParStats.tot_sparks_marked += n;
    if ( n > globalParStats.res_sparks_marked ) 
      globalParStats.res_sparks_marked = n;
  }
}

void
par_ticky_PackNearbyGraph_start (void) {
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    start_pack=usertime();
  }
}

void
par_ticky_PackNearbyGraph_end(nat n, nat thunks) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.time_pack += usertime() - start_pack;

    globalParStats.tot_packets++;
    globalParStats.tot_packet_size += n;
    if ( n > globalParStats.res_packet_size ) 
      globalParStats.res_packet_size = n;
    globalParStats.tot_thunks += thunks;
    if ( thunks > globalParStats.res_thunks ) 
      globalParStats.res_thunks = thunks;
  }
}

void
par_ticky_UnpackGraph_start (void) {
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    start_unpack=usertime();
  }
}

void
par_ticky_UnpackGraph_end(nat n, nat thunks) {
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.time_unpack += usertime() - start_unpack;

    globalParStats.rec_packets++;
    globalParStats.rec_packet_size += n;
    /*
    if ( n > globalParStats.res_packet_size ) 
      globalParStats.res_packet_size = n;
    */
    globalParStats.rec_thunks += thunks;
    /*
    if ( thunks > globalParStats.res_thunks ) 
      globalParStats.res_thunks = thunks;
    */
  }
}

void
par_ticky_TP (void) {
    StgSparkPool *pool;
    nat tp_size, sp_size; // stats only

    // Global stats gathering
    /* the spark pool for the current PE */
    pool = &(MainRegTable.rSparks); // generalise to cap = &MainRegTable

    // Global statistics: residency of thread and spark pool
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      tp_size = run_queue_len() + 1; // add the TSO just poped
      // No: there may be many blocked threads being awoken at the same time
      // ASSERT(tp_size <= RtsFlags.ParFlags.maxThreads);
      globalParStats.tot_tp += tp_size;
      globalParStats.emp_tp += (tp_size==0) ? 1 : 0;
      globalParStats.cnt_tp++;
      if ( tp_size > globalParStats.res_tp)
	globalParStats.res_tp = tp_size;
      // fprintf(stderr, "run_queue_len() = %d (max %d)\n", run_queue_len(), globalParStats.res_tp);
      sp_size = spark_queue_len(pool);
      //ASSERT(sp_size <= RtsFlags.ParFlags.maxLocalSparks);
      globalParStats.tot_sp += sp_size;
      globalParStats.emp_sp += (sp_size==0) ? 1 : 0;
      globalParStats.cnt_sp++;
      if ( sp_size > globalParStats.res_sp)
	globalParStats.res_sp = sp_size;
      // fprintf(stderr, "spark_queue_len(pool) = %d (max %d)\n", spark_queue_len(pool), globalParStats.res_sp);
    }
}

void
globalParStat_exit(void)
{
    FILE *sf = RtsFlags.GcFlags.statsFile;
    double time, etime;

    /* print only if GC stats is enabled, too; i.e. -sstderr */
    if (!(RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS)) 
      return;

    time = usertime();
    etime = elapsedtime() - ElapsedTimeStart;
    // fprintf(stderr, "foo=%7.2f\n", time);

    if (sf != NULL){
        char temp[BIG_STRING_LEN];

	// GC_tot_alloc += alloc;
	fprintf(sf,"\n");

	fprintf(sf, "%11d threads created\n", 
		globalParStats.tot_threads_created);
	/*
	  Would need to add a ++ to the par macro to use this

	fprintf(sf, "%11d sparks created\n", 
		globalParStats.tot_sparks_created);
	fprintf(sf, "%11d sparks ignored\n", 
		globalParStats.tot_sparks_ignored);
	*/
	showStgWord64(globalParStats.res_tp, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s thread pool residency", temp);
	fprintf(sf, " (avg: %3.2f; %d times (%2.2f%%) of %d empty)\n", 
		(double)globalParStats.tot_tp/(double)globalParStats.cnt_tp,
		globalParStats.emp_tp, 
		globalParStats.emp_tp*100.0/(double)globalParStats.cnt_tp,
		globalParStats.cnt_tp);
	showStgWord64(globalParStats.res_sp, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s spark pool residency", temp);

	fprintf(sf, " (avg: %3.2f; %d times (%2.2f%%) of %d empty)\n", 
		(double)globalParStats.tot_sp/(double)globalParStats.cnt_sp,
		globalParStats.emp_sp, 
		globalParStats.emp_sp*100.0/(double)globalParStats.cnt_sp,
		globalParStats.cnt_sp);
	//showStgWord64(globalParStats.tot_fishes, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11d messages sent (%d fish, %d fetch, %d resume, %d schedule", 
		globalParStats.tot_fish_mess+globalParStats.tot_fetch_mess+
		globalParStats.tot_resume_mess+globalParStats.tot_schedule_mess,
		globalParStats.tot_fish_mess, globalParStats.tot_fetch_mess, 
		globalParStats.tot_resume_mess, globalParStats.tot_schedule_mess);
#if defined(DIST)
	fprintf(sf, "%d revals", globalParStats.tot_reval_mess);
#endif
	fprintf(sf,")\n");
	fprintf(sf, "%11d messages received (%d fish, %d fetch, %d resume, %d schedule", 
		globalParStats.rec_fish_mess+globalParStats.rec_fetch_mess+
		globalParStats.rec_resume_mess+globalParStats.rec_schedule_mess,
		globalParStats.rec_fish_mess, globalParStats.rec_fetch_mess, 
		globalParStats.rec_resume_mess, globalParStats.rec_schedule_mess);
#if defined(DIST)
	fprintf(sf, "%d revals", globalParStats.rec_reval_mess);
#endif
	fprintf(sf,")\n\n");

	showStgWord64(globalParStats.tot_size_GA*sizeof(W_), temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s bytes of global heap in total ", temp);
	fprintf(sf, "(%5.2f%% of total allocated heap)\n", 
		globalParStats.tot_size_GA*sizeof(W_)*100.0/(double)GC_tot_alloc*sizeof(W_));
	showStgWord64(globalParStats.res_size_GA*sizeof(W_), temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s bytes global heap residency ", temp);
	fprintf(sf, "(%5.2f%% of max heap residency)\n", 
		globalParStats.res_size_GA*sizeof(W_)*100.0/(double)MaxResidency*sizeof(W_));

	//showStgWord64(globalParStats.res_mark_GA, temp, rtsTrue/*commas*/);
	//fprintf(sf, "%11s GAs residency in GALA table ", temp);
	// showStgWord64(globalParStats.tot_mark_GA, temp, rtsTrue/*commas*/);
	//fprintf(sf, "(avg %5.2f; %d samples)\n", 
	//	(double)globalParStats.tot_mark_GA/(double)globalParStats.cnt_mark_GA,
	//	globalParStats.cnt_mark_GA);

	showStgWord64(globalParStats.local_alloc_GA, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s GAs locally allocated (calls to makeGlobal)\n", temp);

	showStgWord64(globalParStats.tot_rebuild_GA, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s live GAs in total (after rebuilding tables)\n", temp);
	showStgWord64(globalParStats.res_rebuild_GA, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s GAs residency (after rebuilding tables) ", temp);
	fprintf(sf, "(avg %5.2f; %d samples)\n", 
		(double)globalParStats.tot_rebuild_GA/(double)globalParStats.cnt_rebuild_GA,
		globalParStats.cnt_rebuild_GA);
	showStgWord64(globalParStats.res_free_GA, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s residency of freeing GAs", temp);
	fprintf(sf, " (avg %5.2f; %d samples)\n", 
		(double)globalParStats.tot_free_GA/(double)globalParStats.cnt_free_GA,
		globalParStats.cnt_free_GA);

	fprintf(sf, "%11.2fs spent marking GAs (%7.2f%% of %7.2fs)\n", 
		globalParStats.time_mark_GA,
		globalParStats.time_mark_GA*100./time, time);
	fprintf(sf, "%11.2fs spent rebuilding GALA tables (%7.2f%% of %7.2fs; %7.2f%% of %7.2fs)\n", 
		globalParStats.time_rebuild_GA,
		globalParStats.time_rebuild_GA*100./time, time,
		globalParStats.time_rebuild_GA*100./etime, etime);

	showStgWord64(globalParStats.tot_sparks_marked, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s sparks marked\t", temp);
	showStgWord64(globalParStats.res_sparks_marked, temp, rtsTrue/*commas*/);
	fprintf(sf, "%6s spark mark residency\n", temp);
	fprintf(sf, "%11.2fs spent marking sparks (%7.2f%% of %7.2fs; %7.2f%% of %7.2fs elapsed)\n", 
		globalParStats.time_sparks,
		globalParStats.time_sparks*100./time, time,
		globalParStats.time_sparks*100./etime, etime);

	fprintf(sf,"\n");

	showStgWord64(globalParStats.tot_packets, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s packets sent\n", temp);
	showStgWord64(globalParStats.tot_packet_size, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s bytes of graph sent in total (max %d; avg %.2f)\n",
		temp, globalParStats.res_packet_size,
		(double)globalParStats.tot_packet_size/(double)globalParStats.tot_packets);
	showStgWord64(globalParStats.tot_thunks, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s thunks sent in total (max %d; avg %.2f)\n",
		temp, globalParStats.res_thunks,
		(double)globalParStats.tot_thunks/(double)globalParStats.tot_packets);
	fprintf(sf, "%11.2fs spent packing graph structures (%7.2f%% of %7.2fs; %7.2f%% of %7.2fs elapsed)\n", 
		globalParStats.time_pack,
		globalParStats.time_pack*100./time, time,
		globalParStats.time_pack*100./etime, etime);

	showStgWord64(globalParStats.rec_packets, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s packets received\n", temp);
	showStgWord64(globalParStats.rec_packet_size, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s bytes of graph received in total (max %d; avg %.2f)\n",
		temp, globalParStats.rec_res_packet_size,
		(double)globalParStats.rec_packet_size/(double)globalParStats.rec_packets);
	showStgWord64(globalParStats.rec_thunks, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s thunks received in total (max %d; avg %.2f)\n",
		temp, globalParStats.rec_res_thunks,
		(double)globalParStats.rec_thunks/(double)globalParStats.rec_packets);
	fprintf(sf, "%11.2fs spent unpacking graph structures (%7.2f%% of %7.2fs; %7.2f%% of %7.2fs elapsed)\n", 
		globalParStats.time_unpack,
		globalParStats.time_unpack*100./time, time,
		globalParStats.time_unpack*100./etime, etime);

	fprintf(sf,"\n");

	showStgWord64(globalParStats.tot_arrs, temp, rtsTrue/*commas*/);
	fprintf(sf, "%11s bytearrays sent; ", temp);
	showStgWord64(globalParStats.tot_arr_size, temp, rtsTrue/*commas*/);
	fprintf(sf, " %s bytes in total (avg %.2f)\n",
		temp, 
		(double)globalParStats.tot_arr_size/(double)globalParStats.tot_arrs);
	
	fprintf(sf,"\n");

	fprintf(sf, "%11d yields, %d stack overflows, %d heap overflows\n",
		globalParStats.tot_yields, globalParStats.tot_stackover,
 		globalParStats.tot_heapover); 

	fprintf(sf,"\n");

	//fprintf(stderr, "Printing this pathetic statistics took %7.2fs (start @ %7.2f)\n",
	//	usertime()-time, time);

	fflush(sf);
	// Open filehandle needed by other stats printing fcts
	// fclose(sf);
    }
}

#endif

