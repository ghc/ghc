/* --------------------------------------------------------------------------
   Time-stamp: <Sat Dec 04 1999 18:26:22 Stardate: [-30]3998.84 hwloidl>
   $Id: ParInit.c,v 1.2 2000/01/13 14:34:08 hwloidl Exp $

   Initialising the parallel RTS

   An extension based on Kevin Hammond's GRAPH for PVM version
   P. Trinder, January 17th 1995.
   Adapted for the new RTS
   P. Trinder, July 1997.
   H-W. Loidl, November 1999.

   ------------------------------------------------------------------------ */

#ifdef PAR /* whole file */

#define NON_POSIX_SOURCE /* so says Solaris */

//@menu
//* Includes::			
//* Global variables::		
//* Initialisation Routines::	
//@end menu

//@node Includes, Global variables
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "ParallelRts.h"
#include <setjmp.h>
#include "LLC.h"
#include "HLC.h"

//@node Global variables, Initialisation Routines, Includes
//@subsection Global variables

/* Global conditions defined here. */

rtsBool	IAmMainThread = rtsFalse,	/* Set for the main thread	*/
	GlobalStopPending = rtsFalse;	/* Terminating			*/

/* Task identifiers for various interesting global tasks. */

GlobalTaskId IOTask = 0,                /* The IO Task Id		*/
             SysManTask = 0,            /* The System Manager Task Id	*/
             mytid = 0;                 /* This PE's Task Id		*/

rtsTime 	main_start_time;	/* When the program started	*/
rtsTime   	main_stop_time;	    	/* When the program finished    */
jmp_buf		exit_parallel_system;	/* How to abort from the RTS	*/


//rtsBool fishing = rtsFalse;             /* We have no fish out in the stream */
rtsTime last_fish_arrived_at = 0;       /* Time of arrival of most recent fish*/
nat     outstandingFishes = 0;          /* Number of active fishes */ 

//@cindex spark queue
/* GranSim: a globally visible array of spark queues */
rtsSpark *pending_sparks_hd[SPARK_POOLS],  /* ptr to start of a spark pool */ 
         *pending_sparks_tl[SPARK_POOLS],  /* ptr to end of a spark pool */ 
         *pending_sparks_lim[SPARK_POOLS],
         *pending_sparks_base[SPARK_POOLS]; 

//@cindex spark_limit
/* max number of sparks permitted on the PE; 
   see RtsFlags.ParFlags.maxLocalSparks */
nat spark_limit[SPARK_POOLS];

globalAddr theGlobalFromGA, theGlobalToGA;
/*
  HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACK !! see FETCH_ME_entry
  Only used within FETCH_ME_entry as local vars, but they shouldn't
  be defined locally in there -- that would move %esp and you'll never
  return from STG land.
  -- HWL
*/
globalAddr *rga_GLOBAL;
globalAddr *lga_GLOBAL;
globalAddr fmbqga_GLOBAL;
StgClosure *p_GLOBAL;

//@cindex PendingFetches
/* A list of fetch reply messages not yet processed; this list is filled
   by awaken_blocked_queue and processed by processFetches */
StgBlockedFetch *PendingFetches = END_BF_QUEUE;

//@cindex allPEs
GlobalTaskId *allPEs;

//@cindex nPEs
nat nPEs = 0;

//@cindex sparksIgnored
nat sparksIgnored = 0, sparksCreated = 0, 
    threadsIgnored = 0, threadsCreated = 0;

//@cindex advisory_thread_count
nat advisory_thread_count = 0;

/* Where to write the log file 
   This is now in Parallel.c 
FILE *gr_file = NULL;
char gr_filename[STATS_FILENAME_MAXLEN];
*/

/* Flag handling. */

#if 0
/* that's now all done via RtsFlags.ParFlags... */
rtsBool TraceSparks =    rtsFalse;		/* Enable the spark trace mode 		*/
rtsBool SparkLocally =   rtsFalse;		/* Use local threads if possible 	*/
rtsBool DelaySparks =    rtsFalse;		/* Use delayed sparking 		*/
rtsBool LocalSparkStrategy =   rtsFalse;	/* Either delayed threads or local threads*/
rtsBool GlobalSparkStrategy =  rtsFalse;	/* Export all threads	    	     	*/

rtsBool DeferGlobalUpdates =   rtsFalse;	/* Defer updating of global nodes	*/
#endif

//@node Initialisation Routines,  , Global variables
//@subsection Initialisation Routines

/*
  par_exit defines how to terminate the program.  If the exit code is
  non-zero (i.e. an error has occurred), the PE should not halt until
  outstanding error messages have been processed.  Otherwise, messages
  might be sent to non-existent Task Ids.  The infinite loop will actually
  terminate, since STG_Exception will call myexit\tr{(0)} when
  it received a PP_FINISH from the system manager task.
*/
//@cindex par_exit
void
shutdownParallelSystem(StgInt n)
{
  belch("   entered shutdownParallelSystem ...");
  ASSERT(GlobalStopPending = rtsTrue);
  sendOp(PP_FINISH, SysManTask);
  if (n != 0) 
    waitForTermination();
  else
    waitForPEOp(PP_FINISH, SysManTask);
  shutDownPE();
  IF_PAR_DEBUG(verbose,
	       belch("--++ shutting down PE %lx, %ld sparks created, %ld sparks Ignored, %ld threads created, %ld threads Ignored", 
		     (W_) mytid, sparksCreated, sparksIgnored,
		     threadsCreated, threadsIgnored));
  exit(n);
}

/* Some prototypes */
void srand48 (long);
time_t time (time_t *);

//@cindex initParallelSystem
void
initParallelSystem(void)
{
  belch("entered initParallelSystem ...");

  /* Don't buffer standard channels... */
  setbuf(stdout,NULL);
  setbuf(stderr,NULL);

  srand48(time(NULL) * getpid());  /*Initialise Random-number generator seed*/
                                   /* Used to select target of FISH message*/

  theGlobalFromGA.payload.gc.gtid = 0;
  theGlobalToGA.payload.gc.gtid = 0;

  //IF_PAR_DEBUG(verbose,
	       belch("initPackBuffer ...");
  if (!initPackBuffer())
    barf("initPackBuffer");

  // IF_PAR_DEBUG(verbose,
	       belch("initMoreBuffers ...");
  if (!initMoreBuffers())
    barf("initMoreBuffers");

  // IF_PAR_DEBUG(verbose,
	       belch("initSparkPools ...");
  if (!initSparkPools())
    barf("initSparkPools");
}

/* 
 * SynchroniseSystem synchronises the reduction task with the system
 * manager, and initialises the Global address tables (LAGA & GALA)
 */

//@cindex SynchroniseSystem
void
SynchroniseSystem(void)
{
  int i;

  fprintf(stderr, "SynchroniseSystem: nPEs=%d\n", nPEs); 

  initEachPEHook();                  /* HWL: hook to be execed on each PE */

  fprintf(stderr, "SynchroniseSystem: initParallelSystem\n");
  initParallelSystem();
  allPEs = startUpPE(nPEs);

  /* Initialize global address tables */
  initGAtables();

  /* Record the shortened the PE identifiers for LAGA etc. tables */
  for (i = 0; i < nPEs; ++i) {
    fprintf(stderr, "[%x] registering %d-th PE as %x\n", mytid, i, allPEs[i]);
    registerTask(allPEs[i]);
  }
}

#endif /* PAR -- whole file */

//@index
//* PendingFetches::  @cindex\s-+PendingFetches
//* SynchroniseSystem::  @cindex\s-+SynchroniseSystem
//* allPEs::  @cindex\s-+allPEs
//* initParallelSystem::  @cindex\s-+initParallelSystem
//* nPEs::  @cindex\s-+nPEs
//* par_exit::  @cindex\s-+par_exit
//* spark queue::  @cindex\s-+spark queue
//* sparksIgnored::  @cindex\s-+sparksIgnored
//@end index
