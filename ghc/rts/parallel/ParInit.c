/* --------------------------------------------------------------------------
   Time-stamp: <Wed Mar 21 2001 16:37:16 Stardate: [-30]6363.46 hwloidl>

   Initialising the parallel RTS

   An extension based on Kevin Hammond's GRAPH for PVM version
   P. Trinder, January 17th 1995.
   Adapted for the new RTS
   P. Trinder, July 1997.
   H-W. Loidl, November 1999.

   ------------------------------------------------------------------------ */

#ifdef PAR /* whole file */

//@menu
//* Includes::			
//* Global variables::		
//* Initialisation Routines::	
//@end menu

//@node Includes, Global variables
//@subsection Includes

/* Evidently not Posix */
/* #include "PosixSource.h" */

#include <setjmp.h>
#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "ParallelRts.h"
#include "Sparks.h"
#include "LLC.h"
#include "HLC.h"

//@node Global variables, Initialisation Routines, Includes
//@subsection Global variables

/* Global conditions defined here. */

rtsBool	IAmMainThread = rtsFalse;	/* Set for the main thread	*/

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

globalAddr theGlobalFromGA;

/* For flag handling see RtsFlags.h */

//@node Prototypes
//@subsection Prototypes

/* Needed for FISH messages (initialisation of random number generator) */
void srand48 (long);
time_t time (time_t *);

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
//@cindex shutdownParallelSystem
void
shutdownParallelSystem(StgInt n)
{
  /* use the file specified via -S */ 
  FILE *sf = RtsFlags.GcFlags.statsFile;

  IF_PAR_DEBUG(verbose,
	       if (n==0)
  	         belch("==== entered shutdownParallelSystem ...");
               else
  	         belch("==== entered shutdownParallelSystem (ERROR %d)...", n);
	       );
  
  stopPEComms(n);

#if 0
  if (sf!=(FILE*)NULL) 
    fprintf(sf, "PE %x: %u sparks created, %u sparks Ignored, %u threads created, %u threads Ignored", 
	    (W_) mytid, sparksCreated, sparksIgnored,
	    threadsCreated, threadsIgnored);
#endif

  ShutdownEachPEHook();
}

//@cindex initParallelSystem
void
initParallelSystem(void)
{
  /* Don't buffer standard channels... */
  setbuf(stdout,NULL);
  setbuf(stderr,NULL);
  
  srand48(time(NULL) * getpid()); /* Initialise Random-number generator seed*/
                                  /* used to select target of FISH message*/
  if (!InitPackBuffer())
    barf("InitPackBuffer");

  if (!initMoreBuffers())
    barf("initMoreBuffers");

  if (!initSparkPools())
    barf("initSparkPools");
}

/* 
 * SynchroniseSystem synchronises the reduction task with the system
 * manager, and initialises the Global address tables (LAGA & GALA)
 */

//@cindex synchroniseSystem
void
synchroniseSystem(void)
{
  /* Only in debug mode? */
  fprintf(stderr, "==== Starting parallel execution on %d processors ...\n", nPEs);

  InitEachPEHook();                  /* HWL: hook to be execed on each PE */

  /* Initialize global address tables */
  initGAtables();

  initParallelSystem();
  
  startPEComms();
}

/* 
  Do the startup stuff (this is PVM specific!).
  Determines global vars: mytid, IAmMainThread, SysManTask, nPEs
  Called at the beginning of RtsStartup.startupHaskell
*/
void 
startupParallelSystem(char *argv[]) { 
 mytid = pvm_mytid();	        /* Connect to PVM */

 if (*argv[0] == '-') {         /* Look to see whether we're the Main Thread */
  IAmMainThread = rtsTrue;
  sscanf(argv[0],"-%0X",&SysManTask);  /* extract SysMan task ID*/	
  argv++;	                       /* Strip off flag argument */
 } else {
  SysManTask = pvm_parent();
 }

 IF_PAR_DEBUG(verbose,
	       fprintf(stderr, "==== [%x] %s PE located SysMan at %x\n",
		       mytid, IAmMainThread?"Main":"Remote", SysManTask));

 nPEs = atoi(argv[1]);
}

/* 
   Exception handler during startup.
*/
void *
processUnexpectedMessageDuringStartup(rtsPacket p) {
  OpCode opCode;
  GlobalTaskId sender_id;

  getOpcodeAndSender(p, &opCode, &sender_id);

  switch(opCode) { 
      case PP_FISH:
        bounceFish();
	break;
#if defined(DIST)
      case PP_REVAL:
	bounceReval();
	break;
#endif
      case PP_FINISH:
        stg_exit(EXIT_SUCCESS);	
	break;
      default:
	fprintf(stderr,"== Task %x: Unexpected OpCode %x (%s) from %x in startPEComms\n",
		mytid, opCode, getOpName(opCode), sender_id);
    }
}

void 
startPEComms(void){ 

  startUpPE(); 
  allPEs = (GlobalTaskId *) stgMallocBytes(sizeof(GlobalTaskId) * MAX_PES,
					   "(PEs)");
  
  /* Send our tid and IAmMainThread flag back to SysMan */
  sendOp1(PP_READY, SysManTask, (StgWord)IAmMainThread);  
  /* Wait until we get the PE-Id table from Sysman */    
  waitForPEOp(PP_PETIDS, SysManTask, processUnexpectedMessageDuringStartup); 

  IF_PAR_DEBUG(verbose,
               belch("==-- startPEComms: methinks we just received a PP_PETIDS message"));

  /* Digest the PE table we received */
  processPEtids();
}

void
processPEtids(void) { 
  long newPE;
  nat i, sentPEs, currentPEs;

  nPEs=0;
	  
  currentPEs = nPEs;

  IF_PAR_DEBUG(verbose,
		belch("==-- processPEtids: starting to iterate over a PVM buffer"));
  /* ToDo: this has to go into LLComms !!! */
  GetArgs(&sentPEs,1);

  ASSERT(sentPEs > currentPEs);
  ASSERT(sentPEs < MAX_PES); /* enforced by SysMan too*/  
  
  for (i = 0; i < sentPEs; i++) { 
    GetArgs(&newPE,1);
    if (i<currentPEs) { 
      ASSERT(newPE == allPEs[i]);
    } else { 
#if defined(DIST)
      // breaks with PAR && !DEBUG
      IF_PAR_DEBUG(verbose,
	fprintf(stderr, "[%x] registering %d'th %x\n", mytid, i, newPE)); 
      if(!looks_like_tid(newPE))
	  barf("unacceptable taskID %x\n",newPE);
#endif
      allPEs[i] = newPE;      
      nPEs++;
      registerTask(newPE); 
    }
  }

  IF_PAR_DEBUG(verbose,
  	       /* debugging */
  	       belch("++++ [%x] PE table as I see it:", mytid);
  	       for (i = 0; i < sentPEs; i++) { 
  		 belch("++++ allPEs[%d] = %x", i, allPEs[i]);
               });
}

void 
stopPEComms(StgInt n) { 
  if (n != 0) { 
    /* In case sysman doesn't know about us yet...
    pvm_initsend(PvmDataDefault);
    PutArgs(&IAmMainThread,1);
    pvm_send(SysManTask, PP_READY);
     */
    sendOp(PP_READY, SysManTask);  
  } 
  
  sendOp2(PP_FINISH, SysManTask, n, n);  
  waitForPEOp(PP_FINISH, SysManTask, NULL);
  fflush(gr_file);
  shutDownPE();
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

