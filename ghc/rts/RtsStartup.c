/* -----------------------------------------------------------------------------
 * $Id: RtsStartup.c,v 1.36 2000/03/30 12:03:30 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"  
#include "Storage.h"    /* initStorage, exitStorage */
#include "StablePriv.h" /* initStablePtrTable */
#include "Schedule.h"   /* initScheduler */
#include "Stats.h"      /* initStats */
#include "Signals.h"
#include "Itimer.h"
#include "Weak.h"
#include "Ticky.h"
#include "StgRun.h"
#include "StgStartup.h"
#include "Prelude.h"		/* fixupRTStoPreludeRefs */

#if defined(PROFILING) || defined(DEBUG)
# include "ProfRts.h"
# include "ProfHeap.h"
#endif

#if defined(GRAN)
#include "GranSimRts.h"
#include "ParallelRts.h"
#endif

#if defined(PAR)
#include "ParInit.h"
#include "Parallel.h"
#include "LLC.h"
#endif

/*
 * Flag Structure
 */
struct RTS_FLAGS RtsFlags;

static int rts_has_started_up = 0;
#if defined(PAR)
static ullong startTime = 0;
#endif

EXTFUN(__init_Prelude);
static void initModules ( void * );

void
startupHaskell(int argc, char *argv[], void *init_root)
{
    /* To avoid repeated initialisations of the RTS */
   if (rts_has_started_up)
     return;
   else
     rts_has_started_up=1;

    /* The very first thing we do is grab the start time...just in case we're
     * collecting timing statistics.
     */
    start_time();

#ifdef PAR
/*
 * The parallel system needs to be initialised and synchronised before
 * the program is run.  
 */
    fprintf(stderr, "startupHaskell: argv[0]=%s\n", argv[0]);
    if (*argv[0] == '-') {     /* Look to see whether we're the Main Thread */
	IAmMainThread = rtsTrue;
        argv++; argc--;			/* Strip off flag argument */
	// IF_PAR_DEBUG(verbose,
		     fprintf(stderr, "[%x] I am Main Thread\n", mytid);
    }
    /* 
     * Grab the number of PEs out of the argument vector, and
     * eliminate it from further argument processing.
     */
    nPEs = atoi(argv[1]);
    argv[1] = argv[0];
    argv++; argc--;
    initEachPEHook();                  /* HWL: hook to be execed on each PE */
#endif

    /* Set the RTS flags to default values. */
    initRtsFlagsDefaults();

    /* Call the user hook to reset defaults, if present */
    defaultsHook();

    /* Parse the flags, separating the RTS flags from the programs args */
    setupRtsFlags(&argc, argv, &rts_argc, rts_argv);
    prog_argc = argc;
    prog_argv = argv;

#if defined(PAR)
    /* NB: this really must be done after processing the RTS flags */
    fprintf(stderr, "Synchronising system (%d PEs)\n", nPEs);
    SynchroniseSystem();             // calls initParallelSystem etc
#endif	/* PAR */

    /* initialise scheduler data structures (needs to be done before
     * initStorage()).
     */
    initScheduler();

#if defined(GRAN)
    /* And start GranSim profiling if required: */
    if (RtsFlags.GranFlags.GranSimStats.Full)
      init_gr_simulation(rts_argc, rts_argv, prog_argc, prog_argv);
#elif defined(PAR)
    /* And start GUM profiling if required: */
    if (RtsFlags.ParFlags.ParStats.Full)
      init_gr_simulation(rts_argc, rts_argv, prog_argc, prog_argv);
#endif	/* PAR || GRAN */

    /* initialize the storage manager */
    initStorage();

    /* initialise the stable pointer table */
    initStablePtrTable();

#if defined(PROFILING) || defined(DEBUG)
    initProfiling1();
#endif

    /* run the per-module initialisation code */
#if !defined(INTERPRETER)
    initModules(init_root);
#endif

#if defined(PROFILING) || defined(DEBUG)
    initProfiling2();
#endif

    /* start the ticker */
    install_vtalrm_handler();
    initialize_virtual_timer(TICK_MILLISECS);

    /* start our haskell execution tasks */
#ifdef SMP
    startTasks();
#endif

    /* Initialise the stats department */
    initStats();

#if !defined(mingw32_TARGET_OS) && !defined(PAR)
    /* Initialise the user signal handler set */
    initUserSignals();
    /* Set up handler to run on SIGINT, etc. */
    init_default_handlers();
#endif
 
#if !defined(INTERPRETER)
    /* Initialise pointers from the RTS to the prelude.  
       Only for compiled code -- the interpreter
       will call this itself later, so don't do so now.
    */
    fixupRTStoPreludeRefs(NULL);
#endif

    /* Record initialization times */
    end_init();
}

/* -----------------------------------------------------------------------------
   Per-module initialisation

   This process traverses all the compiled modules in the program
   starting with "Main", and performing per-module initialisation for
   each one.

   So far, two things happen at initialisation time:

      - we register stable names for each foreign-exported function
        in that module.  This prevents foreign-exported entities, and
	things they depend on, from being garbage collected.

      - we supply a unique integer to each statically declared cost
        centre and cost centre stack in the program.

   The code generator inserts a small function "__init_<module>" in each
   module and calls the registration functions in each of the modules
   it imports.  So, if we call "__init_PrelMain", each reachable module in the
   program will be registered (because PrelMain.mainIO calls Main.main).

   The init* functions are compiled in the same way as STG code,
   i.e. without normal C call/return conventions.  Hence we must use
   StgRun to call this stuff.
   -------------------------------------------------------------------------- */

#ifndef INTERPRETER

/* The init functions use an explicit stack... 
 */
#define INIT_STACK_SIZE  (BLOCK_SIZE * 4)
F_ *init_stack = NULL;
nat init_sp = 0;

static void
initModules ( void *init_root )
{
  init_sp = 0;
  init_stack = (F_ *)allocate(INIT_STACK_SIZE / sizeof(W_));
  init_stack[init_sp++] = (F_)stg_init_ret;
  init_stack[init_sp++] = (F_)__init_Prelude;
  if (init_root != NULL) {
    init_stack[init_sp++] = (F_)init_root;
  }

  MainRegTable.rSp = (P_)(init_stack + init_sp);
  StgRun((StgFunPtr)stg_init, NULL/* no reg table */);
}

#endif /* !INTERPRETER */

/* -----------------------------------------------------------------------------
 * Shutting down the RTS - two ways of doing this, one which
 * calls exit(), one that doesn't.
 *
 * (shutdownHaskellAndExit() is called by System.exitWith).
 * -----------------------------------------------------------------------------
 */
void
shutdownHaskellAndExit(int n)
{
  OnExitHook();
  shutdownHaskell();
  stg_exit(n);
}

void
shutdownHaskell(void)
{
  if (!rts_has_started_up)
     return;

  /* start timing the shutdown */
  stat_startExit();

#if !defined(GRAN)
  /* Finalize any remaining weak pointers */
  finalizeWeakPointersNow();
#endif

#if defined(GRAN)
  /* end_gr_simulation prints global stats if requested -- HWL */
  if (!RtsFlags.GranFlags.GranSimStats.Suppressed)
    end_gr_simulation();
#endif

  /* stop all running tasks */
  exitScheduler();

  /* stop the ticker */
  initialize_virtual_timer(0);
  
  /* reset the standard file descriptors to blocking mode */
  resetNonBlockingFd(0);
  resetNonBlockingFd(1);
  resetNonBlockingFd(2);

  /* stop timing the shutdown, we're about to print stats */
  stat_endExit();

  /* clean up things from the storage manager's point of view.
   * also outputs the stats (+RTS -s) info.
   */
  exitStorage();

#if defined(PROFILING) || defined(DEBUG)
  endProfiling();
#endif

#if defined(PROFILING) 
  report_ccs_profiling();
#endif

#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

  rts_has_started_up=0;

#if defined(PAR)
  shutdownParallelSystem(0);
#endif

}

/* 
 * called from STG-land to exit the program
 */

void  
stg_exit(I_ n)
{
#if 0 /* def PAR */
  par_exit(n);
#else
  exit(n);
#endif
}

