/* -----------------------------------------------------------------------------
 * $Id: RtsStartup.c,v 1.68 2003/01/28 16:30:06 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
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
#include "HsFFI.h"
#include "Linker.h"
#include "ThreadLabels.h"

#if defined(RTS_GTK_FRONTPANEL)
#include "FrontPanel.h"
#endif

#if defined(PROFILING) || defined(DEBUG)
# include "Profiling.h"
# include "ProfHeap.h"
# include "RetainerProfile.h"
#endif

#if defined(GRAN)
# include "GranSimRts.h"
#endif

#if defined(GRAN) || defined(PAR)
# include "ParallelRts.h"
#endif

#if defined(PAR)
# include "Parallel.h"
# include "LLC.h"
#endif

#include <stdlib.h>

// Flag Structure
struct RTS_FLAGS RtsFlags;

// Count of how many outstanding hs_init()s there have been.
static int hs_init_count = 0;

/* -----------------------------------------------------------------------------
   Starting up the RTS
   -------------------------------------------------------------------------- */

void
hs_init(int *argc, char **argv[])
{
    hs_init_count++;
    if (hs_init_count > 1) {
	// second and subsequent inits are ignored
	return;
    }

    /* The very first thing we do is grab the start time...just in case we're
     * collecting timing statistics.
     */
    stat_startInit();

#ifdef PAR
    /*
     * The parallel system needs to be initialised and synchronised before
     * the program is run.  
     */ 
    startupParallelSystem(argv);
     
    if (*argv[0] == '-') { /* Strip off mainPE flag argument */
      argv++; 
      argc--;			
    }

    argv[1] = argv[0];   /* ignore the nPEs argument */
    argv++; argc--;
#endif

    /* Set the RTS flags to default values. */
    initRtsFlagsDefaults();

    /* Call the user hook to reset defaults, if present */
    defaultsHook();

    /* Parse the flags, separating the RTS flags from the programs args */
    if (argc != NULL && argv != NULL) {
	setupRtsFlags(argc, *argv, &rts_argc, rts_argv);
	prog_argc = *argc;
	prog_argv = *argv;
    }

#if defined(PAR)
    /* NB: this really must be done after processing the RTS flags */
    IF_PAR_DEBUG(verbose,
                 fprintf(stderr, "==== Synchronising system (%d PEs)\n", nPEs));
    synchroniseSystem();             // calls initParallelSystem etc
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

    /* initialise thread label table (tso->char*) */
    initThreadLabelTable();

#if defined(PROFILING) || defined(DEBUG)
    initProfiling1();
#endif

#if defined(PROFILING) || defined(DEBUG)
    initProfiling2();
#endif

    /* start the virtual timer 'subsystem'. */
    startVirtTimer(TICK_MILLISECS);

    /* Initialise the stats department */
    initStats();

#if !defined(mingw32_TARGET_OS) && !defined(PAR)
    /* Initialise the user signal handler set */
    initUserSignals();
    /* Set up handler to run on SIGINT, etc. */
    initDefaultHandlers();
#endif
 
#ifdef RTS_GTK_FRONTPANEL
    if (RtsFlags.GcFlags.frontpanel) {
	initFrontPanel();
    }
#endif

    /* Record initialization times */
    stat_endInit();
}

// Compatibility interface
void
startupHaskell(int argc, char *argv[], void (*init_root)(void))
{
    hs_init(&argc, &argv);
    hs_add_root(init_root);
}


/* -----------------------------------------------------------------------------
   Getting the program's arguments.

   This is used by System.Environment.getArgs.
   -------------------------------------------------------------------------- */

void
getProgArgv(int *argc, char **argv[])
{
    *argc = prog_argc;
    *argv = prog_argv;
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

   The code generator inserts a small function "__stginit_<module>" in each
   module and calls the registration functions in each of the modules it
   imports.

   The init* functions are compiled in the same way as STG code,
   i.e. without normal C call/return conventions.  Hence we must use
   StgRun to call this stuff.
   -------------------------------------------------------------------------- */

/* The init functions use an explicit stack... 
 */
#define INIT_STACK_BLOCKS  4
static F_ *init_stack = NULL;

void
hs_add_root(void (*init_root)(void))
{
    bdescr *bd;
#ifdef SMP
    Capability cap;
#else
#define cap MainCapability
#endif
    nat init_sp;

    if (hs_init_count <= 0) {
	barf("hs_add_root() must be called after hs_init()");
    }

    init_sp = 0;
    bd = allocGroup(INIT_STACK_BLOCKS);
    init_stack = (F_ *)bd->start;
    init_stack[init_sp++] = (F_)stg_init_ret;
    if (init_root != NULL) {
	init_stack[init_sp++] = (F_)init_root;
    }
    
    cap.r.rSp = (P_)(init_stack + init_sp);
    StgRun((StgFunPtr)stg_init, &cap.r);

    freeGroup(bd);
}

/* -----------------------------------------------------------------------------
   Shutting down the RTS
   -------------------------------------------------------------------------- */

void
hs_exit(void)
{
    if (hs_init_count <= 0) {
	barf("too many hs_exit()s");
    }
    hs_init_count--;
    if (hs_init_count > 0) {
	// ignore until it's the last one
	return;
    }

    /* start timing the shutdown */
    stat_startExit();
    
    /* stop all running tasks */
    exitScheduler();
    
#if !defined(GRAN)
    /* Finalize any remaining weak pointers */
    finalizeWeakPointersNow();
#endif
    
#if defined(GRAN)
    /* end_gr_simulation prints global stats if requested -- HWL */
    if (!RtsFlags.GranFlags.GranSimStats.Suppressed)
	end_gr_simulation();
#endif
    
    /* stop the ticker */
    stopVirtTimer();
    
    /* reset the standard file descriptors to blocking mode */
    resetNonBlockingFd(0);
    resetNonBlockingFd(1);
    resetNonBlockingFd(2);

#if defined(PAR)
    /* controlled exit; good thread! */
    shutdownParallelSystem(0);
    
    /* global statistics in parallel system */
    PAR_TICKY_PAR_END();
#endif

    /* stop timing the shutdown, we're about to print stats */
    stat_endExit();
    
    /* clean up things from the storage manager's point of view.
     * also outputs the stats (+RTS -s) info.
     */
    exitStorage();
    
#ifdef RTS_GTK_FRONTPANEL
    if (RtsFlags.GcFlags.frontpanel) {
	stopFrontPanel();
    }
#endif

#if defined(PROFILING) 
    reportCCSProfiling();
#endif

#if defined(PROFILING) || defined(DEBUG)
    endProfiling();
#endif

#ifdef PROFILING
    // Originally, this was in report_ccs_profiling().  Now, retainer
    // profiling might tack some extra stuff on to the end of this file
    // during endProfiling().
    fclose(prof_file);
#endif
    
#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif
}

// Compatibility interfaces
void
shutdownHaskell(void)
{
    hs_exit();
}

void
shutdownHaskellAndExit(int n)
{
    if (hs_init_count == 1) {
	OnExitHook();
	hs_exit();
#if defined(PAR)
	/* really exit (stg_exit() would call shutdownParallelSystem() again) */
	exit(n);
#else
	stg_exit(n);
#endif
    }
}

/* 
 * called from STG-land to exit the program
 */

#ifdef PAR
static int exit_started=rtsFalse;
#endif

void  
stg_exit(int n)
{ 
#ifdef PAR
  /* HACK: avoid a loop when exiting due to a stupid error */
  if (exit_started) 
    return;
  exit_started=rtsTrue;

  IF_PAR_DEBUG(verbose, fprintf(stderr,"==-- stg_exit %d on [%x]...", n, mytid));
  shutdownParallelSystem(n);
#endif
  exit(n);
}

