/* -----------------------------------------------------------------------------
 * $Id: RtsStartup.c,v 1.11 1999/05/04 10:19:19 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
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
#include "Weak.h"
#include "Ticky.h"

#if defined(PROFILING)
# include "ProfRts.h"
#elif defined(DEBUG)
# include "DebugProf.h"
#endif

#ifdef PAR
#include "ParInit.h"
#include "Parallel.h"
#include "LLC.h"
#endif

/*
 * Flag Structure
 */
struct RTS_FLAGS RtsFlags;

extern void startupHaskell(int argc, char *argv[])
{
    static int rts_has_started_up = 0;
    int i;

    /* To avoid repeated initialisations of the RTS */
   if (rts_has_started_up)
     return;
   else
     rts_has_started_up=1;

#if defined(PAR)
    int nPEs = 0;		    /* Number of PEs */
#endif

    /* The very first thing we do is grab the start time...just in case we're
     * collecting timing statistics.
     */
    start_time();

#ifdef PAR
/*
 *The parallel system needs to be initialised and synchronised before
 *the program is run.  
 */
    if (*argv[0] == '-') {     /* Look to see whether we're the Main Thread */
	IAmMainThread = rtsTrue;
        argv++; argc--;			/* Strip off flag argument */
/*	fprintf(stderr, "I am Main Thread\n"); */
    }
    /* 
     * Grab the number of PEs out of the argument vector, and
     * eliminate it from further argument processing.
     */
    nPEs = atoi(argv[1]);
    argv[1] = argv[0];
    argv++; argc--;
    initEachPEHook();                  /* HWL: hook to be execed on each PE */
    SynchroniseSystem();
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
   /* Initialise the parallel system -- before initHeap! */
    initParallelSystem();
   /* And start GranSim profiling if required: omitted for now
    *if (Rtsflags.ParFlags.granSimStats)
    *init_gr_profiling(rts_argc, rts_argv, prog_argc, prog_argv);
    */
#endif	/* PAR */

    /* initialize the storage manager */
    initStorage();

    /* initialise the stable pointer table */
    initStablePtrTable();

#if defined(PROFILING) || defined(DEBUG)
    initProfiling();
#endif

    /* Initialise the scheduler */
    initScheduler();

    /* Initialise the stats department */
    initStats();

#if 0
    initUserSignals();
#endif
 
    /* When the RTS and Prelude live in separate DLLs,
       we need to patch up the char- and int-like tables
       that the RTS keep after both DLLs have been loaded,
       filling in the tables with references to where the
       static info tables have been loaded inside the running
       process.
       
       Ditto for Bool closure tbl.
    */
#ifdef ENABLE_WIN32_DLL_SUPPORT
    for(i=0;i<=255;i++)
       (CHARLIKE_closure[i]).header.info = (const StgInfoTable*)&Czh_static_info;

    for(i=0;i<=32;i++)
       (INTLIKE_closure[i]).header.info = (const StgInfoTable*)&Izh_static_info;
       
#endif
    /* Record initialization times */
    end_init();
}

void
shutdownHaskell(void)
{
  /* Finalize any remaining weak pointers */
  finalizeWeakPointersNow();

#if defined(GRAN)
  #error FixMe.
  if (!RTSflags.GranFlags.granSimStats_suppressed)
    end_gr_simulation();
#endif

  /* clean up things from the storage manager's point of view */
  exitStorage();

#if defined(PROFILING) || defined(DEBUG)
  endProfiling();
#endif

#if defined(PROFILING) 
  report_ccs_profiling( );
#endif

#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

  /*
    This fflush is important, because: if "main" just returns,
    then we will end up in pre-supplied exit code that will close
    streams and flush buffers.  In particular we have seen: it
    will close fd 0 (stdin), then flush fd 1 (stdout), then <who
    cares>...
    
    But if you're playing with sockets, that "close fd 0" might
    suggest to the daemon that all is over, only to be presented
    with more stuff on "fd 1" at the flush.
    
    The fflush avoids this sad possibility.
   */
  fflush(stdout);
}


/* 
 * called from STG-land to exit the program cleanly 
 */

void  
stg_exit(I_ n)
{
#ifdef PAR
  par_exit(n);
#else
  OnExitHook();
  exit(n);
#endif
}
