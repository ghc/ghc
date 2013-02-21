/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

// PAPI uses caddr_t, which is not POSIX
#ifndef USE_PAPI
#include "PosixSource.h"
#endif

#include "Rts.h"
#include "RtsAPI.h"
#include "HsFFI.h"

#include "sm/Storage.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Prelude.h"
#include "Capability.h"
#include "Schedule.h"   /* initScheduler */
#include "Stats.h"      /* initStats */
#include "STM.h"        /* initSTM */
#include "RtsSignals.h"
#include "Weak.h"
#include "Ticky.h"
#include "StgRun.h"
#include "Prelude.h"		/* fixupRTStoPreludeRefs */
#include "ThreadLabels.h"
#include "sm/BlockAlloc.h"
#include "Trace.h"
#include "Stable.h"
#include "Hash.h"
#include "Profiling.h"
#include "Timer.h"
#include "Globals.h"
#include "FileLock.h"
void exitLinker( void );	// there is no Linker.h file to include

#if defined(RTS_GTK_FRONTPANEL)
#include "FrontPanel.h"
#endif

#if defined(PROFILING)
# include "ProfHeap.h"
# include "RetainerProfile.h"
#endif

#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
#include "win32/AsyncIO.h"
#endif

#if !defined(mingw32_HOST_OS)
#include "posix/TTY.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#if USE_PAPI
#include "Papi.h"
#endif

// Count of how many outstanding hs_init()s there have been.
static int hs_init_count = 0;

static void flushStdHandles(void);

const RtsConfig defaultRtsConfig  = {
    .rts_opts_enabled = RtsOptsSafeOnly,
    .rts_opts = NULL
};

/* -----------------------------------------------------------------------------
   Initialise floating point unit on x86 (currently disabled; See Note
   [x86 Floating point precision] in compiler/nativeGen/X86/Instr.hs)
   -------------------------------------------------------------------------- */

#define X86_INIT_FPU 0

#if X86_INIT_FPU
static void
x86_init_fpu ( void )
{
  __volatile unsigned short int fpu_cw;

  // Grab the control word
  __asm __volatile ("fnstcw %0" : "=m" (fpu_cw));

#if 0
  printf("fpu_cw: %x\n", fpu_cw);
#endif

  // Set bits 8-9 to 10 (64-bit precision).
  fpu_cw = (fpu_cw & 0xfcff) | 0x0200;

  // Store the new control word back
  __asm __volatile ("fldcw %0" : : "m" (fpu_cw));
}
#endif

/* -----------------------------------------------------------------------------
   Starting up the RTS
   -------------------------------------------------------------------------- */

void
hs_init(int *argc, char **argv[])
{
    hs_init_ghc(argc, argv, defaultRtsConfig);
}

void
hs_init_ghc(int *argc, char **argv[], RtsConfig rts_config)
{
    hs_init_count++;
    if (hs_init_count > 1) {
	// second and subsequent inits are ignored
	return;
    }

    setlocale(LC_CTYPE,"");

    /* Initialise the stats department, phase 0 */
    initStats0();

    /* Initialize system timer before starting to collect stats */
    initializeTimer();

    /* Next we do is grab the start time...just in case we're
     * collecting timing statistics.
     */
    stat_startInit();

    /* Set the RTS flags to default values. */

    initRtsFlagsDefaults();

    /* Call the user hook to reset defaults, if present */
    defaultsHook();

    /* Parse the flags, separating the RTS flags from the programs args */
    if (argc == NULL || argv == NULL) {
        // Use a default for argc & argv if either is not supplied
        int my_argc = 1;
        char *my_argv[] = { "<unknown>", NULL };
        setFullProgArgv(my_argc,my_argv);
        setupRtsFlags(&my_argc, my_argv,
                      rts_config.rts_opts_enabled, rts_config.rts_opts);
    } else {
	setFullProgArgv(*argc,*argv);
        setupRtsFlags(argc, *argv,
                      rts_config.rts_opts_enabled, rts_config.rts_opts);
    }

    /* Initialise the stats department, phase 1 */
    initStats1();

#ifdef USE_PAPI
    papi_init();
#endif

    /* initTracing must be after setupRtsFlags() */
#ifdef TRACING
    initTracing();
#endif
    /* Trace the startup event
     */
    traceEventStartup();

    /* initialise scheduler data structures (needs to be done before
     * initStorage()).
     */
    initScheduler();

    /* Trace some basic information about the process */
    traceWallClockTime();
    traceOSProcessInfo();

    /* initialize the storage manager */
    initStorage();

    /* initialise the stable pointer table */
    initStableTables();

    /* Add some GC roots for things in the base package that the RTS
     * knows about.  We don't know whether these turn out to be CAFs
     * or refer to CAFs, but we have to assume that they might.
     */
    getStablePtr((StgPtr)runIO_closure);
    getStablePtr((StgPtr)runNonIO_closure);
    getStablePtr((StgPtr)flushStdHandles_closure);
    getStablePtr((StgPtr)runFinalizerBatch_closure);
    getStablePtr((StgPtr)stackOverflow_closure);
    getStablePtr((StgPtr)heapOverflow_closure);
    getStablePtr((StgPtr)unpackCString_closure);
    getStablePtr((StgPtr)blockedIndefinitelyOnMVar_closure);
    getStablePtr((StgPtr)blockedIndefinitelyOnConcDS_closure);
    getStablePtr((StgPtr)nonTermination_closure);
    getStablePtr((StgPtr)blockedIndefinitelyOnSTM_closure);
    getStablePtr((StgPtr)nestedAtomically_closure);

    getStablePtr((StgPtr)runSparks_closure);
    getStablePtr((StgPtr)ensureIOManagerIsRunning_closure);

    getStablePtr((StgPtr)defaultUpcall_closure);
    getStablePtr((StgPtr)defaultExceptionHandler_closure);
    getStablePtr((StgPtr)initSContStatus_closure);
    getStablePtr((StgPtr)scheduleSContActionRts_closure);
    getStablePtr((StgPtr)yieldControlActionRts_closure);
    getStablePtr((StgPtr)ioManagerCapabilitiesChanged_closure);
#ifndef mingw32_HOST_OS
    getStablePtr((StgPtr)runHandlers_closure);
#endif

    /* initialise the shared Typeable store */
    initGlobalStore();

    /* initialise file locking, if necessary */
    initFileLocking();

#if defined(DEBUG)
    /* initialise thread label table (tso->char*) */
    initThreadLabelTable();
#endif

    initProfiling1();

    /* start the virtual timer 'subsystem'. */
    initTimer();
    startTimer();

#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        /* Initialise the user signal handler set */
        initUserSignals();
        /* Set up handler to run on SIGINT, etc. */
        initDefaultHandlers();
    }
#endif

#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
    startupAsyncIO();
#endif

#ifdef RTS_GTK_FRONTPANEL
    if (RtsFlags.GcFlags.frontpanel) {
	initFrontPanel();
    }
#endif

#if X86_INIT_FPU
    x86_init_fpu();
#endif

    startupHpc();

    // This must be done after module initialisation.
    // ToDo: make this work in the presence of multiple hs_add_root()s.
    initProfiling2();

    // ditto.
#if defined(THREADED_RTS)
    ioManagerStart();
#endif

    //This has to come after initScheduler and initStorage.
    initUpcallThreads();

    /* Record initialization times */
    stat_endInit();
}

// Compatibility interface
void
startupHaskell(int argc, char *argv[], void (*init_root)(void) STG_UNUSED)
{
    hs_init(&argc, &argv);
}


/* -----------------------------------------------------------------------------
   hs_add_root: backwards compatibility.  (see #3252)
   -------------------------------------------------------------------------- */

void
hs_add_root(void (*init_root)(void) STG_UNUSED)
{
    /* nothing */
}

/* ----------------------------------------------------------------------------
 * Shutting down the RTS
 *
 * The wait_foreign parameter means:
 *       True  ==> wait for any threads doing foreign calls now.
 *       False ==> threads doing foreign calls may return in the
 *                 future, but will immediately block on a mutex.
 *                 (capability->lock).
 *
 * If this RTS is a DLL that we're about to unload, then you want
 * safe=True, otherwise the thread might return to code that has been
 * unloaded.  If this is a standalone program that is about to exit,
 * then you can get away with safe=False, which is better because we
 * won't hang on exit if there is a blocked foreign call outstanding.
 *
 ------------------------------------------------------------------------- */

static void
hs_exit_(rtsBool wait_foreign)
{
    if (hs_init_count <= 0) {
	errorBelch("warning: too many hs_exit()s");
	return;
    }
    hs_init_count--;
    if (hs_init_count > 0) {
	// ignore until it's the last one
	return;
    }

    /* start timing the shutdown */
    stat_startExit();

    OnExitHook();

    flushStdHandles();

    // sanity check
#if defined(DEBUG)
    checkFPUStack();
#endif

#if defined(THREADED_RTS)
    ioManagerDie();
#endif

    /* stop all running tasks */
    exitScheduler(wait_foreign);

    /* run C finalizers for all active weak pointers */
    runAllCFinalizers(weak_ptr_list);

#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        freeSignalHandlers();
    }
#endif

    /* stop the ticker */
    stopTimer();
    exitTimer(wait_foreign);

    // set the terminal settings back to what they were
#if !defined(mingw32_HOST_OS)
    resetTerminalSettings();
#endif

    // uninstall signal handlers
    resetDefaultHandlers();

    /* stop timing the shutdown, we're about to print stats */
    stat_endExit();

    /* shutdown the hpc support (if needed) */
    exitHpc();

    // clean up things from the storage manager's point of view.
    // also outputs the stats (+RTS -s) info.
    exitStorage();

    /* free the tasks */
    freeScheduler();

    /* free shared Typeable store */
    exitGlobalStore();

    /* free linker data */
    exitLinker();

    /* free file locking tables, if necessary */
    freeFileLocking();

    /* free the stable pointer table */
    exitStableTables();

#if defined(DEBUG)
    /* free the thread label table */
    freeThreadLabelTable();
#endif

#ifdef RTS_GTK_FRONTPANEL
    if (RtsFlags.GcFlags.frontpanel) {
	stopFrontPanel();
    }
#endif

#if defined(PROFILING)
    reportCCSProfiling();
#endif

    endProfiling();
    freeProfiling();

#ifdef PROFILING
    // Originally, this was in report_ccs_profiling().  Now, retainer
    // profiling might tack some extra stuff on to the end of this file
    // during endProfiling().
    if (prof_file != NULL) fclose(prof_file);
#endif

#ifdef TRACING
    endTracing();
    freeTracing();
#endif

#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
    shutdownAsyncIO(wait_foreign);
#endif

    /* free hash table storage */
    exitHashTable();

    // Finally, free all our storage.  However, we only free the heap
    // memory if we have waited for foreign calls to complete;
    // otherwise a foreign call in progress may still be referencing
    // heap memory (e.g. by being passed a ByteArray#).
    freeStorage(wait_foreign);

    // Free the various argvs
    freeRtsArgs();
}

// Flush stdout and stderr.  We do this during shutdown so that it
// happens even when the RTS is being used as a library, without a
// main (#5594)
static void flushStdHandles(void)
{
    Capability *cap;
    cap = rts_lock();
    rts_evalIO(&cap, flushStdHandles_closure, NULL);
    rts_unlock(cap);
}

// The real hs_exit():
void
hs_exit(void)
{
    hs_exit_(rtsTrue);
    // be safe; this might be a DLL
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
    // even if hs_init_count > 1, we still want to shut down the RTS
    // and exit immediately (see #5402)
    hs_init_count = 1;

    // we're about to exit(), no need to wait for foreign calls to return.
    hs_exit_(rtsFalse);

    stg_exit(n);
}

#ifndef mingw32_HOST_OS
void
shutdownHaskellAndSignal(int sig)
{
    hs_exit_(rtsFalse);
    kill(getpid(),sig);
}
#endif

/*
 * called from STG-land to exit the program
 */

void (*exitFn)(int) = 0;

void
stg_exit(int n)
{
  if (exitFn)
    (*exitFn)(n);
  exit(n);
}
