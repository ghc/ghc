/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "HsFFI.h"

#include "sm/Storage.h"
#include "linker/MMap.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Prelude.h"
#include "Printer.h"    /* DEBUG_LoadSymbols */
#include "Schedule.h"   /* initScheduler */
#include "Stats.h"      /* initStats */
#include "STM.h"        /* initSTM */
#include "RtsSignals.h"
#include "Weak.h"
#include "ForeignExports.h"     /* processForeignExports */
#include "Ticky.h"
#include "StgRun.h"
#include "Prelude.h"            /* fixupRTStoPreludeRefs */
#include "Adjustor.h"           /* initAdjustors */
#include "sm/BlockAlloc.h"
#include "Trace.h"
#include "StableName.h"
#include "StablePtr.h"
#include "StaticPtrTable.h"
#include "Hash.h"
#include "Profiling.h"
#include "IPE.h"
#include "ProfHeap.h"
#include "Timer.h"
#include "Globals.h"
#include "FileLock.h"
#include "LinkerInternals.h"
#include "LibdwPool.h"
#include "sm/CNF.h"
#include "TopHandler.h"
#include "CheckVectorSupport.h"

#if defined(PROFILING)
# include "ProfHeap.h"
# include "RetainerProfile.h"
#endif

#include "IOManager.h"
#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
#include "win32/AsyncMIO.h"
#include "win32/AsyncWinIO.h"
#endif

#if defined(mingw32_HOST_OS)
#include <fenv.h>
#include <windows.h>
#else
#include "posix/TTY.h"
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif
#if defined(HAVE_LOCALE_H)
#include <locale.h>
#endif

// Count of how many outstanding hs_init()s there have been.
static StgWord hs_init_count = 0;
static bool rts_shutdown = false;

#if defined(mingw32_HOST_OS)
/* Indicates CodePage to set program to after exit.  */
static int64_t __codePage = -1;
#endif

static void flushStdHandles(void);

/* -----------------------------------------------------------------------------
   Initialise floating point unit on x86 (currently disabled; See Note
   [x86 Floating point precision] in compiler/GHC/CmmToAsm/X86/Instr.hs)
   -------------------------------------------------------------------------- */

#define X86_INIT_FPU 0

static void
x86_init_fpu ( void )
{
#if defined(mingw32_HOST_OS) && defined(x86_64_HOST_ARCH) && !X86_INIT_FPU
    /* Mingw-w64 does a stupid thing. They set the FPU precision to extended mode by default.
    The reasoning is that it's for compatibility with GNU Linux ported libraries. However the
    problem is this is incompatible with the standard Windows double precision mode.  In fact,
    if we create a new OS thread then Windows will reset the FPU to double precision mode.
    So we end up with a weird state where the main thread by default has a different precision
    than any child threads. */
    fesetenv(FE_PC53_ENV);
#elif X86_INIT_FPU
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
#else
    return;
#endif
}

#if defined(mingw32_HOST_OS)
/* And now we have to override the build in ones in Mingw-W64's CRT. */
void _fpreset(void)
{
    x86_init_fpu();
}

#if defined(__GNUC__)
void __attribute__((alias("_fpreset"))) fpreset(void);
#else
void fpreset(void) {
    _fpreset();
}
#endif

/* Set the console's CodePage to UTF-8 if using the new I/O manager and the CP
   is still the default one.  */
static void
initConsoleCP (void)
{
    /* Set the initial codepage to automatic.  */
    __codePage = -1;

    /* Check if the codepage is still the system default ANSI codepage.  */
    if (GetConsoleCP () == GetOEMCP ()
        && GetConsoleOutputCP () == GetOEMCP ()) {
      if (!SetConsoleCP (CP_UTF8) || !SetConsoleOutputCP (CP_UTF8))
        errorBelch ("Unable to set console CodePage, Unicode output may be "
                    "garbled.\n");
      else
        IF_DEBUG (scheduler, debugBelch ("Codepage set to UTF-8.\n"));

      /* Assign the codepage so we can restore it on exit.  */
      __codePage = (int64_t)GetOEMCP ();
    }
}

/* Restore the CodePage to what it was before we started.  If the CodePage was
   already set then this call is a no-op.  */
void
hs_restoreConsoleCP (void)
{
    /* If we set the CP at startup, we should set it on exit.  */
    if (__codePage == -1)
      return;

    UINT cp = (UINT)__codePage;
    __codePage = -1;
    if (SetConsoleCP (cp) && SetConsoleOutputCP (cp)) {
      IF_DEBUG (scheduler, debugBelch ("Codepage restored to OEM.\n"));
    } else {
      IF_DEBUG (scheduler, debugBelch ("Unable to restore CodePage to OEM.\n"));
    }
}
#endif

/* -----------------------------------------------------------------------------
   Starting up the RTS
   -------------------------------------------------------------------------- */

static void initBuiltinGcRoots(void)
{
    /* Add some GC roots for things in the base package that the RTS
     * knows about.  We don't know whether these turn out to be CAFs
     * or refer to CAFs, but we have to assume that they might.
     *
     * Because these stable pointers will retain any CAF references in
     * these closures `Id`s of these can be safely marked as non-CAFFY
     * in the compiler.
     */
    getStablePtr((StgPtr)runIO_closure);
    getStablePtr((StgPtr)runNonIO_closure);
    getStablePtr((StgPtr)flushStdHandles_closure);

    getStablePtr((StgPtr)runFinalizerBatch_closure);

    getStablePtr((StgPtr)stackOverflow_closure);
    getStablePtr((StgPtr)heapOverflow_closure);
    getStablePtr((StgPtr)unpackCString_closure);
    getStablePtr((StgPtr)blockedIndefinitelyOnMVar_closure);
    getStablePtr((StgPtr)nonTermination_closure);
    getStablePtr((StgPtr)blockedIndefinitelyOnSTM_closure);
    getStablePtr((StgPtr)allocationLimitExceeded_closure);
    getStablePtr((StgPtr)cannotCompactFunction_closure);
    getStablePtr((StgPtr)cannotCompactPinned_closure);
    getStablePtr((StgPtr)cannotCompactMutable_closure);
    getStablePtr((StgPtr)nestedAtomically_closure);
    getStablePtr((StgPtr)underflowException_closure);
    getStablePtr((StgPtr)overflowException_closure);
    getStablePtr((StgPtr)divZeroException_closure);
    getStablePtr((StgPtr)runSparks_closure);
    getStablePtr((StgPtr)ensureIOManagerIsRunning_closure);
    getStablePtr((StgPtr)interruptIOManager_closure);
    getStablePtr((StgPtr)ioManagerCapabilitiesChanged_closure);
#if !defined(mingw32_HOST_OS)
    getStablePtr((StgPtr)runHandlersPtr_closure);
#else
    getStablePtr((StgPtr)processRemoteCompletion_closure);
#endif

    /*
     * See Note [Wired-in exceptions are not CAFfy] in GHC.Core.Make.
     * These are precisely the functions for which we construct `Id`s using
     * GHC.Core.Make.mkExceptionId.
     */
    getStablePtr((StgPtr)absentSumFieldError_closure);
    getStablePtr((StgPtr)runAllocationLimitHandler_closure);
}

void
hs_init(int *argc, char **argv[])
{
    hs_init_ghc(argc, argv, defaultRtsConfig);
}

void
hs_init_with_rtsopts(int *argc, char **argv[])
{
    RtsConfig rts_opts = defaultRtsConfig; /* by value */
    rts_opts.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(argc, argv, rts_opts);
}

void
hs_init_ghc(int *argc, char **argv[], RtsConfig rts_config)
{
    // N.B. atomic_inc returns the new value.
    StgWord init_count = atomic_inc(&hs_init_count, 1);
    if (init_count > 1) {
        // second and subsequent inits are ignored
        return;
    }
    if (rts_shutdown) {
        errorBelch("hs_init_ghc: reinitializing the RTS after shutdown is not currently supported");
        stg_exit(1);
    }

#if defined(wasm32_HOST_ARCH)
    char *pwd = getenv("PWD");
    if (pwd != NULL) {
        int chdir_result = chdir(pwd);
        if (chdir_result != 0) {
            errorBelch("hs_init_ghc: chdir(%s) failed with %d", pwd, chdir_result);
            stg_exit(1);
        }
    }
#endif

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
    rts_config.defaultsHook();

    /* Whether to GC CAFs */
    if (rts_config.keep_cafs) {
        setKeepCAFs();
    }

    /* Parse the flags, separating the RTS flags from the programs args */
    if (argc == NULL || argv == NULL) {
        // Use a default for argc & argv if either is not supplied
        int my_argc = 1;
        #if defined(mingw32_HOST_OS)
        //Retry larger buffer sizes on error up to about the NTFS length limit.
        wchar_t* pathBuf;
        char *my_argv[2] = { NULL, NULL };
        for(DWORD maxLength = MAX_PATH; maxLength <= 33280; maxLength *= 2)
        {
            pathBuf = (wchar_t*) stgMallocBytes(sizeof(wchar_t) * maxLength,
                "hs_init_ghc: GetModuleFileName");
            DWORD pathLength = GetModuleFileNameW(NULL, pathBuf, maxLength);
            if(GetLastError() == ERROR_INSUFFICIENT_BUFFER || pathLength == 0) {
                stgFree(pathBuf);
                pathBuf = NULL;
            } else {
                break;
            }
        }
        if(pathBuf == NULL) {
            my_argv[0] = "<unknown>";
        } else {
            my_argv[0] = lpcwstrToUTF8(pathBuf);
            stgFree(pathBuf);
        }


        #else
        char *my_argv[] = { "<unknown>", NULL };
        #endif
        setFullProgArgv(my_argc,my_argv);
        setupRtsFlags(&my_argc, my_argv, rts_config);
    } else {
        setFullProgArgv(*argc,*argv);
        setupRtsFlags(argc, *argv, rts_config);

#if defined(DEBUG)
        /* load debugging symbols for current binary */
        DEBUG_LoadSymbols((*argv)[0]);
#endif /* DEBUG */
    }

    /* Based on the RTS flags, decide which I/O manager to use. */
    selectIOManager();

    /* Set the supported level of vector registers */
    setVectorSupport();

    /* Initialize console Codepage.  */
#if defined(mingw32_HOST_OS)
   if (is_io_mng_native_p())
      initConsoleCP();
#endif

    /* Initialise the adjustors subsystem */
    initAdjustors();

    /* Initialise mmapForLinker */
    initLinkerMMap();

    /* Initialise the stats department, phase 1 */
    initStats1();

    /* initTracing must be after setupRtsFlags() */
#if defined(TRACING)
    initTracing();
#endif

    /* Initialise libdw session pool */
    libdwPoolInit();

    /* Start the "ticker" and profiling timer but don't start until the
     * scheduler is up. However, the ticker itself needs to be initialized
     * before the scheduler to ensure that the ticker mutex is initialized as
     * moreCapabilities will attempt to acquire it.
     */
    initTimer();

    /* initialise scheduler data structures (needs to be done before
     * initStorage()).
     */
    initScheduler();

    /* Trace some basic information about the process */
    traceInitEvent(traceWallClockTime);
    traceInitEvent(traceOSProcessInfo);
    flushTrace();

    /* initialize the storage manager */
    initStorage();

    /* initialise the stable pointer table */
    initStablePtrTable();

    /* initialise the stable name table */
    initStableNameTable();

    /* create StablePtrs for builtin GC roots*/
    initBuiltinGcRoots();

    /*
     * process any foreign exports which were registered while loading the
     * image
     * */
    processForeignExports();

    /* initialize the top-level handler system */
    initTopHandler();

    /* initialise the shared Typeable store */
    initGlobalStore();

    /* initialise file locking, if necessary */
    initFileLocking();

#if defined(PROFILING)
    initProfiling();
#endif
    initIpe();
    traceInitEvent(dumpIPEToEventLog);
    initHeapProfiling();

    /* start the virtual timer 'subsystem'. */
    startTimer();

#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        /* Initialise the user signal handler set */
        initUserSignals();
        /* Set up handler to run on SIGINT, etc. */
        initDefaultHandlers();
    }
#endif

    initIOManager();

    x86_init_fpu();

    startupHpc();

    /* Record initialization times */
    stat_endInit();
}

// Compatibility interface
void
startupHaskell(int argc, char *argv[], void (*init_root)(void) STG_UNUSED)
{
    hs_init(&argc, &argv);
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
hs_exit_(bool wait_foreign)
{
    uint32_t g, i;

    // N.B. atomic_dec returns the new value.
    StgInt init_count = (StgInt)atomic_dec(&hs_init_count, 1);
    if (init_count > 0) {
        // ignore until it's the last one
        return;
    }
    if (init_count < 0) {
        errorBelch("warning: too many hs_exit()s");
        return;
    }

    rts_shutdown = true;

    /* start timing the shutdown */
    stat_startExit();

    rtsConfig.onExitHook();

    flushStdHandles();

    // sanity check
#if defined(DEBUG)
    checkFPUStack();
#endif

    stopIOManager();

    /* stop all running tasks. This is also where we stop concurrent non-moving
     * collection if it's running */
    exitScheduler(wait_foreign);

    /* run C finalizers for all active weak pointers */
    for (i = 0; i < getNumCapabilities(); i++) {
        runAllCFinalizers(getCapability(i)->weak_ptr_list_hd);
    }
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        runAllCFinalizers(generations[g].weak_ptr_list);
    }
    runAllCFinalizers(nonmoving_weak_ptr_list);

#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        freeSignalHandlers();
    }
#endif

    /* stop the ticker */
    stopTimer();
    /*
     * it is quite important that we wait here as some timer implementations
     * (e.g. pthread) may fire even after we exit, which may segfault as we've
     * already freed the capabilities.
     */
    exitTimer(true);

    /*
     * Dump the ticky counter definitions
     * We do this at the end of execution since tickers are registered in the
     * course of program execution.
     */
#if defined(TICKY_TICKY) && defined(TRACING)
    if (RtsFlags.TraceFlags.ticky) {
        emitTickyCounterDefs();
    }
#endif

    // set the terminal settings back to what they were
#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
    resetTerminalSettings();
#endif

#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        // uninstall signal handlers
        resetDefaultHandlers();
    }
#endif

    /* stop timing the shutdown, we're about to print stats */
    stat_endExit();

    /* shutdown the hpc support (if needed) */
    exitHpc();

    // clean up things from the storage manager's point of view.
    // also outputs the stats (+RTS -s) info.
    exitStorage();

    /* flush and clean up capabilities' eventlog buffers before cleaning up
     * scheduler */
    finishCapEventLogging();

    /* free the tasks */
    freeScheduler();

    /* free shared Typeable store */
    exitGlobalStore();

    /* free linker data */
    exitLinker();

    /* free file locking tables, if necessary */
    freeFileLocking();

    /* free the Static Pointer Table */
    exitStaticPtrTable();

    /* remove the top-level handler */
    exitTopHandler();

    /* free the stable pointer table */
    exitStablePtrTable();

    /* free the stable name table */
    exitStableNameTable();

#if defined(PROFILING)
    reportCCSProfiling();
#endif

    endHeapProfiling();
    freeHeapProfiling();

#if defined(PROFILING)
    endProfiling();
    freeProfiling();
#endif

#if defined(PROFILING)
    // Originally, this was in report_ccs_profiling().  Now, retainer
    // profiling might tack some extra stuff on to the end of this file
    // during endProfiling().
    if (prof_file != NULL) fclose(prof_file);
#endif

#if defined(TRACING)
    endTracing();
    freeTracing();
#endif

#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();

    FILE *tf = RtsFlags.TickyFlags.tickyFile;
    if (tf != NULL) fclose(tf);
#endif

    exitIOManager(wait_foreign);

    /* Restore the console Codepage.  */
#if defined(mingw32_HOST_OS)
   if (is_io_mng_native_p())
      hs_restoreConsoleCP();

   /* Disable console signal handlers, we're going down!.  */
   finiUserSignals ();
#endif

    /* tear down statistics subsystem */
    stat_exit();

    // Finally, free all our storage.  However, we only free the heap
    // memory if we have waited for foreign calls to complete;
    // otherwise a foreign call in progress may still be referencing
    // heap memory (e.g. by being passed a ByteArray#).
    freeStorage(wait_foreign);

    // Free the various argvs
    freeRtsArgs();

#if !defined(wasm32_HOST_ARCH)
    // Free threading resources
    freeThreadingResources();
#endif

    exitIpe();
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
    hs_exit_(true);
    // be safe; this might be a DLL
}

void
hs_exit_nowait(void)
{
    hs_exit_(false);
    // do not wait for outstanding foreign calls to return; if they return in
    // the future, they will block indefinitely.
}

// Compatibility interfaces
void
shutdownHaskell(void)
{
    hs_exit();
}

void
shutdownHaskellAndExit(int n, int fastExit)
{
    if (!fastExit) {
        // we're about to exit(), no need to wait for foreign calls to return.
        hs_exit_(false);
    }

    stg_exit(n);
}

#if !defined(mingw32_HOST_OS) && defined(HAVE_SIGNAL_H)
static void exitBySignal(int sig) STG_NORETURN;

void
shutdownHaskellAndSignal(int sig, int fastExit)
{
    if (!fastExit) {
        hs_exit_(false);
    }

    exitBySignal(sig);
}

void
exitBySignal(int sig)
{
    // We're trying to kill ourselves with a given signal.
    // That's easier said that done because:
    //  - signals can be ignored have handlers set for them
    //  - signals can be masked
    //  - signals default action can do things other than terminate:
    //    + can do nothing
    //    + can do weirder things: stop/continue the process

    struct sigaction dfl;
    sigset_t sigset;

    // So first of all, we reset the signal to use the default action.
    (void)sigemptyset(&dfl.sa_mask);
    dfl.sa_flags = 0;
    dfl.sa_handler = SIG_DFL;
    (void)sigaction(sig, &dfl, NULL);

    // Then we unblock the signal so we can deliver it to ourselves
    sigemptyset(&sigset);
    sigaddset(&sigset, sig);
    sigprocmask(SIG_UNBLOCK, &sigset, NULL);

    switch (sig) {
      case SIGSTOP: case SIGTSTP: case SIGTTIN: case SIGTTOU: case SIGCONT:
        // These signals stop (or continue) the process, so are no good for
        // exiting.
        exit(0xff);

      default:
        kill(getpid(),sig);
        // But it's possible the signal is one where the default action is to
        // ignore, in which case we'll still be alive... so just exit.
        exit(0xff);
    }
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
