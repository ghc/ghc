/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Datatypes that holds the command-line flag settings.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "stg/Types.h"
#include "Time.h"

/* For defaults, see the @initRtsFlagsDefaults@ routine. */

/* Note [Synchronization of flags and base APIs]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * We provide accessors to RTS flags in base. (GHC.RTS module)
 * The API should be updated whenever RTS flags are modified.
 */

/* See Note [Synchronization of flags and base APIs] */
typedef struct _GC_FLAGS {
    FILE   *statsFile;
    uint32_t  giveStats;
#define NO_GC_STATS         0
#define COLLECT_GC_STATS 1
#define ONELINE_GC_STATS 2
#define SUMMARY_GC_STATS 3
#define VERBOSE_GC_STATS 4

    uint32_t     maxStkSize;         /* in *words* */
    uint32_t     initialStkSize;     /* in *words* */
    uint32_t     stkChunkSize;       /* in *words* */
    uint32_t     stkChunkBufferSize; /* in *words* */

    uint32_t     maxHeapSize;        /* in *blocks* */
    uint32_t     minAllocAreaSize;   /* in *blocks* */
    uint32_t     largeAllocLim;      /* in *blocks* */
    uint32_t     nurseryChunkSize;   /* in *blocks* */
    uint32_t     minOldGenSize;      /* in *blocks* */
    uint32_t     heapSizeSuggestion; /* in *blocks* */
    bool heapSizeSuggestionAuto;
    double  oldGenFactor;
    double  returnDecayFactor;
    double  pcFreeHeap;

    bool         useNonmoving; // default = false
    uint16_t     nonmovingDenseAllocatorCount; // Amount of dense nonmoving allocators. See Note [Allocator sizes]
    uint32_t     generations;
    bool squeezeUpdFrames;

    bool compact;               /* True <=> "compact all the time" */
    double  compactThreshold;

    bool sweep;                 /* use "mostly mark-sweep" instead of copying
                                 * for the oldest generation */
    bool ringBell;

    Time    idleGCDelayTime;    /* units: TIME_RESOLUTION */
    Time    interIdleGCWait;    /* units: TIME_RESOLUTION */
    bool doIdleGC;

    Time    longGCSync;         /* units: TIME_RESOLUTION */

    StgWord heapBase;           /* address to ask the OS for memory */

    StgWord allocLimitGrace;    /* units: *blocks*
                                 * After an AllocationLimitExceeded
                                 * exception has been raised, how much
                                 * extra space is given to the thread
                                 * to handle the exception before we
                                 * raise it again.
                                 */
    StgWord heapLimitGrace;     /* units: *blocks*
                                 * After a HeapOverflow exception has
                                 * been raised, how much extra space is
                                 * given to the thread to handle the
                                 * exception before we raise it again.
                                 */

    bool numa;                   /* Use NUMA */
    StgWord numaMask;

    StgWord64 addressSpaceSize;  /* large address space size in bytes */
} GC_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _DEBUG_FLAGS {
    /* flags to control debugging output & extra checking in various subsystems */
    bool scheduler;      /* 's' */
    bool interpreter;    /* 'i' */
    bool weak;           /* 'w' */
    bool gccafs;         /* 'G' */
    bool gc;             /* 'g' */
    bool nonmoving_gc;   /* 'n' */
    bool block_alloc;    /* 'b' */
    bool sanity;         /* 'S'   warning: might be expensive! */
    bool zero_on_gc;     /* 'Z' */
    bool stable;         /* 't' */
    bool prof;           /* 'p' */
    bool linker;         /* 'l'   the object linker, output which scales with O(# objects) or errors */
    bool linker_verbose; /* 'L'   the object linker, output which scales with O(# symbols) */
    bool apply;          /* 'a' */
    bool stm;            /* 'm' */
    bool squeeze;        /* 'z'  stack squeezing & lazy blackholing */
    bool hpc;            /* 'c' coverage */
    bool sparks;         /* 'r' */
    bool numa;           /* '--debug-numa' */
    bool compact;        /* 'C' */
    bool continuation;   /* 'k' */
    bool iomanager;      /* 'o' */
} DEBUG_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _COST_CENTRE_FLAGS {
    uint32_t    doCostCentres;
# define COST_CENTRES_NONE      0
# define COST_CENTRES_SUMMARY   1
# define COST_CENTRES_VERBOSE   2 /* incl. serial time profile */
# define COST_CENTRES_ALL       3
# define COST_CENTRES_JSON      4

    int profilerTicks;   /* derived */
    int msecsPerTick;    /* derived */
    char const *outputFileNameStem;
} COST_CENTRE_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _PROFILING_FLAGS {
    uint32_t doHeapProfile;
# define NO_HEAP_PROFILING      0 /* N.B. Used as indexes into arrays */
# define HEAP_BY_CCS            1
# define HEAP_BY_MOD            2
# define HEAP_BY_DESCR          4
# define HEAP_BY_TYPE           5
# define HEAP_BY_RETAINER       6
# define HEAP_BY_LDV            7

# define HEAP_BY_CLOSURE_TYPE   8
# define HEAP_BY_INFO_TABLE     9
# define HEAP_BY_ERA            10

    Time        heapProfileInterval; /* time between samples */
    uint32_t    heapProfileIntervalTicks; /* ticks between samples (derived) */
    bool        startHeapProfileAtStartup; /* true if we start profiling from program startup */
    bool        startTimeProfileAtStartup; /* true if we start profiling from program startup */
    bool        incrementUserEra;


    bool        showCCSOnException;

    uint32_t    maxRetainerSetSize;

    uint32_t    ccsLength;

    const char*         modSelector;
    const char*         descrSelector;
    const char*         typeSelector;
    const char*         ccSelector;
    const char*         ccsSelector;
    const char*         retainerSelector;
    StgWord             eraSelector;
    const char*         bioSelector;

} PROFILING_FLAGS;

#define TRACE_NONE      0
#define TRACE_EVENTLOG  1
#define TRACE_STDERR    2

/* See Note [Synchronization of flags and base APIs] */
typedef struct _TRACE_FLAGS {
    int tracing;
    bool timestamp;      /* show timestamp in stderr output */
    bool scheduler;      /* trace scheduler events */
    bool gc;             /* trace GC events */
    bool nonmoving_gc;   /* trace nonmoving GC events */
    bool sparks_sampled; /* trace spark events by a sampled method */
    bool sparks_full;    /* trace spark events 100% accurately */
    bool ticky;          /* trace ticky-ticky samples */
    bool user;           /* trace user events (emitted from Haskell code) */
    Time eventlogFlushTime;  /* Time between force eventlog flushes (or 0 if disabled) */
    int eventlogFlushTicks;
    char *trace_output;  /* output filename for eventlog */
    bool nullWriter; /* use null writer instead of file writer */
} TRACE_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _CONCURRENT_FLAGS {
    Time ctxtSwitchTime;         /* units: TIME_RESOLUTION */
    int ctxtSwitchTicks;         /* derived */
} CONCURRENT_FLAGS;

/*
 * The tickInterval is the time interval between "ticks", ie.
 * timer signals (see Timer.{c,h}).  It is the frequency at
 * which we sample CCCS for profiling.
 *
 * It is changed by the +RTS -V<secs> flag.
 */

// Note [No timer on wasm32]
// ~~~~~~~~~~~~~~~~~~~~~~~~~
// Due to the lack of threads and preemption semantics, on wasm32
// there can't be a background timer that periodically resets the
// context switch flag. So it makes sense to make the scheduler
// default to -C0 on wasm32 for better fairness and avoid starving
// threads.
#if defined(wasm32_HOST_ARCH)
#define DEFAULT_TICK_INTERVAL USToTime(0)
#else
#define DEFAULT_TICK_INTERVAL USToTime(10000)
#endif

/*
 * When linkerAlwaysPic is true, the runtime linker assume that all object
 * files were compiled with -fPIC -fexternal-dynamic-refs and load them
 * anywhere in the address space.
 * Note that there is no 32bit darwin system we can realistically expect to
 * run on or compile for.
 */
#if defined(darwin_HOST_OS) || defined(aarch64_HOST_ARCH) || defined(arm_HOST_ARCH)
#define DEFAULT_LINKER_ALWAYS_PIC true
#else
#define DEFAULT_LINKER_ALWAYS_PIC false
#endif

/* Which I/O Manager to use in the target program. */
typedef enum _IO_MANAGER_FLAG {

    /* Select an I/O manager automatically. This will pick the one determined
     * at configure time, for the RTS way. This can also fall back to other
     * available I/O managers if the first choice cannot be initialised,
     * if platform support turns out to be unavailable (e.g. too old a kernel).
     */
    IO_MNGR_FLAG_AUTO,

    /* All other choices pick only the requested one, with no fallback. */
    IO_MNGR_FLAG_SELECT,          /* Unix only,    non-threaded RTS only */
    IO_MNGR_FLAG_MIO,             /* cross-platform,   threaded RTS only */
    IO_MNGR_FLAG_WINIO,           /* Windows only                        */
    IO_MNGR_FLAG_WIN32_LEGACY,    /* Windows only, non-threaded RTS only */
  } IO_MANAGER_FLAG;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _MISC_FLAGS {
    Time    tickInterval;        /* units: TIME_RESOLUTION */
    bool install_signal_handlers;
    bool install_seh_handlers;
    bool generate_dump_file;
    bool generate_stack_trace;
    bool machineReadable;
    bool disableDelayedOsMemoryReturn; /* See Note [MADV_FREE and MADV_DONTNEED].
                                          It's in `MiscFlags` instead of
                                          `GcFlags` because if GHC used madvise()
                                          memory management for non-GC related
                                          tasks in the future, we'd respect it
                                          there as well. */
    bool internalCounters;       /* See Note [Internal Counters Stats] */
    bool linkerAlwaysPic;        /* Assume the object code is always PIC */
    StgWord linkerMemBase;       /* address to ask the OS for memory
                                  * for the linker, NULL ==> off */
    IO_MANAGER_FLAG ioManager;   /* The I/O manager to use.  */
    uint32_t numIoWorkerThreads; /* Number of I/O worker threads to use.  */
} MISC_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _PAR_FLAGS {
  uint32_t       nCapabilities;  /* number of threads to run simultaneously */
  bool           migrate;        /* migrate threads between capabilities */
  uint32_t       maxLocalSparks;
  bool           parGcEnabled;   /* enable parallel GC */
  uint32_t       parGcGen;       /* do parallel GC in this generation
                                  * and higher only */
  bool           parGcLoadBalancingEnabled;
                                 /* enable load-balancing in the
                                  * parallel GC */
  uint32_t       parGcLoadBalancingGen;
                                 /* do load-balancing in this
                                  * generation and higher only */

  uint32_t       parGcNoSyncWithIdle;
                                 /* if a Capability has been idle for
                                  * this many GCs, do not try to wake
                                  * it up when doing a
                                  * non-load-balancing parallel GC.
                                  * (zero disables) */

  uint32_t       parGcThreads;
                                 /* Use this many threads for parallel
                                  * GC (default: use all nNodes). */

  bool           setAffinity;    /* force thread affinity with CPUs */
} PAR_FLAGS;

/* Corresponds to the RTS flag `--read-tix-file=<yes|no>`.
 * The accepted GHC proposal 612 introduced a one-release warning period
 * during which we emit a warning if we read a .tix file and the flag
 * isn't explicitly set. In order to distinguish between whether the flag
 * was explicitly set or defaulted we need to use a tri-state variable.
 */
typedef enum _HPC_READ_FILE {
    HPC_NO_EXPLICIT = 0,  /* The user has specified --read-tix-file=no */
    HPC_YES_IMPLICIT = 1, /* The user hasn't specified an option and we emit
                           * a warning when we read a tix file.
                           */
    HPC_YES_EXPLICIT = 2  /* The user has specified --read-tix-file=yes */
  } HPC_READ_FILE;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _HPC_FLAGS {
  bool           writeTixFile;   /* Whether the RTS should write a tix
                                    file at the end of execution */
  HPC_READ_FILE  readTixFile;    /* Whether the RTS should read a tix
                                    file at the beginning of execution */
} HPC_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _TICKY_FLAGS {
    bool showTickyStats;
    FILE   *tickyFile;
} TICKY_FLAGS;

/* Put them together: */

/* See Note [Synchronization of flags and base APIs] */
typedef struct _RTS_FLAGS {
    /* The first portion of RTS_FLAGS is invariant. */
    GC_FLAGS          GcFlags;
    CONCURRENT_FLAGS  ConcFlags;
    MISC_FLAGS        MiscFlags;
    DEBUG_FLAGS       DebugFlags;
    COST_CENTRE_FLAGS CcFlags;
    PROFILING_FLAGS   ProfFlags;
    TRACE_FLAGS       TraceFlags;
    TICKY_FLAGS       TickyFlags;
    PAR_FLAGS         ParFlags;
    HPC_FLAGS         HpcFlags;
} RTS_FLAGS;

#if defined(COMPILING_RTS_MAIN)
extern DLLIMPORT RTS_FLAGS RtsFlags;
#elif IN_STG_CODE
/* Note [RtsFlags is a pointer in STG code]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * When compiling with IN_STG_CODE the RtsFlags symbol is defined as a pointer.
 * This is necessary because the C code generator can't generate '&label'.
 */
extern RTS_FLAGS RtsFlags[];
#else
extern RTS_FLAGS RtsFlags;
#endif

/*
 * The printf formats are here, so we are less likely to make
 * overly-long filenames (with disastrous results).  No more than 128
 * chars, please!
 */

#define STATS_FILENAME_MAXLEN 128

#define GR_FILENAME_FMT         "%0.124s.gr"
#define HP_FILENAME_FMT         "%0.124s.hp"
#define LIFE_FILENAME_FMT       "%0.122s.life"
#define PROF_FILENAME_FMT       "%0.122s.prof"
#define PROF_FILENAME_FMT_GUM   "%0.118s.%03d.prof"
#define QP_FILENAME_FMT         "%0.124s.qp"
#define STAT_FILENAME_FMT       "%0.122s.stat"
#define TICKY_FILENAME_FMT      "%0.121s.ticky"
#define TIME_FILENAME_FMT       "%0.122s.time"
#define TIME_FILENAME_FMT_GUM   "%0.118s.%03d.time"

/* an "int" so as to match normal "argc" */
/* Now defined in Stg.h (lib/std/cbits need these too.)
extern int     prog_argc;
extern char  **prog_argv;
*/
extern int      rts_argc;  /* ditto */
extern char   **rts_argv;
