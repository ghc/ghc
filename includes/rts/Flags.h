/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Datatypes that holds the command-line flag settings.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
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
 *
 * We provide accessors to RTS flags in base. (GHC.RTS module)
 * The API should be updated whenever RTS flags are modified.
 */

/* See Note [Synchronization of flags and base APIs] */
typedef struct _GC_FLAGS {
    FILE   *statsFile;
    uint32_t  giveStats;
#define NO_GC_STATS	 0
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
    double  pcFreeHeap;

    uint32_t     generations;
    bool squeezeUpdFrames;

    bool compact;		/* True <=> "compact all the time" */
    double  compactThreshold;

    bool sweep;		/* use "mostly mark-sweep" instead of copying
                                 * for the oldest generation */
    bool ringBell;

    Time    idleGCDelayTime;    /* units: TIME_RESOLUTION */
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
} GC_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _DEBUG_FLAGS {
    /* flags to control debugging output & extra checking in various subsystems */
    bool scheduler;      /* 's' */
    bool interpreter;    /* 'i' */
    bool weak;           /* 'w' */
    bool gccafs;         /* 'G' */
    bool gc;             /* 'g' */
    bool block_alloc;    /* 'b' */
    bool sanity;         /* 'S'   warning: might be expensive! */
    bool stable;         /* 't' */
    bool prof;           /* 'p' */
    bool linker;         /* 'l'   the object linker */
    bool apply;          /* 'a' */
    bool stm;            /* 'm' */
    bool squeeze;        /* 'z'  stack squeezing & lazy blackholing */
    bool hpc;            /* 'c' coverage */
    bool sparks;         /* 'r' */
    bool numa;           /* '--debug-numa' */
    bool compact;        /* 'C' */
} DEBUG_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _COST_CENTRE_FLAGS {
    uint32_t    doCostCentres;
# define COST_CENTRES_NONE      0
# define COST_CENTRES_SUMMARY	1
# define COST_CENTRES_VERBOSE	2 /* incl. serial time profile */
# define COST_CENTRES_ALL	3
# define COST_CENTRES_JSON      4

    int	    profilerTicks;   /* derived */
    int	    msecsPerTick;    /* derived */
    char const *outputFileNameStem;
} COST_CENTRE_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _PROFILING_FLAGS {
    uint32_t doHeapProfile;
# define NO_HEAP_PROFILING	0	/* N.B. Used as indexes into arrays */
# define HEAP_BY_CCS		1
# define HEAP_BY_MOD		2
# define HEAP_BY_DESCR		4
# define HEAP_BY_TYPE		5
# define HEAP_BY_RETAINER       6
# define HEAP_BY_LDV            7

# define HEAP_BY_CLOSURE_TYPE   8

    Time        heapProfileInterval; /* time between samples */
    uint32_t    heapProfileIntervalTicks; /* ticks between samples (derived) */
    bool        includeTSOs;


    bool		showCCSOnException;

    uint32_t    maxRetainerSetSize;

    uint32_t    ccsLength;

    const char*         modSelector;
    const char*         descrSelector;
    const char*         typeSelector;
    const char*         ccSelector;
    const char*         ccsSelector;
    const char*         retainerSelector;
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
    bool sparks_sampled; /* trace spark events by a sampled method */
    bool sparks_full;    /* trace spark events 100% accurately */
    bool user;           /* trace user events (emitted from Haskell code) */
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
#define DEFAULT_TICK_INTERVAL USToTime(10000)

/* See Note [Synchronization of flags and base APIs] */
typedef struct _MISC_FLAGS {
    Time    tickInterval;        /* units: TIME_RESOLUTION */
    bool install_signal_handlers;
    bool install_seh_handlers;
    bool generate_dump_file;
    bool generate_stack_trace;
    bool machineReadable;
    bool internalCounters;       /* See Note [Internal Counter Stats] */
    StgWord linkerMemBase;       /* address to ask the OS for memory
                                  * for the linker, NULL ==> off */
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

/* See Note [Synchronization of flags and base APIs] */
typedef struct _TICKY_FLAGS {
    bool showTickyStats;
    FILE   *tickyFile;
} TICKY_FLAGS;

/* Put them together: */

/* See Note [Synchronization of flags and base APIs] */
typedef struct _RTS_FLAGS {
    /* The first portion of RTS_FLAGS is invariant. */
    GC_FLAGS	      GcFlags;
    CONCURRENT_FLAGS  ConcFlags;
    MISC_FLAGS        MiscFlags;
    DEBUG_FLAGS	      DebugFlags;
    COST_CENTRE_FLAGS CcFlags;
    PROFILING_FLAGS   ProfFlags;
    TRACE_FLAGS       TraceFlags;
    TICKY_FLAGS	      TickyFlags;
    PAR_FLAGS	      ParFlags;
} RTS_FLAGS;

#if defined(COMPILING_RTS_MAIN)
extern DLLIMPORT RTS_FLAGS RtsFlags;
#elif IN_STG_CODE
/* Hack because the C code generator can't generate '&label'. */
extern RTS_FLAGS RtsFlags[];
#else
extern RTS_FLAGS RtsFlags;
#endif

/*
 * The printf formats are here, so we are less likely to make
 * overly-long filenames (with disastrous results).  No more than 128
 * chars, please!
 */

#define STATS_FILENAME_MAXLEN	128

#define GR_FILENAME_FMT		"%0.124s.gr"
#define HP_FILENAME_FMT		"%0.124s.hp"
#define LIFE_FILENAME_FMT	"%0.122s.life"
#define PROF_FILENAME_FMT	"%0.122s.prof"
#define PROF_FILENAME_FMT_GUM	"%0.118s.%03d.prof"
#define QP_FILENAME_FMT		"%0.124s.qp"
#define STAT_FILENAME_FMT	"%0.122s.stat"
#define TICKY_FILENAME_FMT	"%0.121s.ticky"
#define TIME_FILENAME_FMT	"%0.122s.time"
#define TIME_FILENAME_FMT_GUM	"%0.118s.%03d.time"

/* an "int" so as to match normal "argc" */
/* Now defined in Stg.h (lib/std/cbits need these too.)
extern int     prog_argc;
extern char  **prog_argv;
*/
extern int      rts_argc;  /* ditto */
extern char   **rts_argv;
