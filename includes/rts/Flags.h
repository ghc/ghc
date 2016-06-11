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

#ifndef RTS_FLAGS_H
#define RTS_FLAGS_H

#include <stdio.h>

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
    rtsBool heapSizeSuggestionAuto;
    double  oldGenFactor;
    double  pcFreeHeap;

    uint32_t     generations;
    rtsBool squeezeUpdFrames;

    rtsBool compact;		/* True <=> "compact all the time" */
    double  compactThreshold;

    rtsBool sweep;		/* use "mostly mark-sweep" instead of copying
                                 * for the oldest generation */
    rtsBool ringBell;

    Time    idleGCDelayTime;    /* units: TIME_RESOLUTION */
    rtsBool doIdleGC;

    StgWord heapBase;           /* address to ask the OS for memory */

    StgWord allocLimitGrace;    /* units: *blocks*
                                 * After an AllocationLimitExceeded
                                 * exception has been raised, how much
                                 * extra space is given to the thread
                                 * to handle the exception before we
                                 * raise it again.
                                 */

    rtsBool numa;               /* Use NUMA */
    StgWord numaMask;
} GC_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _DEBUG_FLAGS {
    /* flags to control debugging output & extra checking in various subsystems */
    rtsBool scheduler;      /* 's' */
    rtsBool interpreter;    /* 'i' */
    rtsBool weak;           /* 'w' */
    rtsBool gccafs;         /* 'G' */
    rtsBool gc;             /* 'g' */
    rtsBool block_alloc;    /* 'b' */
    rtsBool sanity;         /* 'S'   warning: might be expensive! */
    rtsBool stable;         /* 't' */
    rtsBool prof;           /* 'p' */
    rtsBool linker;         /* 'l'   the object linker */
    rtsBool apply;          /* 'a' */
    rtsBool stm;            /* 'm' */
    rtsBool squeeze;        /* 'z'  stack squeezing & lazy blackholing */
    rtsBool hpc; 	    /* 'c' coverage */
    rtsBool sparks; 	    /* 'r' */
    rtsBool numa; 	    /* '--debug-numa' */
} DEBUG_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _COST_CENTRE_FLAGS {
    uint32_t    doCostCentres;
# define COST_CENTRES_NONE      0
# define COST_CENTRES_SUMMARY	1
# define COST_CENTRES_VERBOSE	2 /* incl. serial time profile */
# define COST_CENTRES_ALL	3
# define COST_CENTRES_XML       4

    int	    profilerTicks;   /* derived */
    int	    msecsPerTick;    /* derived */
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
    rtsBool     includeTSOs;


    rtsBool		showCCSOnException;

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
    rtsBool timestamp;      /* show timestamp in stderr output */
    rtsBool scheduler;      /* trace scheduler events */
    rtsBool gc;             /* trace GC events */
    rtsBool sparks_sampled; /* trace spark events by a sampled method */
    rtsBool sparks_full;    /* trace spark events 100% accurately */
    rtsBool user;           /* trace user events (emitted from Haskell code) */
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
    rtsBool install_signal_handlers;
    rtsBool machineReadable;
    StgWord linkerMemBase;       /* address to ask the OS for memory
                                  * for the linker, NULL ==> off */
} MISC_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _PAR_FLAGS {
  uint32_t       nCapabilities;  /* number of threads to run simultaneously */
  rtsBool        migrate;        /* migrate threads between capabilities */
  uint32_t       maxLocalSparks;
  rtsBool        parGcEnabled;   /* enable parallel GC */
  uint32_t       parGcGen;       /* do parallel GC in this generation
                                  * and higher only */
  rtsBool        parGcLoadBalancingEnabled;
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

  rtsBool        setAffinity;    /* force thread affinity with CPUs */
} PAR_FLAGS;

/* See Note [Synchronization of flags and base APIs] */
typedef struct _TICKY_FLAGS {
    rtsBool showTickyStats;
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

#ifdef COMPILING_RTS_MAIN
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
#define GR_FILENAME_FMT_GUM	"%0.120s.%03d.%s"
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

#endif	/* RTS_FLAGS_H */
