/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Datatypes that holds the command-line flag settings.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSFLAGS_H
#define RTSFLAGS_H

#include <stdio.h>

/* For defaults, see the @initRtsFlagsDefaults@ routine. */

struct GC_FLAGS {
    FILE   *statsFile;
    nat	    giveStats;
#define NO_GC_STATS	 0
#define COLLECT_GC_STATS 1
#define ONELINE_GC_STATS 2
#define SUMMARY_GC_STATS 3
#define VERBOSE_GC_STATS 4

    nat     maxStkSize;         /* in *words* */
    nat     initialStkSize;     /* in *words* */

    nat	    maxHeapSize;        /* in *blocks* */
    nat     minAllocAreaSize;   /* in *blocks* */
    nat     minOldGenSize;      /* in *blocks* */
    nat     heapSizeSuggestion; /* in *blocks* */
    double  oldGenFactor;
    double  pcFreeHeap;

    nat     generations;
    nat     steps;
    rtsBool squeezeUpdFrames;

    rtsBool compact;		/* True <=> "compact all the time" */
    double  compactThreshold;

    rtsBool ringBell;
    rtsBool frontpanel;

    int idleGCDelayTime;	/* in milliseconds */
};

struct DEBUG_FLAGS {  
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
    rtsBool gran;           /* 'r' */
    rtsBool par;            /* 'P' */
    rtsBool linker;         /* 'l'   the object linker */
    rtsBool apply;          /* 'a' */
    rtsBool stm;            /* 'm' */
    rtsBool squeeze;        /* 'z'  stack squeezing & lazy blackholing */
    rtsBool hpc; 	    /* 'c' coverage */
};

struct COST_CENTRE_FLAGS {
    unsigned int	    doCostCentres;
# define COST_CENTRES_SUMMARY	1
# define COST_CENTRES_VERBOSE	2 /* incl. serial time profile */
# define COST_CENTRES_ALL	3
# define COST_CENTRES_XML       4

    int	    profilerTicks;   /* derived */
    int	    msecsPerTick;    /* derived */
};

struct PROFILING_FLAGS {
    unsigned int	doHeapProfile;
# define NO_HEAP_PROFILING	0	/* N.B. Used as indexes into arrays */
# define HEAP_BY_CCS		1
# define HEAP_BY_MOD		2
# define HEAP_BY_DESCR		4
# define HEAP_BY_TYPE		5
# define HEAP_BY_RETAINER       6
# define HEAP_BY_LDV            7

# define HEAP_BY_INFOPTR        1      /* DEBUG only */
# define HEAP_BY_CLOSURE_TYPE   2      /* DEBUG only */

    nat                 profileInterval;      /* delta between samples (in ms) */
    nat                 profileIntervalTicks; /* delta between samples (in 'ticks') */
    rtsBool             includeTSOs;


    rtsBool		showCCSOnException;

    nat                 maxRetainerSetSize;

    nat                 ccsLength;

    char*               modSelector;
    char*               descrSelector;
    char*               typeSelector;
    char*               ccSelector;
    char*               ccsSelector;
    char*               retainerSelector;
    char*               bioSelector;

};

struct CONCURRENT_FLAGS {
    int ctxtSwitchTime;		/* in milliseconds */
    int ctxtSwitchTicks;	/* derived */
};

struct MISC_FLAGS {
    int tickInterval;     /* in milliseconds */
};

#ifdef PAR
/* currently the same as GRAN_STATS_FLAGS */
struct PAR_STATS_FLAGS {
  rtsBool Full;       /* Full .gr profile (rtsTrue) or only END events? */
  rtsBool Suppressed; /* No .gr profile at all */
  rtsBool Binary;     /* Binary profile? (not yet implemented) */
  rtsBool Sparks;     /* Info on sparks in profile? */
  rtsBool Heap;       /* Info on heap allocs in profile? */ 
  rtsBool NewLogfile; /* Use new log-file format? (not yet implemented) */
  rtsBool Global;     /* Global statistics? (printed on shutdown; no log file) */
};

struct PAR_DEBUG_FLAGS {  
  /* flags to control debugging output in various subsystems */
  rtsBool verbose    : 1; /*    1 */
  rtsBool bq         : 1; /*    2 */
  rtsBool schedule   : 1; /*    4 */
  rtsBool free       : 1; /*    8 */
  rtsBool resume     : 1; /*   16 */
  rtsBool weight     : 1; /*   32 */
  rtsBool fetch      : 1; /*   64 */
  rtsBool fish       : 1; /*  128 */
  rtsBool tables     : 1; /*  256 */
  rtsBool packet     : 1; /*  512 */
  rtsBool pack       : 1; /* 1024 */
  rtsBool paranoia   : 1; /* 2048 */
};

#define MAX_PAR_DEBUG_OPTION     11
#define PAR_DEBUG_MASK(n)        ((nat)(ldexp(1,n)))
#define MAX_PAR_DEBUG_MASK       ((nat)(ldexp(1,(MAX_PAR_DEBUG_OPTION+1))-1))

struct PAR_FLAGS {
  struct PAR_STATS_FLAGS ParStats;  /* profile and stats output */
  struct PAR_DEBUG_FLAGS Debug;         /* debugging options */
  rtsBool  outputDisabled;	  /* Disable output for performance purposes */
  rtsBool  doFairScheduling;	  /* Fair-ish scheduling (round robin; no time-slices) */
  nat      packBufferSize;
  nat      thunksToPack;          /* number of thunks in packet + 1 */ 
  nat      globalising;           /* globalisation scheme */
  nat	   maxLocalSparks;        /* spark pool size */
  nat      maxThreads;            /* thread pool size */
  nat      maxFishes;             /* max number of active fishes */
  rtsTime  fishDelay;             /* delay before sending a new fish */
  long   wait;
};
#endif /* PAR */

#ifdef THREADED_RTS
struct PAR_FLAGS {
  nat            nNodes;         /* number of threads to run simultaneously */
  rtsBool        migrate;        /* migrate threads between capabilities */
  rtsBool        wakeupMigrate;  /* migrate a thread on wakeup */
  unsigned int	 maxLocalSparks;
};
#endif /* THREADED_RTS */

#ifdef GRAN
struct GRAN_STATS_FLAGS {
  rtsBool Full;       /* Full .gr profile (rtsTrue) or only END events? */
  rtsBool Suppressed; /* No .gr profile at all */
  rtsBool Binary;     /* Binary profile? (not yet implemented) */
  rtsBool Sparks;     /* Info on sparks in profile? */
  rtsBool Heap;       /* Info on heap allocs in profile? */ 
  rtsBool NewLogfile; /* Use new log-file format? (not yet implemented) */
  rtsBool Global;     /* Global statistics? (printed on shutdown; no log file) */
};

struct GRAN_COST_FLAGS {
  /* Communication Cost Variables -- set in main program */
  nat latency;              /* Latency for single packet */
  nat additional_latency;   /* Latency for additional packets */
  nat fetchtime;            
  nat lunblocktime;         /* Time for local unblock */
  nat gunblocktime;         /* Time for global unblock */
  nat mpacktime;            /* Cost of creating a packet */     
  nat munpacktime;	    /* Cost of receiving a packet */    
  nat mtidytime;	    /* Cost of cleaning up after send */
  
  nat threadcreatetime;     /* Thread creation costs */
  nat threadqueuetime;      /* Cost of adding a thread to the running/runnable queue */
  nat threaddescheduletime; /* Cost of descheduling a thread */
  nat threadscheduletime;   /* Cost of scheduling a thread */
  nat threadcontextswitchtime;  /* Cost of context switch  */
  
  /* Instruction Costs */
  nat arith_cost;        /* arithmetic instructions (+,i,< etc) */
  nat branch_cost;       /* branch instructions */ 
  nat load_cost;         /* load into register */
  nat store_cost;        /* store into memory */
  nat float_cost;        /* floating point operations */
  
  nat heapalloc_cost;    /* heap allocation costs */
  
  /* Overhead for granularity control mechanisms */
  /* overhead per elem of spark queue */
  nat pri_spark_overhead;
  /* overhead per elem of thread queue */
  nat pri_sched_overhead;
};

struct GRAN_DEBUG_FLAGS {  
  /* flags to control debugging output in various subsystems */
  rtsBool event_trace    : 1; /*    1 */
  rtsBool event_stats    : 1; /*    2 */
  rtsBool bq             : 1; /*    4 */
  rtsBool pack           : 1; /*    8 */
  rtsBool checkSparkQ    : 1; /*   16 */
  rtsBool thunkStealing  : 1; /*   32 */
  rtsBool randomSteal  	 : 1; /*   64 */
  rtsBool findWork     	 : 1; /*  128 */
  rtsBool unused     	 : 1; /*  256 */
  rtsBool pri     	 : 1; /*  512 */
  rtsBool checkLight   	 : 1; /* 1024 */
  rtsBool sortedQ      	 : 1; /* 2048 */
  rtsBool blockOnFetch   : 1; /* 4096 */
  rtsBool packBuffer     : 1; /* 8192 */
  rtsBool blockOnFetch_sanity : 1; /*  16384 */
};

#define MAX_GRAN_DEBUG_OPTION     14
#define GRAN_DEBUG_MASK(n)        ((nat)(ldexp(1,n)))
#define MAX_GRAN_DEBUG_MASK       ((nat)(ldexp(1,(MAX_GRAN_DEBUG_OPTION+1))-1))

struct GRAN_FLAGS {
  struct GRAN_STATS_FLAGS GranSimStats;  /* profile and stats output */
  struct GRAN_COST_FLAGS Costs;          /* cost metric for simulation */
  struct GRAN_DEBUG_FLAGS Debug;         /* debugging options */

  nat  maxThreads;              /* ToDo: share with THREADED_RTS and GUM */
  /* rtsBool labelling; */
  nat  packBufferSize;
  nat  packBufferSize_internal;

  PEs proc;                     /* number of processors */
  rtsBool Fishing;              /* Simulate GUM style fishing mechanism? */
  nat maxFishes;                /* max number of spark or thread steals */
  rtsTime time_slice;           /* max time slice of one reduction thread */

    /* GrAnSim-Light: This version puts no bound on the number of
         processors but in exchange doesn't model communication costs
         (all communication is 0 cost). Mainly intended to show maximal
         degree of parallelism in the program (*not* to simulate the
         execution on a real machine). */
   
    rtsBool Light;

    rtsBool DoFairSchedule ;        /* fair scheduling alg? default: unfair */
    rtsBool DoAsyncFetch;           /* async. communication? */
    rtsBool DoStealThreadsFirst;    /* prefer threads over sparks when stealing */
  rtsBool DoAlwaysCreateThreads;  /* eager thread creation */
  rtsBool DoBulkFetching;         /* bulk fetching */
  rtsBool DoThreadMigration;      /* allow to move threads */
  nat     FetchStrategy;         /* what to do when waiting for data */
  rtsBool PreferSparksOfLocalNodes; /* prefer local over global sparks */
  rtsBool DoPrioritySparking;     /* sparks sorted by priorities */
  rtsBool DoPriorityScheduling;   /* threads sorted by priorities */
  nat     SparkPriority;         /* threshold for cut-off mechanism */
  nat     SparkPriority2;
  rtsBool RandomPriorities;
  rtsBool InversePriorities;
  rtsBool IgnorePriorities;
  nat     ThunksToPack;      /* number of thunks in packet + 1 */ 
  rtsBool RandomSteal;        /* steal spark/thread from random proc */
  rtsBool NoForward;        /* no forwarding of fetch messages */

  /* unsigned int	    debug; */
  /*  rtsBool event_trace; */
  /*  rtsBool event_trace_all; */
};
#endif /* GRAN */

struct TICKY_FLAGS {
    rtsBool showTickyStats;
    FILE   *tickyFile;
};

struct TRACE_FLAGS {
    rtsBool sched;		/* trace scheduler events for profiling */
    rtsBool timestamp;          /* add timestamps to traces */
};

/* Put them together: */

#ifdef USE_PAPI
struct PAPI_FLAGS {
    nat     eventType;          /* The type of events to count */
};

#define PAPI_FLAG_CACHE_L1 1
#define PAPI_FLAG_CACHE_L2 2
#define PAPI_FLAG_BRANCH 3
#define PAPI_FLAG_STALLS 4
#define PAPI_FLAG_CB_EVENTS 5

#endif

typedef struct _RTS_FLAGS {
    /* The first portion of RTS_FLAGS is invariant. */
    struct GC_FLAGS	     GcFlags;
    struct CONCURRENT_FLAGS  ConcFlags;
    struct MISC_FLAGS        MiscFlags;
    struct DEBUG_FLAGS	     DebugFlags;
    struct COST_CENTRE_FLAGS CcFlags;
    struct PROFILING_FLAGS   ProfFlags;
    struct TICKY_FLAGS	     TickyFlags;
    struct TRACE_FLAGS       TraceFlags;

#if defined(THREADED_RTS) || defined(PAR)
    struct PAR_FLAGS	ParFlags;
#endif
#ifdef GRAN
    struct GRAN_FLAGS	GranFlags;
#endif
#ifdef USE_PAPI
    struct PAPI_FLAGS   PapiFlags;
#endif
} RTS_FLAGS;

#ifdef COMPILING_RTS_MAIN
extern DLLIMPORT RTS_FLAGS RtsFlags;
#elif IN_STG_CODE
/* Hack because the C code generator can't generate '&label'. */
extern RTS_FLAGS RtsFlags[];
#else
extern RTS_FLAGS RtsFlags;
#endif

/* Routines that operate-on/to-do-with RTS flags: */

extern void initRtsFlagsDefaults(void);
extern void setupRtsFlags(int *argc, char *argv[], int *rts_argc, char *rts_argv[]);
extern void setProgName(char *argv[]);


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
extern int     rts_argc;  /* ditto */
extern char   *rts_argv[];

#endif	/* RTSFLAGS_H */
