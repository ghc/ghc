/* -----------------------------------------------------------------------------
 * $Id: RtsFlags.h,v 1.25 2000/04/19 12:42:48 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Datatypes that holds the command-line flag settings.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSFLAGS_H
#define RTSFLAGS_H

/* For defaults, see the @initRtsFlagsDefaults@ routine. */

struct GC_FLAGS {
    FILE   *statsFile;
    nat	    giveStats;
#define NO_GC_STATS	 0
#define SUMMARY_GC_STATS 1
#define VERBOSE_GC_STATS 2

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

    rtsBool ringBell;

    rtsBool squeezeUpdFrames;
};

/* Hack: this struct uses bitfields so that we can use a binary arg
 * with the -D flag.
 * Remember to update the corresponding bit of RtsFlags.c if you
 * change/extend this struct.
 */
struct DEBUG_FLAGS {  
  /* flags to control debugging output in various subsystems */
  rtsBool scheduler   : 1; /*  1 */
  rtsBool evaluator   : 1; /*  2 */
  rtsBool codegen     : 1; /*  4 */
  rtsBool weak        : 1; /*  8 */
  rtsBool gccafs      : 1; /* 16 */
  rtsBool gc          : 1; /* 32 */
  rtsBool block_alloc : 1; /* 64 */

  /* flags to control consistency checking (often very expensive!) */
  rtsBool sanity      : 1; /* 128 */

  rtsBool stable      : 1; /* 256 */
  rtsBool prof        : 1; /* 512 */
  rtsBool gran        : 1; /* 1024 */
  rtsBool par         : 1; /* 2048 */
};

#define MAX_DEBUG_OPTION     11
#define DEBUG_MASK(n)        ((nat)(ldexp(1,n)))
#define MAX_DEBUG_MASK       ((nat)(ldexp(1,(MAX_DEBUG_OPTION+1))-1))

#if defined(PROFILING) || defined(PAR)
    /* with PROFILING, full cost-centre stuff (also PROFILING_FLAGS);
       with PAR, just the four fixed cost-centres.
    */
struct COST_CENTRE_FLAGS {
    unsigned int	    doCostCentres;
# define COST_CENTRES_SUMMARY	1
# define COST_CENTRES_VERBOSE	2 /* incl. serial time profile */
# define COST_CENTRES_ALL	3
# define COST_CENTRES_XML       4

    int	    ctxtSwitchTicks; /* derived */
    int	    profilerTicks;   /* derived */
    int	    msecsPerTick;    /* derived */
};
#endif

#ifdef PROFILING
struct PROFILING_FLAGS {
    unsigned int	doHeapProfile;

# define NO_HEAP_PROFILING	0	/* N.B. Used as indexes into arrays */
# define HEAP_BY_CCS		1
# define HEAP_BY_MOD		2
# define HEAP_BY_DESCR		4
# define HEAP_BY_TYPE		5
# define HEAP_BY_TIME		6

    rtsBool		showCCSOnException;
  
# define CCchar    'C'
# define MODchar   'M'
# define DESCRchar 'D'
# define TYPEchar  'Y'
# define TIMEchar  'T'
};
#elif defined(DEBUG)
# define NO_HEAP_PROFILING	0
# define HEAP_BY_INFOPTR        1
# define HEAP_BY_CLOSURE_TYPE   2
struct PROFILING_FLAGS {
    unsigned int      doHeapProfile; /* heap profile using symbol table */
};
#endif /* DEBUG || PROFILING */

struct CONCURRENT_FLAGS {
  int ctxtSwitchTime; /* in milliseconds */
};

#ifdef PAR
/* currently the same as GRAN_STATS_FLAGS */
struct PAR_STATS_FLAGS {
  rtsBool Full;       /* Full .gr profile (rtsTrue) or only END events? */
  // rtsBool Suppressed; /* No .gr profile at all */
  rtsBool Binary;     /* Binary profile? (not yet implemented) */
  rtsBool Sparks;     /* Info on sparks in profile? */
  rtsBool Heap;       /* Info on heap allocs in profile? */ 
  rtsBool NewLogfile; /* Use new log-file format? (not yet implemented) */
  rtsBool Global;     /* Global statistics? (printed on shutdown; no log file) */
};

struct PAR_DEBUG_FLAGS {  
  /* flags to control debugging output in various subsystems */
  rtsBool verbose    : 1; /*    1 */
  rtsBool trace      : 1; /*    2 */
  rtsBool schedule   : 1; /*    4 */
  rtsBool free       : 1; /*    8 */
  rtsBool resume     : 1; /*   16 */
  rtsBool weight     : 1; /*   32 */
  rtsBool fetch      : 1; /*   64 */
  rtsBool fish       : 1; /*  128 */
  rtsBool tables     : 1; /*  256 */
  rtsBool packet     : 1; /*  512 */
  rtsBool pack       : 1; /* 1024 */
};

#define MAX_PAR_DEBUG_OPTION     10
#define PAR_DEBUG_MASK(n)        ((nat)(ldexp(1,n)))
#define MAX_PAR_DEBUG_MASK       ((nat)(ldexp(1,(MAX_PAR_DEBUG_OPTION+1))-1))

struct PAR_FLAGS {
  struct PAR_STATS_FLAGS ParStats;  /* profile and stats output */
  struct PAR_DEBUG_FLAGS Debug;         /* debugging options */
  rtsBool  outputDisabled;	  /* Disable output for performance purposes */
  nat      packBufferSize;
  nat	   maxLocalSparks;        /* spark pool size */
  nat      maxThreads;            /* thread pool size */
  nat      maxFishes;             /* max number of active fishes */
  rtsTime  fishDelay;             /* delay before sending a new fish */
  long   wait;
};
#endif /* PAR */

#ifdef SMP
struct PAR_FLAGS {
  nat            nNodes;         /* number of threads to run simultaneously */
  unsigned int	 maxLocalSparks;
};
#endif /* SMP */

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

  nat  maxThreads;              // ToDo: share with SMP and GUM
  // rtsBool labelling;
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

  // unsigned int	    debug;
  //  rtsBool event_trace;
  //  rtsBool event_trace_all;
};
#endif /* GRAN */

#ifdef TICKY_TICKY
struct TICKY_FLAGS {
    rtsBool showTickyStats;
    FILE   *tickyFile;
};
#endif /* TICKY_TICKY */


/* Put them together: */

struct RTS_FLAGS {
    struct GC_FLAGS	GcFlags;
    struct DEBUG_FLAGS	DebugFlags;
    struct CONCURRENT_FLAGS ConcFlags;

#if defined(PROFILING) || defined(PAR)
    struct COST_CENTRE_FLAGS CcFlags;
#endif
#if defined(PROFILING) || defined(DEBUG)
    struct PROFILING_FLAGS ProfFlags;
#endif
#if defined(SMP) || defined(PAR)
    struct PAR_FLAGS	ParFlags;
#endif
#ifdef GRAN
    struct GRAN_FLAGS	GranFlags;
#endif
#ifdef TICKY_TICKY
    struct TICKY_FLAGS	TickyFlags;
#endif
};

#ifdef COMPILING_RTS_MAIN
extern DLLIMPORT struct RTS_FLAGS RtsFlags;
#else
extern struct RTS_FLAGS RtsFlags;
#endif

/* Routines that operate-on/to-do-with RTS flags: */

void initRtsFlagsDefaults(void);
void setupRtsFlags(int *argc, char *argv[], int *rts_argc, char *rts_argv[]);

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
