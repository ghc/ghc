/* -----------------------------------------------------------------------------
 * $Id: RtsFlags.h,v 1.6 1999/01/26 11:12:46 simonm Exp $
 *
 * Datatypes that holds the command-line flag settings.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSFLAGS_H
#define RTSFLAGS_H

/* For defaults, see the @initRtsFlagsDefaults@ routine. */

struct GC_FLAGS {
    FILE   *statsFile;
    nat	    giveStats; /* ToDo: replace with enum type? */
#define NO_GC_STATS	 0
#define VERBOSE_GC_STATS 1

    nat     maxStkSize;         /* in *words* */
    nat     initialStkSize;     /* in *words* */

    nat	    maxHeapSize;        /* in *blocks* */
    nat     minAllocAreaSize;   /* in *blocks* */
    nat     minOldGenSize;      /* in *blocks* */
    double  oldGenFactor;
    double  pcFreeHeap;

    nat     generations;

    rtsBool forceGC; /* force a major GC every <interval> bytes */
    int	    forcingInterval; /* actually, stored as a number of *words* */
    rtsBool ringBell;

    rtsBool squeezeUpdFrames;
};

/* Hack: this struct uses bitfields so that we can use a binary arg
 * with the -D flag
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
};

#if defined(PROFILING) || defined(PAR)
    /* with PROFILING, full cost-centre stuff (also PROFILING_FLAGS);
       with PAR, just the four fixed cost-centres.
    */
struct COST_CENTRE_FLAGS {
    unsigned int	    doCostCentres;
# define COST_CENTRES_SUMMARY	1
# define COST_CENTRES_VERBOSE	2 /* incl. serial time profile */
# define COST_CENTRES_ALL	3

    char    sortBy;
# define SORTCC_LABEL	'C'
# define SORTCC_TIME 	'T'
# define SORTCC_ALLOC	'A'

    int	    ctxtSwitchTicks; /* derived */
    int	    profilerTicks;   /* derived */
    int	    msecsPerTick;    /* derived */
};
#endif

#ifdef PROFILING
struct PROFILING_FLAGS {
    unsigned int	doHeapProfile;
# define NO_HEAP_PROFILING	0	/* N.B. Used as indexes into arrays */
# define HEAP_BY_CC		1
# define HEAP_BY_MOD		2
# define HEAP_BY_GRP		3
# define HEAP_BY_DESCR		4
# define HEAP_BY_TYPE		5
# define HEAP_BY_TIME		6
  
# define CCchar    'C'
# define MODchar   'M'
# define GRPchar   'G'
# define DESCRchar 'D'
# define TYPEchar  'Y'
# define TIMEchar  'T'

    char *ccSelector;
    char *modSelector;
    char *grpSelector;
    char *descrSelector;
    char *typeSelector;
    char *kindSelector;
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
    int	    ctxtSwitchTime; /* in milliseconds */
    int	    maxThreads;
};

#ifdef PAR
struct PAR_FLAGS {
    rtsBool parallelStats; 	/* Gather parallel statistics */
    rtsBool granSimStats;	/* Full .gr profile (rtsTrue) or only END events? */
    rtsBool granSimStats_Binary;

    rtsBool outputDisabled;	/* Disable output for performance purposes */
    
    unsigned int	    packBufferSize;
    unsigned int	    maxLocalSparks;
};

#endif /* PAR */

#ifdef GRAN
struct GRAN_FLAGS {
    rtsBool granSimStats;  /* Full .gr profile (rtsTrue) or only END events? */
    rtsBool granSimStats_suppressed; /* No .gr profile at all */
    rtsBool granSimStats_Binary;
    rtsBool granSimStats_Sparks;
    rtsBool granSimStats_Heap;
    rtsBool labelling;
    unsigned int	    packBufferSize;
    unsigned int	    packBufferSize_internal;

    int proc;                      /* number of processors */
    int max_fishes;                /* max number of spark or thread steals */
    TIME time_slice;              /* max time slice of one reduction thread */

    /* Communication Cost Variables -- set in main program */
    unsigned int gran_latency;              /* Latency for single packet */
    unsigned int gran_additional_latency;   /* Latency for additional packets */
    unsigned int gran_fetchtime;            
    unsigned int gran_lunblocktime;         /* Time for local unblock */
    unsigned int gran_gunblocktime;         /* Time for global unblock */
    unsigned int gran_mpacktime;            /* Cost of creating a packet */     
    unsigned int gran_munpacktime;	  /* Cost of receiving a packet */    
    unsigned int gran_mtidytime;		  /* Cost of cleaning up after send */

    unsigned int gran_threadcreatetime;     /* Thread creation costs */
    unsigned int gran_threadqueuetime;      /* Cost of adding a thread to the running/runnable queue */
    unsigned int gran_threaddescheduletime; /* Cost of descheduling a thread */
    unsigned int gran_threadscheduletime;   /* Cost of scheduling a thread */
    unsigned int gran_threadcontextswitchtime;  /* Cost of context switch  */

    /* Instruction Costs */
    unsigned int gran_arith_cost;        /* arithmetic instructions (+,i,< etc) */
    unsigned int gran_branch_cost;       /* branch instructions */ 
    unsigned int gran_load_cost;         /* load into register */
    unsigned int gran_store_cost;        /* store into memory */
    unsigned int gran_float_cost;        /* floating point operations */

    unsigned int gran_heapalloc_cost;    /* heap allocation costs */

    /* Overhead for granularity control mechanisms */
    /* overhead per elem of spark queue */
    unsigned int gran_pri_spark_overhead;
    /* overhead per elem of thread queue */
    unsigned int gran_pri_sched_overhead;

    /* GrAnSim-Light: This version puts no bound on the number of
         processors but in exchange doesn't model communication costs
         (all communication is 0 cost). Mainly intended to show maximal
         degree of parallelism in the program (*not* to simulate the
         execution on a real machine). */
   
    rtsBool Light;

    rtsBool DoFairSchedule ;        /* fair scheduling alg? default: unfair */
    rtsBool DoReScheduleOnFetch ;   /* async. communication? */
    rtsBool DoStealThreadsFirst;    /* prefer threads over sparks when stealing */
    rtsBool SimplifiedFetch;        /* fast but inaccurate fetch modelling */
    rtsBool DoAlwaysCreateThreads;  /* eager thread creation */
    rtsBool DoGUMMFetching;         /* bulk fetching */
    rtsBool DoThreadMigration;      /* allow to move threads */
    int      FetchStrategy;          /* what to do when waiting for data */
    rtsBool PreferSparksOfLocalNodes; /* prefer local over global sparks */
    rtsBool DoPrioritySparking;     /* sparks sorted by priorities */
    rtsBool DoPriorityScheduling;   /* threads sorted by priorities */
    int      SparkPriority;          /* threshold for cut-off mechanism */
    int      SparkPriority2;
    rtsBool RandomPriorities;
    rtsBool InversePriorities;
    rtsBool IgnorePriorities;
    int      ThunksToPack;           /* number of thunks in packet + 1 */ 
    rtsBool RandomSteal;            /* steal spark/thread from random proc */
    rtsBool NoForward;              /* no forwarding of fetch messages */
    rtsBool PrintFetchMisses;       /* print number of fetch misses */

    unsigned int	    debug;
    rtsBool event_trace;
    rtsBool event_trace_all;
   
};
#endif /* GRAN */

#ifdef TICKY_TICKY
struct TICKY_FLAGS {
    rtsBool showTickyStats;
    FILE   *tickyFile;

    /* see also: doUpdEntryCounts in AllFlags */
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
#ifdef PAR
    struct PAR_FLAGS	ParFlags;
#endif
#ifdef GRAN
    struct GRAN_FLAGS	GranFlags;
#endif
#ifdef TICKY_TICKY
    struct TICKY_FLAGS	TickyFlags;
#endif
};

extern struct RTS_FLAGS RtsFlags;

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

extern int     prog_argc; /* an "int" so as to match normal "argc" */
extern char  **prog_argv;
extern int     rts_argc;  /* ditto */
extern char   *rts_argv[];

#endif	/* RTSFLAGS_H */
