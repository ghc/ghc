/*
  Definitions for GUM i.e. running on a parallel machine.

  This section contains definitions applicable only to programs compiled
  to run on a parallel machine, i.e. on GUM. Some of these definitions
  are also used when simulating parallel execution, i.e. on GranSim.
*/

#ifndef PARALLEL_H
#define PARALLEL_H

#if defined(PAR) || defined(GRAN)        /* whole file */

/*
 * @node Parallel definitions, End of File
 * @section Parallel definitions
 *
 * @menu
 * * Basic definitions::		
 * * GUM::			
 * * GranSim::			
 * @end menu
 *
 * @node Basic definitions, GUM, Parallel definitions, Parallel definitions
 * @subsection Basic definitions
 */

/* This clashes with TICKY, but currently TICKY and PAR hate each other anyway */
#define _HS  sizeofW(StgHeader)

/* SET_PAR_HDR and SET_STATIC_PAR_HDR now live in ClosureMacros.h */

/* Needed for dumping routines */
#if defined(PAR)
# define NODE_STR_LEN              20
# define TIME_STR_LEN              120
# define TIME                      rtsTime
# define CURRENT_TIME              (msTime() - startTime)
# define TIME_ON_PROC(p)           (msTime() - startTime)
# define CURRENT_PROC              thisPE
# define BINARY_STATS              RtsFlags.ParFlags.ParStats.Binary
#elif defined(GRAN)
# define NODE_STR_LEN              20
# define TIME_STR_LEN              120
# define TIME                      rtsTime
# define CURRENT_TIME              CurrentTime[CurrentProc]
# define TIME_ON_PROC(p)           CurrentTime[p]
# define CURRENT_PROC              CurrentProc
# define BINARY_STATS              RtsFlags.GranFlags.GranSimStats.Binary
#endif

#if defined(PAR)
#  define MAX_PES	256		/* Maximum number of processors */
	/* MAX_PES is enforced by SysMan, which does not
	   allow more than this many "processors".
	   This is important because PackGA [GlobAddr.lc]
	   **assumes** that a PE# can fit in 8+ bits.
	*/

# define SPARK_POOLS 	2   /* no. of spark pools */
# define REQUIRED_POOL 	0   /* idx of pool of mandatory sparks (concurrency) */
# define ADVISORY_POOL 	1   /* idx of pool of advisory sparks (parallelism) */
#endif

/*
 * @menu
 * * GUM::			
 * * GranSim::			
 * @end menu
 *
 * @node GUM, GranSim, Basic definitions, Parallel definitions
 * @subsection GUM
 */

#if defined(PAR) 
/*
  Symbolic constants for the packing code.
  
  This constant defines how many words of data we can pack into a single
  packet in the parallel (GUM) system.
*/

/*
 * @menu
 * * Types::			
 * * Externs::			
 * * Prototypes::		
 * * Macros::			
 * @end menu
 *
 * @node Types, Externs, GUM, GUM
 * @subsubsection Types
 */

/* Sparks and spark queues */
typedef StgClosure  *rtsSpark;
typedef rtsSpark    *rtsSparkQ;

typedef struct rtsPackBuffer_ {
  StgInt /* nat */           id; 
  StgInt /* nat */           size;
  StgInt /* nat */           unpacked_size;
  StgTSO       *tso;
  StgWord     *buffer[0];  
} rtsPackBuffer;

#define PACK_BUFFER_HDR_SIZE 4

/*
 * @node Externs, Prototypes, Types, GUM
 * @subsubsection Externs
 */

/* extern rtsBool do_sp_profile; */

extern globalAddr theGlobalFromGA, theGlobalToGA;
extern StgBlockedFetch *PendingFetches;
extern GlobalTaskId    *allPEs;

extern rtsBool      IAmMainThread, GlobalStopPending;
/*extern rtsBool      fishing; */
extern rtsTime      last_fish_arrived_at;
extern nat          outstandingFishes;
extern GlobalTaskId SysManTask;
extern int          seed;     /* pseudo-random-number generator seed: */
                              /* Initialised in ParInit */
extern StgInt       threadId; /* Number of Threads that have existed on a PE */
extern GlobalTaskId mytid;

extern GlobalTaskId *allPEs;
extern nat nPEs;
extern nat sparksIgnored, sparksCreated, threadsIgnored, threadsCreated;
extern nat advisory_thread_count;

extern rtsBool InGlobalGC;  /* Are we in the midst of performing global GC */

extern ullong startTime;    /* start of comp; in RtsStartup.c */

/* the spark pools proper */
extern rtsSpark *pending_sparks_hd[];  /* ptr to start of a spark pool */ 
extern rtsSpark *pending_sparks_tl[];  /* ptr to end of a spark pool */ 
extern rtsSpark *pending_sparks_lim[]; 
extern rtsSpark *pending_sparks_base[]; 
extern nat spark_limit[];

extern rtsPackBuffer *PackBuffer;      /* size: can be set via option */
extern rtsPackBuffer *buffer;
extern rtsPackBuffer *freeBuffer;
extern rtsPackBuffer *packBuffer;
extern rtsPackBuffer *gumPackBuffer;

extern nat thisPE;

/* From Global.c 
extern GALA *freeGALAList;
extern GALA *freeIndirections;
extern GALA *liveIndirections;
extern GALA *liveRemoteGAs;
*/

/*
 * @node Prototypes, Macros, Externs, GUM
 * @subsubsection Prototypes
 */

/* From ParInit.c */
void          initParallelSystem(void);
void          SynchroniseSystem(void);
void          par_exit(StgInt n);

PEs           taskIDtoPE (GlobalTaskId gtid);
void          registerTask (GlobalTaskId gtid);
globalAddr   *LAGAlookup (StgClosure *addr);
StgClosure   *GALAlookup (globalAddr *ga);
/*static GALA  *allocIndirection (StgPtr addr); */
globalAddr   *makeGlobal (StgClosure *addr, rtsBool preferred);
globalAddr   *setRemoteGA (StgClosure *addr, globalAddr *ga, rtsBool preferred);
void          splitWeight (globalAddr *to, globalAddr *from);
globalAddr   *addWeight (globalAddr *ga);
void          initGAtables (void);
void          RebuildLAGAtable (void);
StgWord       PackGA (StgWord pe, int slot);

# if defined(DEBUG)
/* from Global.c */
/* highest_slot breaks the abstraction of the slot counter for GAs; it is
   only used for sanity checking and should used nowhere else */
StgInt highest_slot (void); 
# endif

/*
 * @node Macros,  , Prototypes, GUM
 * @subsubsection Macros
 */

/* delay (in us) between dying fish returning and sending out a new fish */
#define FISH_DELAY                   1000
/* max no. of outstanding spark steals */
#define MAX_FISHES                   1  

/* ToDo: check which of these is actually needed! */

#    define PACK_HEAP_REQUIRED  ((RtsFlags.ParFlags.packBufferSize - PACK_HDR_SIZE) / (PACK_GA_SIZE + _HS) * (MIN_UPD_SIZE + 2))

#  define MAX_GAS 	(RtsFlags.ParFlags.packBufferSize / PACK_GA_SIZE)


#  define PACK_GA_SIZE	3	/* Size of a packed GA in words */
			        /* Size of a packed fetch-me in words */
#  define PACK_FETCHME_SIZE (PACK_GA_SIZE + _HS)

#  define PACK_HDR_SIZE	1	/* Words of header in a packet */

#  define PACK_PLC_SIZE	2	/* Size of a packed PLC in words */

/*
  Definitions relating to the entire parallel-only fixed-header field.

  On GUM, the global addresses for each local closure are stored in a
  separate hash table, rather then with the closure in the heap.  We call
  @getGA@ to look up the global address associated with a local closure (0
  is returned for local closures that have no global address), and @setGA@
  to store a new global address for a local closure which did not
  previously have one.  */

#  define GA_HDR_SIZE			0
  
#  define GA(closure)		        getGA(closure)
  
#  define SET_GA(closure, ga)             setGA(closure,ga)
#  define SET_STATIC_GA(closure)
#  define SET_GRAN_HDR(closure,pe)
#  define SET_STATIC_PROCS(closure)
  
#  define MAX_GA_WEIGHT			0	/* Treat as 2^n */
  
/* At the moment, there is no activity profiling for GUM.  This may change. */
#  define SET_TASK_ACTIVITY(act)        /* nothing */

/* 
   The following macros are only needed for sanity checking (see Sanity.c).
*/

/* NB: this is PVM specific and should be updated for MPI etc
       in PVM a task id (tid) is split into 2 parts: the id for the 
       physical processor it is running on and an index of tasks running
       on a processor; PVM_PE_MASK indicates which part of a tid holds the 
       id of the physical processor (the other part of the word holds the 
       index on that processor)
       MAX_PVM_PES and MAX_PVM_TIDS are maximal values for these 2 components
       in GUM we have an upper bound for the total number of PVM PEs allowed:
       it's MAX_PE defined in Parallel.h
       to check the slot field of a GA we call a fct highest_slot which just
       returns the internal counter 
*/
#define PVM_PE_MASK    0xfffc0000
#define MAX_PVM_PES    MAX_PES
#define MAX_PVM_TIDS   MAX_PES

#if 0
#define LOOKS_LIKE_TID(tid)  (((tid & PVM_PE_MASK) != 0) && \
                              (((tid & PVM_PE_MASK) + (tid & ~PVM_PE_MASK)) < MAX_PVM_TIDS))
#define LOOKS_LIKE_SLOT(slot) (slot<=highest_slot())

#define LOOKS_LIKE_GA(ga)    (LOOKS_LIKE_TID((ga)->payload.gc.gtid) && \
                             LOOKS_LIKE_SLOT((ga)->payload.gc.slot))
#else
rtsBool looks_like_tid(StgInt tid);
rtsBool looks_like_slot(StgInt slot);
rtsBool looks_like_ga(globalAddr *ga);
#define LOOKS_LIKE_TID(tid)  looks_like_tid(tid)
#define LOOKS_LIKE_GA(ga)    looks_like_ga(ga)
#endif /* 0 */

#endif /* PAR */

/*
 * @node GranSim,  , GUM, Parallel definitions
 * @subsection GranSim
 */

#if defined(GRAN)
/* ToDo: Check which of the PAR routines are needed in GranSim -- HWL */

/*
 * @menu
 * * Types::			
 * * Prototypes::		
 * * Macros::			
 * @end menu
 *
 * @node Types, Prototypes, GranSim, GranSim
 * @subsubsection Types
 */

typedef StgWord *StgBuffer;
typedef struct rtsPackBuffer_ {
  StgInt /* nat */           id;
  StgInt /* nat */           size;
  StgInt /* nat */           unpacked_size;
  StgTSO       *tso;
  StgWord      *buffer;  
} rtsPackBuffer;

/*
 * @node Macros,  , Prototypes, GranSim
 * @subsubsection Macros
 */

/* max no. of outstanding spark steals */
#define MAX_FISHES                   1  

/* These are needed in the packing code to get the size of the packet
   right. The closures itself are never built in GrAnSim. */
#  define FETCHME_VHS				IND_VHS
#  define FETCHME_HS				IND_HS
  
#  define FETCHME_GA_LOCN                       FETCHME_HS
  
#  define FETCHME_CLOSURE_SIZE(closure)		IND_CLOSURE_SIZE(closure)
#  define FETCHME_CLOSURE_NoPTRS(closure)		0L
#  define FETCHME_CLOSURE_NoNONPTRS(closure)	(IND_CLOSURE_SIZE(closure)-IND_VHS)
  
#  define MAX_GAS 	(RtsFlags.GranFlags.packBufferSize / PACK_GA_SIZE)
#  define PACK_GA_SIZE	3	/* Size of a packed GA in words */
			        /* Size of a packed fetch-me in words */
#  define PACK_FETCHME_SIZE (PACK_GA_SIZE + _HS)
#  define PACK_HDR_SIZE	4	/* Words of header in a packet */

#    define PACK_HEAP_REQUIRED  \
      (RtsFlags.GranFlags.packBufferSize * sizeofW(StgClosure*) + \
      2 * sizeofW(StgInt) + sizeofW(StgTSO*))

#    define PACK_FLAG_LOCN           0  
#    define PACK_TSO_LOCN            1
#    define PACK_UNPACKED_SIZE_LOCN  2
#    define PACK_SIZE_LOCN           3
#    define MAGIC_PACK_FLAG          0xfabc

#  define GA_HDR_SIZE			1

#  define PROCS_HDR_POSN		PAR_HDR_POSN
#  define PROCS_HDR_SIZE		1

/* Accessing components of the field */
#  define PROCS(closure)	        ((closure)->header.gran.procs)
/* SET_PROCS is now SET_GRAN_HEADER in ClosureMacros.h. */

#endif   /* GRAN */

/*
 * @node End of File,  , Parallel definitions
 * @section End of File
 */

#endif /* defined(PAR) || defined(GRAN)         whole file */

#endif /* Parallel_H */


