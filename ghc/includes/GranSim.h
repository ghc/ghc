/*
  Headers for GranSim specific objects.
  
  Note that in GranSim we have one run-queue and blocking-queue for each
  processor. Therefore, this header file redefines variables like
  run_queue_hd to be relative to CurrentProc. The main arrays of runnable
  and blocking queues are defined in Schedule.c.  The important STG-called
  GranSim macros (e.g. for fetching nodes) are at the end of this
  file. Usually they are just wrappers to proper C functions in GranSim.c.  
*/

#ifndef GRANSIM_H
#define GRANSIM_H

#if !defined(GRAN)

/* Dummy definitions for basic GranSim macros called from STG land */
#define DO_GRAN_ALLOCATE(n)     			  /* nothing */
#define DO_GRAN_UNALLOCATE(n)   			  /* nothing */
#define DO_GRAN_FETCH(node)     			  /* nothing */
#define DO_GRAN_EXEC(arith,branch,load,store,floats)      /* nothing */
#define GRAN_FETCH_AND_RESCHEDULE(liveness_mask,reenter)  /* nothing */
#define GRAN_RESCHEDULE(liveness_mask,reenter)	          /* nothing */

#endif

#if defined(GRAN)  /* whole file */

extern StgTSO *CurrentTSO;

/*
 * @node Headers for GranSim specific objects, , ,
 * @section Headers for GranSim specific objects
 *
 * @menu
 * * Externs and prototypes::	
 * * Run and blocking queues::	
 * * Spark queues::		
 * * Processor related stuff::	
 * * GranSim costs::		
 * * STG called GranSim functions::  
 * * STG-called routines::	
 * @end menu
 *
 * @node Externs and prototypes, Run and blocking queues, Includes, Headers for GranSim specific objects
 * @subsection Externs and prototypes
 */

/* Global constants */
extern char *gran_event_names[];
extern char *proc_status_names[];
extern char *event_names[];

/* Vars checked from within STG land */
extern rtsBool  NeedToReSchedule, IgnoreEvents, IgnoreYields; 
; 
extern rtsTime  TimeOfNextEvent, TimeOfLastEvent, EndOfTimeSlice;

/* costs for basic operations (copied from RTS flags) */
extern nat gran_arith_cost, gran_branch_cost, gran_load_cost, gran_store_cost, gran_float_cost;

extern nat SparksAvail;     /* How many sparks are available */
extern nat SurplusThreads;  /* How many excess threads are there */
extern nat sparksIgnored, sparksCreated;

/*
 * @node Run and blocking queues, Spark queues, Externs and prototypes, Headers for GranSim specific objects
 * @subsection Run and blocking queues
 */

/* declared in Schedule.c */
extern StgTSO *run_queue_hds[], *run_queue_tls[];
extern StgTSO *blocked_queue_hds[], *blocked_queue_tls[];
extern StgTSO *ccalling_threadss[];

#define run_queue_hd         run_queue_hds[CurrentProc]
#define run_queue_tl         run_queue_tls[CurrentProc]
#define blocked_queue_hd     blocked_queue_hds[CurrentProc]
#define blocked_queue_tl     blocked_queue_tls[CurrentProc]
#define pending_sparks_hd    pending_sparks_hds[CurrentProc]
#define pending_sparks_tl    pending_sparks_tls[CurrentProc]
#define ccalling_threads     ccalling_threadss[CurrentProc]

/*
 * @node Spark queues, Processor related stuff, Run and blocking queues, Headers for GranSim specific objects
 * @subsection Spark queues
 */

/*
  In GranSim we use a double linked list to represent spark queues.
  
  This is more flexible, but slower, than the array of pointers
  representation used in GUM. We use the flexibility to define new fields in
  the rtsSpark structure, representing e.g. granularity info (see HWL's PhD
  thesis), or info about the parent of a spark.
*/

/* Sparks and spark queues */
typedef struct rtsSpark_
{
  StgClosure    *node;
  nat            name, global;
  nat            gran_info;      /* for granularity improvement mechanisms */
  PEs            creator;        /* PE that created this spark (unused) */
  struct rtsSpark_  *prev, *next;
} rtsSpark;
typedef rtsSpark *rtsSparkQ;

/* The spark queues, proper */
/* In GranSim this is a globally visible array of spark queues */
extern rtsSparkQ pending_sparks_hds[];
extern rtsSparkQ pending_sparks_tls[];

/* Prototypes of those spark routines visible to compiler generated .hc */
/* Routines only used inside the RTS are defined in rts/parallel GranSimRts.h */
rtsSpark    *newSpark(StgClosure *node, 
		      nat name, nat gran_info, nat size_info, 
		      nat par_info, nat local);
/* void         add_to_spark_queue(rtsSpark *spark); */

/*
 * @node Processor related stuff, GranSim costs, Spark queues, Headers for GranSim specific objects
 * @subsection Processor related stuff
 */

extern PEs CurrentProc;
extern rtsTime CurrentTime[];  

/* Maximum number of PEs that can be simulated */
#define MAX_PROC             32 /* (BITS_IN(StgWord))  */ /* ToDo: fix this!! */
/*
#if MAX_PROC==16 
#else 
#error MAX_PROC should be 32 on this architecture 
#endif
*/

/* #define CurrentTSO           CurrentTSOs[CurrentProc] */

/* Processor numbers to bitmasks and vice-versa */
#define MainProc	     0           /* Id of main processor */
#define NO_PRI               0           /* dummy priority */
#define MAX_PRI              10000       /* max possible priority */
#define MAIN_PRI             MAX_PRI     /* priority of main thread */ 

/* GrAnSim uses IdleProcs as bitmask to indicate which procs are idle */
#define PE_NUMBER(n)          (1l << (long)n)
#define ThisPE		      PE_NUMBER(CurrentProc)
#define MainPE		      PE_NUMBER(MainProc)
#define Everywhere	      (~0l)
#define Nowhere	              (0l)
#define Now                   CurrentTime[CurrentProc]

#define IS_LOCAL_TO(ga,proc)  ((1l << (PEs) proc) & ga)

#define GRAN_TIME_SLICE       1000        /* max time between 2 ReSchedules */

/*
 * @node GranSim costs, STG called GranSim functions, Processor related stuff, Headers for GranSim specific objects
 * @subsection GranSim costs
 */

/* Default constants for communication (see RtsFlags on how to change them) */

#define LATENCY		           1000	/* Latency for single packet */
#define ADDITIONAL_LATENCY	    100	/* Latency for additional packets */
#define BASICBLOCKTIME	    	     10
#define FETCHTIME	  	(LATENCY*2+MSGUNPACKTIME)
#define LOCALUNBLOCKTIME  	     10
#define GLOBALUNBLOCKTIME 	(LATENCY+MSGUNPACKTIME)

#define	MSGPACKTIME		     0  /* Cost of creating a packet */
#define	MSGUNPACKTIME		     0  /* Cost of receiving a packet */
#define MSGTIDYTIME                  0  /* Cost of cleaning up after send */

/* How much to increase GrAnSims internal packet size if an overflow 
   occurs.
   NB: This is a GrAnSim internal variable and is independent of the
   simulated packet buffer size.
*/

#define GRANSIM_DEFAULT_PACK_BUFFER_SIZE     400
#define REALLOC_SZ                           200

/* extern W_ gran_mpacktime, gran_mtidytime, gran_munpacktime; */

/* Thread cost model */
#define THREADCREATETIME	   (25+THREADSCHEDULETIME)
#define THREADQUEUETIME		    12  /* Cost of adding a thread to the running/runnable queue */
#define THREADDESCHEDULETIME	    75  /* Cost of descheduling a thread */
#define THREADSCHEDULETIME	    75  /* Cost of scheduling a thread */
#define THREADCONTEXTSWITCHTIME	    (THREADDESCHEDULETIME+THREADSCHEDULETIME)

/* Instruction Cost model (SPARC, including cache misses) */
#define ARITH_COST	     	   1
#define BRANCH_COST	     	   2
#define LOAD_COST	  	   4
#define STORE_COST	  	   4
#define FLOAT_COST		   1 /* ? */

#define HEAPALLOC_COST             11

#define PRI_SPARK_OVERHEAD    5
#define PRI_SCHED_OVERHEAD    5

/*
 * @node STG called GranSim functions, STG-called routines, GranSim costs, Headers for GranSim specific objects
 * @subsection STG called GranSim functions
 */

/* STG called GranSim functions */
void GranSimAllocate(StgInt n);
void GranSimUnallocate(StgInt n);
void GranSimExec(StgWord ariths, StgWord branches, StgWord loads, StgWord stores, StgWord floats);
StgInt GranSimFetch(StgClosure *node);
void GranSimSpark(StgInt local, StgClosure *node);
void GranSimSparkAt(rtsSpark *spark, StgClosure *where,StgInt identifier);
void GranSimSparkAtAbs(rtsSpark *spark, PEs proc, StgInt identifier);
void GranSimBlock(StgTSO *tso, PEs proc, StgClosure *node);


/*
 * @node STG-called routines,  , STG called GranSim functions, Headers for GranSim specific objects
 * @subsection STG-called routines
 */

/* Wrapped version of calls to GranSim-specific STG routines */

/*
#define DO_PERFORM_RESCHEDULE(liveness, always_reenter_node) PerformReschedule_wrapper(liveness, always_reenter_node)
*/
#define DO_GRAN_ALLOCATE(n)     STGCALL1(GranSimAllocate, n)
#define DO_GRAN_UNALLOCATE(n)   STGCALL1(GranSimUnallocate, n)
#define DO_GRAN_FETCH(node)     STGCALL1(GranSimFetch, node)
#define DO_GRAN_EXEC(arith,branch,load,store,floats) GranSimExec(arith,branch,load,store,floats)

/* 
   ToDo: Clean up this mess of GRAN macros!!! -- HWL
*/
/* DO_GRAN_FETCH((StgClosure*)R1.p); */
#define GRAN_FETCH()		/* nothing */

#define GRAN_FETCH_AND_RESCHEDULE(liveness,reenter)	\
          DO_GRAN_FETCH((StgClosure*)R1.p); 			        \
          DO_GRAN_YIELD(liveness,ENTRY_CODE((D_)(*R1.p))); 
/* RESTORE_EVERYTHING is done implicitly before entering threaded world again */

/*
  This is the only macro currently enabled;
  It should check whether it is time for the current thread to yield
  (e.g. if there is a more recent event in the queue) and it should check
  whether node is local, via a call to GranSimFetch.
  ToDo: split this in 2 routines:
         - GRAN_YIELD (as it is below)
	 - GRAN_FETCH (the rest of this macro)
        emit only these 2 macros based on node's liveness
	  node alive: emit both macros
	  node not alive: do only a GRAN_YIELD
	  
        replace gran_yield_? with gran_block_? (they really block the current
	thread)
*/
#define GRAN_RESCHEDULE(liveness,ptrs)  \
          if (RET_STGCALL1(StgInt, GranSimFetch, (StgClosure*)R1.p)) {\
            EXTFUN_RTS(gran_block_##ptrs); \
            JMP_(gran_block_##ptrs);       \
          } else {                         \
	    if (TimeOfLastEvent < CurrentTime[CurrentProc] && \
                HEAP_ALLOCED((StgClosure *)R1.p) && \
                LOOKS_LIKE_GHC_INFO(get_itbl((StgClosure *)R1.p))) { \
                                  EXTFUN_RTS(gran_yield_##ptrs); \
                                  JMP_(gran_yield_##ptrs); \
                } \
            /* GRAN_YIELD(ptrs)  */             \
	  }


/*                                                   YIELD(liveness,reenter) */

/* GRAN_YIELD(liveness_mask); */

/* GRAN_FETCH_AND_RESCHEDULE(liveness_mask,reenter) */

#define THREAD_CONTEXT_SWITCH(liveness_mask,reenter)	\
        do { \
	if (context_switch /* OR_INTERVAL_EXPIRED */) {	\
          GRAN_RESCHEDULE(liveness_mask,reenter); \
        } }while(0)

#define GRAN_EXEC(arith,branch,load,store,floats)       \
        { \
          W_ cost = gran_arith_cost*arith +   \
                    gran_branch_cost*branch + \
                    gran_load_cost*load +   \
                    gran_store_cost*store +   \
                    gran_float_cost*floats;   \
          CurrentTSO->gran.exectime += cost;                      \
          CurrentTime[CurrentProc] += cost;                      \
        }

/* In GranSim we first check whether there is an event to handle; only if
   this is the case (or the time slice is over in case of fair scheduling)
   we do a yield, which is very similar to that in the concurrent world 
   ToDo: check whether gran_yield_? can be merged with other yielding codes
*/

#define DO_GRAN_YIELD(ptrs)	if (!IgnoreYields && \
                                    TimeOfLastEvent < CurrentTime[CurrentProc] && \
				    HEAP_ALLOCED((StgClosure *)R1.p) && \
                                    LOOKS_LIKE_GHC_INFO(get_itbl((StgClosure *)R1.p))) { \
                                  EXTFUN_RTS(gran_yield_##ptrs); \
                                  JMP_(gran_yield_##ptrs); \
                                }

#define GRAN_YIELD(ptrs)                                   \
        {                                                   \
          extern int context_switch;                          \
          if ( (CurrentTime[CurrentProc]>=EndOfTimeSlice) ||   \
               ((CurrentTime[CurrentProc]>=TimeOfNextEvent) && \
	        (TimeOfNextEvent!=0) && !IgnoreEvents )) {     \
	    /* context_switch = 1; */                          \
            DO_GRAN_YIELD(ptrs);   \
	  }                                                    \
	}

#define ADD_TO_SPARK_QUEUE(spark)	      \
   STGCALL1(add_to_spark_queue,spark) \

#endif /* GRAN */

#endif /* GRANSIM_H */
