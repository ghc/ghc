/* --------------------------------------------------------------------------
   Time-stamp: <Tue Mar 06 2001 00:18:30 Stardate: [-30]6285.06 hwloidl>

   Variables and functions specific to GranSim.
   ----------------------------------------------------------------------- */

#ifndef GRANSIM_RTS_H
#define GRANSIM_RTS_H

//@node Headers for GranSim objs used only in the RTS internally, , ,
//@section Headers for GranSim objs used only in the RTS internally

//@menu
//* Event queue::		
//* Spark handling routines::	
//* Processor related stuff::	
//* Local types::		
//* Statistics gathering::	
//* Prototypes::		
//@end menu
//*/ fool highlight

//@node Event queue, Spark handling routines, Headers for GranSim objs used only in the RTS internally, Headers for GranSim objs used only in the RTS internally
//@subsection Event queue

#if defined(GRAN) || defined(PAR)
/* Granularity event types for output (see DumpGranEvent) */
typedef enum GranEventType_ {
    GR_START = 0, GR_STARTQ, 
    GR_STEALING, GR_STOLEN, GR_STOLENQ, 
    GR_FETCH, GR_REPLY, GR_BLOCK, GR_RESUME, GR_RESUMEQ,
    GR_SCHEDULE, GR_DESCHEDULE,
    GR_END,
    SP_SPARK, SP_SPARKAT, SP_USED, SP_PRUNED, SP_EXPORTED, SP_ACQUIRED, SP_REQUESTED,
    GR_ALLOC,
    GR_TERMINATE,
    GR_SYSTEM_START, GR_SYSTEM_END,            /* only for debugging */
    GR_EVENT_MAX
} GranEventType;

extern char *gran_event_names[];
#endif

#if defined(GRAN)                                            /* whole file */

/* Event Types (internal use only) */
typedef enum rtsEventType_ {
 ContinueThread = 0,  /* Continue running the first thread in the queue */
 StartThread,         /* Start a newly created thread */
 ResumeThread,        /* Resume a previously running thread */
 MoveSpark,           /* Move a spark from one PE to another */
 MoveThread,          /* Move a thread from one PE to another */
 FindWork,            /* Search for work */
 FetchNode,           /* Fetch a node */
 FetchReply,          /* Receive a node */
 GlobalBlock,         /* Block a TSO on a remote node */
 UnblockThread        /* Make a TSO runnable */
} rtsEventType;

/* Number of last event type */
#define MAX_EVENT       9
 
typedef struct rtsEvent_ {
  PEs           proc;    /* Processor id */
  PEs           creator; /* Processor id of PE that created the event */
  rtsEventType  evttype; /* rtsEvent type */
  rtsTime       time;    /* Time at which event happened */
  StgTSO       *tso;     /* Associated TSO, if relevant */
  StgClosure   *node;    /* Associated node, if relevant */
  rtsSpark     *spark;   /* Associated SPARK, if relevant */
  StgInt        gc_info; /* Counter of heap objects to mark (used in GC only)*/
  struct rtsEvent_ *next;
  } rtsEvent;

typedef rtsEvent *rtsEventQ;

extern rtsEventQ EventHd;

/* Interface for ADT of Event Queue */
rtsEvent *get_next_event(void);
rtsTime   get_time_of_next_event(void);
void      insert_event(rtsEvent *newentry);
void      new_event(PEs proc, PEs creator, rtsTime time, 
		    rtsEventType evttype, StgTSO *tso, 
		    StgClosure *node, rtsSpark *spark);
void      print_event(rtsEvent *event);
void      print_eventq(rtsEvent *hd);
void      prepend_event(rtsEvent *event);
rtsEventQ grab_event(void);
void      prune_eventq(StgTSO *tso, StgClosure *node); 

void      traverse_eventq_for_gc(void);
void      markEventQueue(void);

//@node Spark handling routines, Processor related stuff, Event queue, Headers for GranSim objs used only in the RTS internally
//@subsection Spark handling routines

/* These functions are only used in the RTS internally; see GranSim.h for rest */
void 	  disposeSpark(rtsSpark *spark);
void 	  disposeSparkQ(rtsSparkQ spark);
void 	  print_spark(rtsSpark *spark);
void      print_sparkq(PEs proc);
void 	  print_sparkq_stats(void);
nat  	  spark_queue_len(PEs proc);
rtsSpark *delete_from_sparkq (rtsSpark *spark, PEs p, rtsBool dispose_too);
void      markSparkQueue(void);

//@node Processor related stuff, Local types, Spark handling routines, Headers for GranSim objs used only in the RTS internally
//@subsection Processor related stuff

typedef enum rtsProcStatus_ {
  Idle = 0,             /* empty threadq */
  Sparking,             /* non-empty sparkq; FINDWORK has been issued */
  Starting,             /* STARTTHREAD has been issue */
  Fetching,             /* waiting for remote data (only if block-on-fetch) */
  Fishing,              /* waiting for remote spark/thread */
  Busy                  /* non-empty threadq, with head of queue active */
} rtsProcStatus;

/*
#define IS_IDLE(proc)        (procStatus[proc] == Idle)
#define IS_SPARKING(proc)    (procStatus[proc] == Sparking)
#define IS_STARTING(proc)    (procStatus[proc] == Starting)
#define IS_FETCHING(proc)    (procStatus[proc] == Fetching)
#define IS_FISHING(proc)     (procStatus[proc] == Fishing)
#define IS_BUSY(proc)        (procStatus[proc] == Busy)    
#define ANY_IDLE             (any_idle())
#define MAKE_IDLE(proc)      procStatus[proc] = Idle
#define MAKE_SPARKING(proc)  procStatus[proc] = Sparking
#define MAKE_STARTING(proc)  procStatus[proc] = Starting
#define MAKE_FETCHING(proc)  procStatus[proc] = Fetching
#define MAKE_FISHING(proc)   procStatus[proc] = Fishing
#define MAKE_BUSY(proc)      procStatus[proc] = Busy
*/

//@node Local types, Statistics gathering, Processor related stuff, Headers for GranSim objs used only in the RTS internally
//@subsection Local types

/* Return codes of HandleFetchRequest:
    0 ... ok (FETCHREPLY event with a buffer containing addresses of the 
              nearby graph has been scheduled)
    1 ... node is already local (fetched by somebody else; no event is
                                  scheduled in here)
    2 ... fetch request has been forwrded to the PE that now contains the
           node
    3 ... node is a black hole (BH, BQ or RBH); no event is scheduled, and
           the current TSO is put into the blocking queue of that node
    4 ... out of heap in PackNearbyGraph; GC should be triggered in calling
          function to guarantee that the tso and node inputs are valid
          (they may be moved during GC).
   Return codes of blockFetch:
    0 ... ok; tso is now at beginning of BQ attached to the bh closure
    1 ... the bh closure is no BH any more; tso is immediately unblocked
*/

typedef enum rtsFetchReturnCode_ {
  Ok = 0,
  NodeIsLocal,
  NodeHasMoved,
  NodeIsBH,
  NodeIsNoBH,
  OutOfHeap,
} rtsFetchReturnCode;
  
//@node Statistics gathering, Prototypes, Local types, Headers for GranSim objs used only in the RTS internally
//@subsection Statistics gathering

extern unsigned int /* nat */ OutstandingFetches[], OutstandingFishes[];
extern rtsProcStatus procStatus[];
extern StgTSO *BlockedOnFetch[];

/* global structure for collecting statistics */
typedef struct GlobalGranStats_ {
  /* event stats */
  nat noOfEvents;
  nat event_counts[MAX_EVENT];

  /* communication stats */
  nat fetch_misses;
  nat tot_fake_fetches;   // GranSim internal; faked Fetches are a kludge!!
  nat tot_low_pri_sparks;

  /* load distribution statistics */  
  nat rs_sp_count, rs_t_count, ntimes_total, fl_total, 
      no_of_steals, no_of_migrates;

  /* spark queue stats */
  nat tot_sq_len, tot_sq_probes, tot_sparks;
  nat tot_add_threads, tot_tq_len, non_end_add_threads;

  /* packet statistics */
  nat tot_packets, tot_packet_size, tot_cuts, tot_thunks;

  /* thread stats */
  nat tot_threads_created, threads_created_on_PE[MAX_PROC],
      tot_TSOs_migrated;

  /* spark stats */
  nat pruned_sparks, withered_sparks;
  nat tot_sparks_created, sparks_created_on_PE[MAX_PROC];

  /* scheduling stats */
  nat tot_yields, tot_stackover, tot_heapover;

  /* blocking queue statistics */
  rtsTime tot_bq_processing_time;
  nat tot_bq_len, tot_bq_len_local, tot_awbq, tot_FMBQs;
} GlobalGranStats;

extern GlobalGranStats globalGranStats;

//@node Prototypes,  , Statistics gathering, Headers for GranSim objs used only in the RTS internally
//@subsection Prototypes

/* Generally useful fcts */
PEs where_is(StgClosure *node);
rtsBool is_unique(StgClosure *node);

/* Prototypes of event handling functions; needed in Schedule.c:ReSchedule() */
void do_the_globalblock (rtsEvent* event);
void do_the_unblock (rtsEvent* event);
void do_the_fetchnode (rtsEvent* event);
void do_the_fetchreply (rtsEvent* event);
void do_the_movethread (rtsEvent* event);
void do_the_movespark (rtsEvent* event);
void do_the_startthread(rtsEvent *event);
void do_the_findwork(rtsEvent* event);
void gimme_spark (rtsEvent *event, rtsBool *found_res, rtsSparkQ *spark_res);
rtsBool munch_spark (rtsEvent *event, rtsSparkQ spark);

/* GranSimLight routines */
void GranSimLight_enter_system(rtsEvent *event, StgTSO **ActiveTSOp);
void GranSimLight_leave_system(rtsEvent *event, StgTSO **ActiveTSOp);

/* Communication related routines */
rtsFetchReturnCode fetchNode(StgClosure* node, PEs from, PEs to);
rtsFetchReturnCode handleFetchRequest(StgClosure* node, PEs curr_proc, PEs p, StgTSO* tso);
void               handleIdlePEs(void);

long int random(void); /* used in stealSpark() and stealThread() in GranSim.c */

/* Scheduling fcts defined in GranSim.c */
void    insertThread(StgTSO *tso, PEs proc);
void    endThread(StgTSO *tso, PEs proc);
rtsBool GranSimLight_insertThread(StgTSO *tso, PEs proc);
nat     thread_queue_len(PEs proc);

/* For debugging */
rtsBool is_on_queue (StgTSO *tso, PEs proc);
#endif

#if defined(GRAN) || defined(PAR)
/* 
   Interface for dumping routines (i.e. writing to log file).
   These routines are shared with GUM (and could also be used for SMP).
*/
void DumpGranEvent(GranEventType name, StgTSO *tso);
void DumpEndEvent(PEs proc, StgTSO *tso, rtsBool mandatory_thread);
void DumpTSO(StgTSO *tso);
void DumpRawGranEvent(PEs proc, PEs p, GranEventType name, 
 	              StgTSO *tso, StgClosure *node, 
		      StgInt sparkname, StgInt len);
void DumpVeryRawGranEvent(rtsTime time, PEs proc, PEs p, GranEventType name,
			  StgTSO *tso, StgClosure *node, 
			  StgInt sparkname, StgInt len);
#endif

#endif /* GRANSIM_RTS_H  */
