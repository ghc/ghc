/* 
   Time-stamp: <Tue Mar 06 2001 00:17:42 Stardate: [-30]6285.06 hwloidl>

   Variables and functions specific to GranSim the parallelism simulator
   for GPH.
*/

//@node GranSim specific code, , ,
//@section GranSim specific code

/*
   Macros for dealing with the new and improved GA field for simulating
   parallel execution. Based on @CONCURRENT@ package. The GA field now
   contains a mask, where the n-th bit stands for the n-th processor, where
   this data can be found. In case of multiple copies, several bits are
   set. The total number of processors is bounded by @MAX_PROC@, which
   should be <= the length of a word in bits.  -- HWL 
*/

//@menu
//* Includes::			
//* Prototypes and externs::	
//* Constants and Variables::	
//* Initialisation::		
//* Global Address Operations::	 
//* Global Event Queue::	
//* Spark queue functions::	
//* Scheduling functions::	
//* Thread Queue routines::	
//* GranSim functions::		
//* GranSimLight routines::	
//* Code for Fetching Nodes::	
//* Idle PEs::			
//* Routines directly called from Haskell world::  
//* Emiting profiling info for GrAnSim::  
//* Dumping routines::		
//* Index::			
//@end menu

//@node Includes, Prototypes and externs, GranSim specific code, GranSim specific code
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "StgMiscClosures.h"
#include "StgTypes.h"
#include "Schedule.h"
#include "SchedAPI.h"       // for pushClosure
#include "GranSimRts.h"
#include "GranSim.h"
#include "ParallelRts.h"
#include "ParallelDebug.h"
#include "Sparks.h"
#include "Storage.h"       // for recordMutable


//@node Prototypes and externs, Constants and Variables, Includes, GranSim specific code
//@subsection Prototypes and externs

#if defined(GRAN)

/* Prototypes */
static inline PEs      ga_to_proc(StgWord);
static inline rtsBool  any_idle(void);
static inline nat      idlers(void);
       PEs             where_is(StgClosure *node);

static rtsBool         stealSomething(PEs proc, rtsBool steal_spark, rtsBool steal_thread);
static rtsBool         stealSpark(PEs proc);
static rtsBool         stealThread(PEs proc);
static rtsBool         stealSparkMagic(PEs proc);
static rtsBool         stealThreadMagic(PEs proc);
/* subsumed by stealSomething
static void            stealThread(PEs proc); 
static void            stealSpark(PEs proc);
*/
static rtsTime         sparkStealTime(void);
static nat             natRandom(nat from, nat to);
static PEs             findRandomPE(PEs proc);
static void            sortPEsByTime (PEs proc, PEs *pes_by_time, 
				      nat *firstp, nat *np);

void GetRoots(void);

#endif /* GRAN */

//@node Constants and Variables, Initialisation, Prototypes and externs, GranSim specific code
//@subsection Constants and Variables

#if defined(GRAN) || defined(PAR)
/* See GranSim.h for the definition of the enum gran_event_types */
char *gran_event_names[] = {
    "START", "START(Q)",
    "STEALING", "STOLEN", "STOLEN(Q)",
    "FETCH", "REPLY", "BLOCK", "RESUME", "RESUME(Q)",
    "SCHEDULE", "DESCHEDULE",
    "END",
    "SPARK", "SPARKAT", "USED", "PRUNED", "EXPORTED", "ACQUIRED",
    "ALLOC",
    "TERMINATE",
    "SYSTEM_START", "SYSTEM_END",           /* only for debugging */
    "??"
};
#endif

#if defined(GRAN)                                              /* whole file */
char *proc_status_names[] = {
  "Idle", "Sparking", "Starting", "Fetching", "Fishing", "Busy", 
  "UnknownProcStatus"
};

/* For internal use (event statistics) only */
char *event_names[] =
    { "ContinueThread", "StartThread", "ResumeThread", 
      "MoveSpark", "MoveThread", "FindWork",
      "FetchNode", "FetchReply",
      "GlobalBlock", "UnblockThread"
    };

//@cindex CurrentProc
PEs CurrentProc = 0;

/*
  ToDo: Create a structure for the processor status and put all the 
        arrays below into it. 
  -- HWL */

//@cindex CurrentTime
/* One clock for each PE */
rtsTime CurrentTime[MAX_PROC];  

/* Useful to restrict communication; cf fishing model in GUM */
nat OutstandingFetches[MAX_PROC], OutstandingFishes[MAX_PROC];

/* Status of each PE (new since but independent of GranSim Light) */
rtsProcStatus procStatus[MAX_PROC];

# if defined(GRAN) && defined(GRAN_CHECK)
/* To check if the RTS ever tries to run a thread that should be blocked
   because of fetching remote data */
StgTSO *BlockedOnFetch[MAX_PROC];
# define FETCH_MASK_TSO  0x08000000      /* only bits 0, 1, 2 should be used */
# endif

nat SparksAvail = 0;     /* How many sparks are available */
nat SurplusThreads = 0;  /* How many excess threads are there */

/* Do we need to reschedule following a fetch? */
rtsBool NeedToReSchedule = rtsFalse, IgnoreEvents = rtsFalse, IgnoreYields = rtsFalse; 
rtsTime TimeOfNextEvent, TimeOfLastEvent, EndOfTimeSlice; /* checked from the threaded world! */

//@cindex spark queue
/* GranSim: a globally visible array of spark queues */
rtsSparkQ pending_sparks_hds[MAX_PROC];
rtsSparkQ pending_sparks_tls[MAX_PROC];

nat sparksIgnored = 0, sparksCreated = 0;

GlobalGranStats globalGranStats;

nat gran_arith_cost, gran_branch_cost, gran_load_cost, 
    gran_store_cost, gran_float_cost;

/*
Old comment from 0.29. ToDo: Check and update -- HWL

The following variables control the behaviour of GrAnSim. In general, there
is one RTS option for enabling each of these features. In getting the
desired setup of GranSim the following questions have to be answered:
\begin{itemize}
\item {\em Which scheduling algorithm} to use (@RtsFlags.GranFlags.DoFairSchedule@)? 
      Currently only unfair scheduling is supported.
\item What to do when remote data is fetched (@RtsFlags.GranFlags.DoAsyncFetch@)? 
      Either block and wait for the
      data or reschedule and do some other work.
      Thus, if this variable is true, asynchronous communication is
      modelled. Block on fetch mainly makes sense for incremental fetching.

      There is also a simplified fetch variant available
      (@RtsFlags.GranFlags.SimplifiedFetch@). This variant does not use events to model
      communication. It is faster but the results will be less accurate.
\item How aggressive to be in getting work after a reschedule on fetch
      (@RtsFlags.GranFlags.FetchStrategy@)?
      This is determined by the so-called {\em fetching
      strategy\/}. Currently, there are four possibilities:
      \begin{enumerate}
       \item Only run a runnable thread.
       \item Turn a spark into a thread, if necessary.
       \item Steal a remote spark, if necessary.
       \item Steal a runnable thread from another processor, if necessary.
      \end{itemize}
      The variable @RtsFlags.GranFlags.FetchStrategy@ determines how far to go in this list
      when rescheduling on a fetch.
\item Should sparks or threads be stolen first when looking for work
      (@RtsFlags.GranFlags.DoStealThreadsFirst@)? 
      The default is to steal sparks first (much cheaper).
\item Should the RTS use a lazy thread creation scheme
      (@RtsFlags.GranFlags.DoAlwaysCreateThreads@)?  By default yes i.e.\ sparks are only
      turned into threads when work is needed. Also note, that sparks
      can be discarded by the RTS (this is done in the case of an overflow
      of the spark pool). Setting @RtsFlags.GranFlags.DoAlwaysCreateThreads@  to @True@ forces
      the creation of threads at the next possibility (i.e.\ when new work
      is demanded the next time).
\item Should data be fetched closure-by-closure or in packets
      (@RtsFlags.GranFlags.DoBulkFetching@)? The default strategy is a GRIP-like incremental 
      (i.e.\ closure-by-closure) strategy. This makes sense in a
      low-latency setting but is bad in a high-latency system. Setting 
      @RtsFlags.GranFlags.DoBulkFetching@ to @True@ enables bulk (packet) fetching. Other
      parameters determine the size of the packets (@pack_buffer_size@) and the number of
      thunks that should be put into one packet (@RtsFlags.GranFlags.ThunksToPack@).
\item If there is no other possibility to find work, should runnable threads
      be moved to an idle processor (@RtsFlags.GranFlags.DoThreadMigration@)? In any case, the
      RTS tried to get sparks (either local or remote ones) first. Thread
      migration is very expensive, since a whole TSO has to be transferred
      and probably data locality becomes worse in the process. Note, that
      the closure, which will be evaluated next by that TSO is not
      transferred together with the TSO (that might block another thread).
\item Should the RTS distinguish between sparks created by local nodes and
      stolen sparks (@RtsFlags.GranFlags.PreferSparksOfLocalNodes@)?  The idea is to improve 
      data locality by preferring sparks of local nodes (it is more likely
      that the data for those sparks is already on the local processor). 
      However, such a distinction also imposes an overhead on the spark
      queue management, and typically a large number of sparks are
      generated during execution. By default this variable is set to @False@.
\item Should the RTS use granularity control mechanisms? The idea of a 
      granularity control mechanism is to make use of granularity
      information provided via annotation of the @par@ construct in order
      to prefer bigger threads when either turning a spark into a thread or
      when choosing the next thread to schedule. Currently, three such
      mechanisms are implemented:
      \begin{itemize}
        \item Cut-off: The granularity information is interpreted as a
              priority. If a threshold priority is given to the RTS, then
              only those sparks with a higher priority than the threshold 
              are actually created. Other sparks are immediately discarded.
              This is similar to a usual cut-off mechanism often used in 
              parallel programs, where parallelism is only created if the 
              input data is lage enough. With this option, the choice is 
              hidden in the RTS and only the threshold value has to be 
              provided as a parameter to the runtime system.
        \item Priority Sparking: This mechanism keeps priorities for sparks
              and chooses the spark with the highest priority when turning
              a spark into a thread. After that the priority information is
              discarded. The overhead of this mechanism comes from
              maintaining a sorted spark queue.
        \item Priority Scheduling: This mechanism keeps the granularity
              information for threads, to. Thus, on each reschedule the 
              largest thread is chosen. This mechanism has a higher
              overhead, as the thread queue is sorted, too.
       \end{itemize}  
\end{itemize}
*/

//@node Initialisation, Global Address Operations, Constants and Variables, GranSim specific code
//@subsection Initialisation

void 
init_gr_stats (void) {
  memset(&globalGranStats, '\0', sizeof(GlobalGranStats));
#if 0
  /* event stats */
  globalGranStats.noOfEvents = 0;
  for (i=0; i<MAX_EVENT; i++) globalGranStats.event_counts[i]=0;

  /* communication stats */
  globalGranStats.fetch_misses = 0;
  globalGranStats.tot_low_pri_sparks = 0;

  /* obscure stats */  
  globalGranStats.rs_sp_count = 0;
  globalGranStats.rs_t_count = 0;
  globalGranStats.ntimes_total = 0, 
  globalGranStats.fl_total = 0;
  globalGranStats.no_of_steals = 0;

  /* spark queue stats */
  globalGranStats.tot_sq_len = 0, 
  globalGranStats.tot_sq_probes = 0; 
  globalGranStats.tot_sparks = 0;
  globalGranStats.withered_sparks = 0;
  globalGranStats.tot_add_threads = 0;
  globalGranStats.tot_tq_len = 0;
  globalGranStats.non_end_add_threads = 0;

  /* thread stats */
  globalGranStats.tot_threads_created = 0;
  for (i=0; i<MAX_PROC; i++) globalGranStats.threads_created_on_PE[i]=0;
#endif /* 0 */
}

//@node Global Address Operations, Global Event Queue, Initialisation, GranSim specific code
//@subsection Global Address Operations
/*
  ----------------------------------------------------------------------
  Global Address Operations

  These functions perform operations on the global-address (ga) part of a
  closure. The ga is the only new field (1 word) in a closure introduced by
  GrAnSim. It serves as a bitmask, indicating on which processor the
  closure is residing. Since threads are described by Thread State Object
  (TSO), which is nothing but another kind of closure, this scheme allows
  gives placement information about threads.

  A ga is just a bitmask, so the operations on them are mainly bitmask
  manipulating functions. Note, that there are important macros like PROCS,
  IS_LOCAL_TO etc. They are defined in @GrAnSim.lh@.

  NOTE: In GrAnSim-light we don't maintain placement information. This
  allows to simulate an arbitrary number of processors. The price we have
  to be is the lack of costing any communication properly. In short,
  GrAnSim-light is meant to reveal the maximal parallelism in a program.
  From an implementation point of view the important thing is: {\em
  GrAnSim-light does not maintain global-addresses}.  */

/* ga_to_proc returns the first processor marked in the bitmask ga.
   Normally only one bit in ga should be set. But for PLCs all bits
   are set. That shouldn't hurt since we only need IS_LOCAL_TO for PLCs */
 
//@cindex ga_to_proc

static inline PEs
ga_to_proc(StgWord ga)
{
    PEs i;
    for (i = 0; i < RtsFlags.GranFlags.proc && !IS_LOCAL_TO(ga, i); i++);
    ASSERT(i<RtsFlags.GranFlags.proc);
    return (i);
}

/* NB: This takes a *node* rather than just a ga as input */
//@cindex where_is
PEs
where_is(StgClosure *node)
{ return (ga_to_proc(PROCS(node))); }

// debugging only
//@cindex is_unique
rtsBool
is_unique(StgClosure *node)
{ 
  PEs i;
  rtsBool unique = rtsFalse;

  for (i = 0; i < RtsFlags.GranFlags.proc ; i++)
    if (IS_LOCAL_TO(PROCS(node), i))
      if (unique)          // exactly 1 instance found so far
	return rtsFalse;   // found a 2nd instance => not unique
      else 
	unique = rtsTrue;  // found 1st instance 
  ASSERT(unique);          // otherwise returned from within loop
  return (unique);
}

//@cindex any_idle
static inline rtsBool
any_idle(void) { /* any (map (\ i -> procStatus[i] == Idle)) [0,..,MAX_PROC] */
 PEs i; 
 rtsBool any_idle; 
 for(i=0, any_idle=rtsFalse; 
     !any_idle && i<RtsFlags.GranFlags.proc; 
     any_idle = any_idle || procStatus[i] == Idle, i++) 
 {} ;
}

//@cindex idlers
static inline nat
idlers(void) {  /* number of idle PEs */
 PEs i, j; 
 for(i=0, j=0;
     i<RtsFlags.GranFlags.proc; 
     j += (procStatus[i] == Idle) ? 1 : 0, i++) 
 {} ;
 return j;
}

//@node Global Event Queue, Spark queue functions, Global Address Operations, GranSim specific code
//@subsection Global Event Queue
/*
The following routines implement an ADT of an event-queue (FIFO). 
ToDo: Put that in an own file(?)
*/

/* Pointer to the global event queue; events are currently malloc'ed */
rtsEventQ EventHd = NULL;

//@cindex get_next_event
rtsEvent *
get_next_event(void)
{
  static rtsEventQ entry = NULL;

  if (EventHd == NULL) {
    barf("No next event. This may be caused by a circular data dependency in the program.");
  }

  if (entry != NULL)
    free((char *)entry);

  if (RtsFlags.GranFlags.GranSimStats.Global) {     /* count events */
    globalGranStats.noOfEvents++;
    globalGranStats.event_counts[EventHd->evttype]++;
  }

  entry = EventHd;

  IF_GRAN_DEBUG(event_trace,
	   print_event(entry));

  EventHd = EventHd->next;
  return(entry);
}

/* When getting the time of the next event we ignore CONTINUETHREAD events:
   we don't want to be interrupted before the end of the current time slice
   unless there is something important to handle. 
*/
//@cindex get_time_of_next_event
rtsTime
get_time_of_next_event(void)
{ 
  rtsEventQ event = EventHd;

  while (event != NULL && event->evttype==ContinueThread) {
    event = event->next;
  }
  if(event == NULL)
      return ((rtsTime) 0);
  else
      return (event->time);
}

/* ToDo: replace malloc/free with a free list */
//@cindex insert_event
void
insert_event(newentry)
rtsEvent *newentry;
{
  rtsEventType evttype = newentry->evttype;
  rtsEvent *event, **prev;

  /* if(evttype >= CONTINUETHREAD1) evttype = CONTINUETHREAD; */

  /* Search the queue and insert at the right point:
     FINDWORK before everything, CONTINUETHREAD after everything.

     This ensures that we find any available work after all threads have
     executed the current cycle.  This level of detail would normally be
     irrelevant, but matters for ridiculously low latencies...
  */

  /* Changed the ordering: Now FINDWORK comes after everything but 
     CONTINUETHREAD. This makes sure that a MOVESPARK comes before a 
     FINDWORK. This is important when a GranSimSparkAt happens and
     DoAlwaysCreateThreads is turned on. Also important if a GC occurs
     when trying to build a new thread (see much_spark)  -- HWL 02/96  */

  if(EventHd == NULL)
    EventHd = newentry;
  else {
    for (event = EventHd, prev=(rtsEvent**)&EventHd; 
	 event != NULL; 
         prev = (rtsEvent**)&(event->next), event = event->next) {
      switch (evttype) {
        case FindWork: if ( event->time < newentry->time ||
                            ( (event->time == newentry->time) &&
			      (event->evttype != ContinueThread) ) )
                         continue;
                       else
                         break;
        case ContinueThread: if ( event->time <= newentry->time )
			       continue;
			     else
                               break;
        default: if ( event->time < newentry->time || 
	              ((event->time == newentry->time) &&
		       (event->evttype == newentry->evttype)) )
		   continue;
		 else
                   break;
       }
       /* Insert newentry here (i.e. before event) */
       *prev = newentry;
       newentry->next = event;
       break;
    }
    if (event == NULL)
      *prev = newentry;
  }
}

//@cindex new_event
void
new_event(proc,creator,time,evttype,tso,node,spark)
PEs proc, creator;
rtsTime time;
rtsEventType evttype;
StgTSO *tso;
StgClosure *node;
rtsSpark *spark;
{
  rtsEvent *newentry = (rtsEvent *) stgMallocBytes(sizeof(rtsEvent), "new_event");

  newentry->proc     = proc;
  newentry->creator  = creator;
  newentry->time     = time;
  newentry->evttype  = evttype;
  newentry->tso      = tso;
  newentry->node     = node;
  newentry->spark    = spark;
  newentry->gc_info  = 0;
  newentry->next     = NULL;

  insert_event(newentry);

  IF_DEBUG(gran, 
	   fprintf(stderr, "GRAN: new_event: \n"); 
	   print_event(newentry));
}

//@cindex prepend_event
void
prepend_event(event)       /* put event at beginning of EventQueue */
rtsEvent *event;
{				  /* only used for GC! */
 event->next = EventHd;
 EventHd = event;
}

//@cindex grab_event
rtsEventQ
grab_event(void)             /* undo prepend_event i.e. get the event */
{			 /* at the head of EventQ but don't free anything */
 rtsEventQ event = EventHd;

 if (EventHd == NULL) {
   barf("No next event (in grab_event). This may be caused by a circular data dependency in the program.");
 }

 EventHd = EventHd->next;
 return (event);
}

//@cindex traverse_eventq_for_gc
void 
traverse_eventq_for_gc(void)
{
 rtsEventQ event = EventHd;
 StgWord bufsize;
 StgClosure *closurep;
 StgTSO *tsop;
 StgPtr buffer, bufptr;
 PEs proc, creator;

 /* Traverse eventq and replace every FETCHREPLY by a FETCHNODE for the
    orig closure (root of packed graph). This means that a graph, which is
    between processors at the time of GC is fetched again at the time when
    it would have arrived, had there been no GC. Slightly inaccurate but
    safe for GC.
    This is only needed for GUM style fetchng. -- HWL */
 if (!RtsFlags.GranFlags.DoBulkFetching)
   return;

 for(event = EventHd; event!=NULL; event=event->next) {
   if (event->evttype==FetchReply) {
     buffer = stgCast(StgPtr,event->node);
     ASSERT(buffer[PACK_FLAG_LOCN]==MAGIC_PACK_FLAG);  /* It's a pack buffer */
     bufsize = buffer[PACK_SIZE_LOCN];
     closurep = stgCast(StgClosure*,buffer[PACK_HDR_SIZE]);
     tsop = stgCast(StgTSO*,buffer[PACK_TSO_LOCN]);
     proc = event->proc;
     creator = event->creator;                 /* similar to unpacking */
     for (bufptr=buffer+PACK_HDR_SIZE; 
	  bufptr<(buffer+bufsize);
	  bufptr++) {
	 // if ( (INFO_TYPE(INFO_PTR(*bufptr)) == INFO_SPEC_RBH_TYPE) ||
	 //      (INFO_TYPE(INFO_PTR(*bufptr)) == INFO_GEN_RBH_TYPE) ) {
	   if ( GET_INFO(stgCast(StgClosure*,bufptr)) ) {
	     convertFromRBH(stgCast(StgClosure *,bufptr));
	 }
     }
     free(buffer);
     event->evttype = FetchNode;
     event->proc    = creator;
     event->creator = proc;
     event->node    = closurep;
     event->tso     = tsop;
     event->gc_info = 0;
   }
 }
}

void
markEventQueue(void)
{ 
  StgClosure *MarkRoot(StgClosure *root); // prototype

  rtsEventQ event = EventHd;
  nat len;

  /* iterate over eventq and register relevant fields in event as roots */
  for(event = EventHd, len =  0; event!=NULL; event=event->next, len++) {
    switch (event->evttype) {
      case ContinueThread:  
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	break;
      case StartThread: 
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	event->node = (StgClosure *)MarkRoot((StgClosure *)event->node);
	break;
      case ResumeThread:
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	event->node = (StgClosure *)MarkRoot((StgClosure *)event->node);
	break;
      case MoveSpark:
	event->spark->node = (StgClosure *)MarkRoot((StgClosure *)event->spark->node);
	break;
      case MoveThread:
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	break;
      case FindWork:
	break;
      case FetchNode: 
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	event->node = (StgClosure *)MarkRoot((StgClosure *)event->node);
  	break;
      case FetchReply:
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	if (RtsFlags.GranFlags.DoBulkFetching)
	  // ToDo: traverse_eventw_for_gc if GUM-Fetching!!! HWL
	  belch("ghuH: packets in BulkFetching not marked as roots; mayb be fatal");
	else
	  event->node = (StgClosure *)MarkRoot((StgClosure *)event->node);
	break;
      case GlobalBlock:
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	event->node = (StgClosure *)MarkRoot((StgClosure *)event->node);
	break;
      case UnblockThread:
	event->tso = (StgTSO *)MarkRoot((StgClosure *)event->tso);
	event->node = (StgClosure *)MarkRoot((StgClosure *)event->node);
	break;
      default:
	barf("markEventQueue: trying to mark unknown event @ %p", event);
    }}
  IF_DEBUG(gc,
	   belch("GC: markEventQueue: %d events in queue", len));
}

/*
  Prune all ContinueThread events related to tso or node in the eventq.
  Currently used if a thread leaves STG land with ThreadBlocked status,
  i.e. it blocked on a closure and has been put on its blocking queue.  It
  will be reawakended via a call to awakenBlockedQueue. Until then no
  event effecting this tso should appear in the eventq.  A bit of a hack,
  because ideally we shouldn't generate such spurious ContinueThread events
  in the first place.  
*/
//@cindex prune_eventq 
void 
prune_eventq(tso, node) 
StgTSO *tso; 
StgClosure *node; 
{ rtsEventQ prev = (rtsEventQ)NULL, event = EventHd;

  /* node unused for now */ 
  ASSERT(node==NULL); 
  /* tso must be valid, then */
  ASSERT(tso!=END_TSO_QUEUE);
  while (event != NULL) {
    if (event->evttype==ContinueThread && 
	(event->tso==tso)) {
      IF_GRAN_DEBUG(event_trace, // ToDo: use another debug flag
		    belch("prune_eventq: pruning ContinueThread event for TSO %d (%p) on PE %d @ %lx (%p)",
			  event->tso->id, event->tso, event->proc, event->time, event));
      if (prev==(rtsEventQ)NULL) { // beginning of eventq
	EventHd = event->next;
	free(event); 
	event = EventHd;
      } else {
	prev->next = event->next;
	free(event); 
	event = prev->next;
      }
    } else { // no pruning necessary; go to next event
      prev = event;
      event = event->next;
    }
  }
}

//@cindex print_event
void
print_event(event)
rtsEvent *event;
{
  char str_tso[16], str_node[16];
  StgThreadID tso_id;

  if (event->tso==END_TSO_QUEUE) {
    strcpy(str_tso, "______");
    tso_id = 0;
  } else { 
    sprintf(str_tso, "%p", event->tso);
    tso_id = (event->tso==NULL) ? 0 : event->tso->id;
  }
  if  (event->node==(StgClosure*)NULL) {
    strcpy(str_node, "______");
  } else {
    sprintf(str_node, "%p", event->node);
  }
  // HWL: shouldn't be necessary; ToDo: nuke
  //str_tso[6]='\0';
  //str_node[6]='\0';

  if (event==NULL)
    fprintf(stderr,"Evt: NIL\n");
  else
    fprintf(stderr, "Evt: %s (%u), PE %u [%u], Time %lu, TSO %d (%s), Node %s\n", //"Evt: %s (%u), PE %u [%u], Time %u, TSO %s (%#l), Node %s\n",
	      event_names[event->evttype], event->evttype,
              event->proc, event->creator, event->time, 
	      tso_id, str_tso, str_node
	      /*, event->spark, event->next */ );

}

//@cindex print_eventq
void
print_eventq(hd)
rtsEvent *hd;
{
  rtsEvent *x;

  fprintf(stderr,"Event Queue with root at %p:\n", hd);
  for (x=hd; x!=NULL; x=x->next) {
    print_event(x);
  }
}

/* 
   Spark queue functions are now all  in Sparks.c!!
*/
//@node Scheduling functions, Thread Queue routines, Spark queue functions, GranSim specific code
//@subsection Scheduling functions

/* 
   These functions are variants of thread initialisation and therefore
   related to initThread and friends in Schedule.c. However, they are
   specific to a GranSim setup in storing more info in the TSO's statistics
   buffer and sorting the thread queues etc.  
*/

/*
   A large portion of startThread deals with maintaining a sorted thread
   queue, which is needed for the Priority Sparking option. Without that
   complication the code boils down to FIFO handling.  
*/
//@cindex insertThread
void
insertThread(tso, proc)
StgTSO*     tso;
PEs         proc;
{
  StgTSO *prev = NULL, *next = NULL;
  nat count = 0;
  rtsBool found = rtsFalse;

  ASSERT(CurrentProc==proc);
  ASSERT(!is_on_queue(tso,proc));
  /* Idle proc: put the thread on the run queue
     same for pri spark and basic version */
  if (run_queue_hds[proc] == END_TSO_QUEUE)
    {
      /* too strong!
      ASSERT((CurrentProc==MainProc &&   
	      CurrentTime[MainProc]==0 &&
	      procStatus[MainProc]==Idle) ||
	     procStatus[proc]==Starting);
      */
      run_queue_hds[proc] = run_queue_tls[proc] = tso;

      CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadqueuetime;

      /* new_event of ContinueThread has been moved to do_the_startthread */

      /* too strong!
      ASSERT(procStatus[proc]==Idle || 
             procStatus[proc]==Fishing || 
             procStatus[proc]==Starting);
      procStatus[proc] = Busy;
      */
      return;
    }

  if (RtsFlags.GranFlags.Light)
    GranSimLight_insertThread(tso, proc);

  /* Only for Pri Scheduling: find place where to insert tso into queue */
  if (RtsFlags.GranFlags.DoPriorityScheduling && tso->gran.pri!=0)
    /* {add_to_spark_queue}vo' jInIHta'; Qu' wa'DIch yIleghQo' */
    for (prev = run_queue_hds[proc], next = run_queue_hds[proc]->link, count=0;
	 (next != END_TSO_QUEUE) && 
	 !(found = tso->gran.pri >= next->gran.pri);
	 prev = next, next = next->link, count++) 
      { 
       ASSERT((prev!=(StgTSO*)NULL || next==run_queue_hds[proc]) &&
	      (prev==(StgTSO*)NULL || prev->link==next));
      }

  ASSERT(!found || next != END_TSO_QUEUE);
  ASSERT(procStatus[proc]!=Idle);
 
  if (found) {
     /* found can only be rtsTrue if pri scheduling enabled */ 
     ASSERT(RtsFlags.GranFlags.DoPriorityScheduling);
     if (RtsFlags.GranFlags.GranSimStats.Global) 
       globalGranStats.non_end_add_threads++;
     /* Add tso to ThreadQueue between prev and next */
     tso->link = next;
     if ( next == (StgTSO*)END_TSO_QUEUE ) {
       run_queue_tl = tso;
     } else {
       /* no back link for TSO chain */
     }
     
     if ( prev == (StgTSO*)END_TSO_QUEUE ) {
       /* Never add TSO as first elem of thread queue; the first */
       /* element should be the one that is currently running -- HWL */
       IF_DEBUG(gran,
		belch("GRAN: Qagh: NewThread (w/ PriorityScheduling): Trying to add TSO %p (PRI=%d) as first elem of threadQ (%p) on proc %u (@ %u)\n",
		    tso, tso->gran.pri, run_queue_hd, proc,
		    CurrentTime[proc]));
     } else {
      prev->link = tso;
     }
  } else { /* !found */ /* or not pri sparking! */
    /* Add TSO to the end of the thread queue on that processor */
    run_queue_tls[proc]->link = tso;
    run_queue_tls[proc] = tso;
  }
  ASSERT(RtsFlags.GranFlags.DoPriorityScheduling || count==0);
  CurrentTime[proc] += count * RtsFlags.GranFlags.Costs.pri_sched_overhead +
                       RtsFlags.GranFlags.Costs.threadqueuetime;

  /* ToDo: check if this is still needed -- HWL 
  if (RtsFlags.GranFlags.DoThreadMigration)
    ++SurplusThreads;

  if (RtsFlags.GranFlags.GranSimStats.Full &&
      !(( event_type == GR_START || event_type == GR_STARTQ) && 
	RtsFlags.GranFlags.labelling) )
    DumpRawGranEvent(proc, creator, event_type+1, tso, node, 
	             tso->gran.sparkname, spark_queue_len(proc));
  */

# if defined(GRAN_CHECK)
  /* Check if thread queue is sorted. Only for testing, really!  HWL */
  if ( RtsFlags.GranFlags.DoPriorityScheduling && 
       (RtsFlags.GranFlags.Debug.sortedQ) ) {
    rtsBool sorted = rtsTrue;
    StgTSO *prev, *next;

    if (run_queue_hds[proc]==END_TSO_QUEUE || 
	run_queue_hds[proc]->link==END_TSO_QUEUE) {
      /* just 1 elem => ok */
    } else {
      /* Qu' wa'DIch yIleghQo' (ignore first elem)! */
      for (prev = run_queue_hds[proc]->link, next = prev->link;
	   (next != END_TSO_QUEUE) ;
	   prev = next, next = prev->link) {
	ASSERT((prev!=(StgTSO*)NULL || next==run_queue_hds[proc]) &&
	       (prev==(StgTSO*)NULL || prev->link==next));
	sorted = sorted && 
	         (prev->gran.pri >= next->gran.pri);
      }
    }
    if (!sorted) {
      fprintf(stderr,"Qagh: THREADQ on PE %d is not sorted:\n",
	      CurrentProc);
      G_THREADQ(run_queue_hd,0x1);
    }
  }
# endif
}

/*
  insertThread, which is only used for GranSim Light, is similar to
  startThread in that it adds a TSO to a thread queue. However, it assumes
  that the thread queue is sorted by local clocks and it inserts the TSO at
  the right place in the queue. Don't create any event, just insert.  
*/
//@cindex GranSimLight_insertThread
rtsBool
GranSimLight_insertThread(tso, proc)
StgTSO* tso;
PEs proc;
{
  StgTSO *prev, *next;
  nat count = 0;
  rtsBool found = rtsFalse;

  ASSERT(RtsFlags.GranFlags.Light);

  /* In GrAnSim-Light we always have an idle `virtual' proc.
     The semantics of the one-and-only thread queue is different here:
     all threads in the queue are running (each on its own virtual processor);
     the queue is only needed internally in the simulator to interleave the
     reductions of the different processors.
     The one-and-only thread queue is sorted by the local clocks of the TSOs.
  */
  ASSERT(run_queue_hds[proc] != END_TSO_QUEUE);
  ASSERT(tso->link == END_TSO_QUEUE);

  /* If only one thread in queue so far we emit DESCHEDULE in debug mode */
  if (RtsFlags.GranFlags.GranSimStats.Full &&
      (RtsFlags.GranFlags.Debug.checkLight) && 
      (run_queue_hd->link == END_TSO_QUEUE)) {
    DumpRawGranEvent(proc, proc, GR_DESCHEDULE,
		     run_queue_hds[proc], (StgClosure*)NULL, 
		     tso->gran.sparkname, spark_queue_len(proc)); // ToDo: check spar_queue_len
    // resched = rtsTrue;
  }

  /* this routine should only be used in a GrAnSim Light setup */
  /* && CurrentProc must be 0 in GrAnSim Light setup */
  ASSERT(RtsFlags.GranFlags.Light && CurrentProc==0);

  /* Idle proc; same for pri spark and basic version */
  if (run_queue_hd==END_TSO_QUEUE)
    {
      run_queue_hd = run_queue_tl = tso;
      /* MAKE_BUSY(CurrentProc); */
      return rtsTrue;
    }

  for (prev = run_queue_hds[proc], next = run_queue_hds[proc]->link, count = 0;
       (next != END_TSO_QUEUE) && 
       !(found = (tso->gran.clock < next->gran.clock));
       prev = next, next = next->link, count++) 
    { 
       ASSERT((prev!=(StgTSO*)NULL || next==run_queue_hds[proc]) &&
	      (prev==(StgTSO*)NULL || prev->link==next));
    }

  /* found can only be rtsTrue if pri sparking enabled */ 
  if (found) {
     /* Add tso to ThreadQueue between prev and next */
     tso->link = next;
     if ( next == END_TSO_QUEUE ) {
       run_queue_tls[proc] = tso;
     } else {
       /* no back link for TSO chain */
     }
     
     if ( prev == END_TSO_QUEUE ) {
       run_queue_hds[proc] = tso;
     } else {
       prev->link = tso;
     }
  } else { /* !found */ /* or not pri sparking! */
    /* Add TSO to the end of the thread queue on that processor */
    run_queue_tls[proc]->link = tso;
    run_queue_tls[proc] = tso;
  }

  if ( prev == END_TSO_QUEUE ) {        /* new head of queue */
    new_event(proc, proc, CurrentTime[proc],
	      ContinueThread,
	      tso, (StgClosure*)NULL, (rtsSpark*)NULL);
  }
  /*
  if (RtsFlags.GranFlags.GranSimStats.Full && 
      !(( event_type == GR_START || event_type == GR_STARTQ) && 
	RtsFlags.GranFlags.labelling) )
    DumpRawGranEvent(proc, creator, gr_evttype, tso, node,
		     tso->gran.sparkname, spark_queue_len(proc));
  */
  return rtsTrue;
}

/*
  endThread is responsible for general clean-up after the thread tso has
  finished. This includes emitting statistics into the profile etc.  
*/
void
endThread(StgTSO *tso, PEs proc) 
{
  ASSERT(procStatus[proc]==Busy);        // coming straight out of STG land
  ASSERT(tso->what_next==ThreadComplete);
  // ToDo: prune ContinueThreads for this TSO from event queue
  DumpEndEvent(proc, tso, rtsFalse /* not mandatory */);

  /* if this was the last thread on this PE then make it Idle */
  if (run_queue_hds[proc]==END_TSO_QUEUE) {
    procStatus[CurrentProc] = Idle;
  }
}

//@node Thread Queue routines, GranSim functions, Scheduling functions, GranSim specific code
//@subsection Thread Queue routines

/* 
   Check whether given tso resides on the run queue of the current processor.
   Only used for debugging.
*/
   
//@cindex is_on_queue
rtsBool
is_on_queue (StgTSO *tso, PEs proc) 
{
  StgTSO *t;
  rtsBool found;

  for (t=run_queue_hds[proc], found=rtsFalse; 
       t!=END_TSO_QUEUE && !(found = t==tso);
       t=t->link)
    /* nothing */ ;

  return found;
}

/* This routine  is only  used for keeping   a statistics  of thread  queue
   lengths to evaluate the impact of priority scheduling. -- HWL 
   {spark_queue_len}vo' jInIHta'
*/
//@cindex thread_queue_len
nat
thread_queue_len(PEs proc) 
{
 StgTSO *prev, *next;
 nat len;

 for (len = 0, prev = END_TSO_QUEUE, next = run_queue_hds[proc];
      next != END_TSO_QUEUE; 
      len++, prev = next, next = prev->link)
   {}

 return (len);
}

//@node GranSim functions, GranSimLight routines, Thread Queue routines, GranSim specific code
//@subsection GranSim functions

/* -----------------------------------------------------------------  */
/* The main event handling functions; called from Schedule.c (schedule) */
/* -----------------------------------------------------------------  */
 
//@cindex do_the_globalblock

void 
do_the_globalblock(rtsEvent* event)
{ 
  PEs proc          = event->proc;        /* proc that requested node */
  StgTSO *tso       = event->tso;         /* tso that requested node */
  StgClosure  *node = event->node;        /* requested, remote node */

  IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the GlobalBlock\n"));
  /* There should be no GLOBALBLOCKs in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);
  /* GlobalBlock events only valid with GUM fetching */
  ASSERT(RtsFlags.GranFlags.DoBulkFetching);

  IF_GRAN_DEBUG(bq, // globalBlock,
    if (IS_LOCAL_TO(PROCS(node),proc)) {
      belch("## Qagh: GlobalBlock: Blocking TSO %d (%p) on LOCAL node %p (PE %d).\n",
	    tso->id, tso, node, proc);
    });

  /* CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.munpacktime; */
  if ( blockFetch(tso,proc,node) != 0 )
    return;                     /* node has become local by now */

#if 0
 ToDo: check whether anything has to be done at all after blockFetch -- HWL

  if (!RtsFlags.GranFlags.DoAsyncFetch) { /* head of queue is next thread */
    StgTSO* tso = run_queue_hds[proc];       /* awaken next thread */
    if (tso != (StgTSO*)NULL) {
      new_event(proc, proc, CurrentTime[proc],
		ContinueThread,
		tso, (StgClosure*)NULL, (rtsSpark*)NULL);
      CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadcontextswitchtime;
      if (RtsFlags.GranFlags.GranSimStats.Full)
        DumpRawGranEvent(proc, CurrentProc, GR_SCHEDULE, tso,
			 (StgClosure*)NULL, tso->gran.sparkname, spark_queue_len(CurrentProc));  // ToDo: check sparkname and spar_queue_len
      procStatus[proc] = Busy;                  /* might have been fetching */
    } else {
      procStatus[proc] = Idle;                     /* no work on proc now */
    }
  } else {  /* RtsFlags.GranFlags.DoAsyncFetch i.e. block-on-fetch */
	      /* other thread is already running */
	      /* 'oH 'utbe' 'e' vIHar ; I think that's not needed -- HWL 
	      new_event(proc,proc,CurrentTime[proc],
		       CONTINUETHREAD,EVENT_TSO(event),
		       (RtsFlags.GranFlags.DoBulkFetching ? closure :
		       EVENT_NODE(event)),NULL);
	      */
  }
#endif
}

//@cindex do_the_unblock

void 
do_the_unblock(rtsEvent* event) 
{
  PEs proc = event->proc,       /* proc that requested node */
      creator = event->creator; /* proc that requested node */
  StgTSO* tso = event->tso;     /* tso that requested node */
  StgClosure* node = event->node;  /* requested, remote node */
  
  IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the UnBlock\n"))
  /* There should be no UNBLOCKs in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);
  /* UnblockThread means either FetchReply has arrived or
     a blocking queue has been awakened;
     ToDo: check with assertions
  ASSERT(procStatus[proc]==Fetching || IS_BLACK_HOLE(event->node));
  */
  if (!RtsFlags.GranFlags.DoAsyncFetch) {  /* block-on-fetch */
    /* We count block-on-fetch as normal block time */    
    tso->gran.blocktime += CurrentTime[proc] - tso->gran.blockedat;
    /* Dumping now done when processing the event
       No costs for contextswitch or thread queueing in this case 
       if (RtsFlags.GranFlags.GranSimStats.Full)
         DumpRawGranEvent(proc, CurrentProc, GR_RESUME, tso, 
                          (StgClosure*)NULL, tso->gran.sparkname, spark_queue_len(CurrentProc));
    */
    /* Maybe do this in FetchReply already 
    if (procStatus[proc]==Fetching)
      procStatus[proc] = Busy;
    */
    /*
    new_event(proc, proc, CurrentTime[proc],
	      ContinueThread,
	      tso, node, (rtsSpark*)NULL);
    */
  } else {
    /* Asynchr comm causes additional costs here: */
    /* Bring the TSO from the blocked queue into the threadq */
  }
  /* In all cases, the UnblockThread causes a ResumeThread to be scheduled */
  new_event(proc, proc, 
	    CurrentTime[proc]+RtsFlags.GranFlags.Costs.threadqueuetime,
	    ResumeThread,
	    tso, node, (rtsSpark*)NULL);
}

//@cindex do_the_fetchnode

void
do_the_fetchnode(rtsEvent* event)
{
  PEs proc = event->proc,       /* proc that holds the requested node */
      creator = event->creator; /* proc that requested node */
  StgTSO* tso = event->tso;
  StgClosure* node = event->node;  /* requested, remote node */
  rtsFetchReturnCode rc;

  ASSERT(CurrentProc==proc);
  /* There should be no FETCHNODEs in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);

  IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the FetchNode\n"));

  CurrentTime[proc] += RtsFlags.GranFlags.Costs.munpacktime;

  /* ToDo: check whether this is the right place for dumping the event */
  if (RtsFlags.GranFlags.GranSimStats.Full)
    DumpRawGranEvent(creator, proc, GR_FETCH, tso, node, (StgInt)0, 0);

  do {
    rc = handleFetchRequest(node, proc, creator, tso);
    if (rc == OutOfHeap) {                                   /* trigger GC */
# if defined(GRAN_CHECK)  && defined(GRAN)
     if (RtsFlags.GcFlags.giveStats)
       fprintf(RtsFlags.GcFlags.statsFile,"*****   veQ boSwI'  PackNearbyGraph(node %p, tso %p (%d))\n",
     	        node, tso, tso->id);
# endif
     barf("//// do_the_fetchnode: out of heap after handleFetchRequest; ToDo: call GarbageCollect()");
     prepend_event(event);
     GarbageCollect(GetRoots, rtsFalse); 
     // HWL: ToDo: check whether a ContinueThread has to be issued
     // HWL old: ReallyPerformThreadGC(PACK_HEAP_REQUIRED, rtsFalse);
# if 0 && defined(GRAN_CHECK)  && defined(GRAN)
     if (RtsFlags.GcFlags.giveStats) {
       fprintf(RtsFlags.GcFlags.statsFile,"*****      SAVE_Hp=%p, SAVE_HpLim=%p, PACK_HEAP_REQUIRED=%d\n",
     	        Hp, HpLim, 0) ; // PACK_HEAP_REQUIRED);  ???
       fprintf(stderr,"*****      No. of packets so far: %d (total size: %d)\n", 
     	        globalGranStats.tot_packets, globalGranStats.tot_packet_size);
     }
# endif 
     event = grab_event();
     // Hp -= PACK_HEAP_REQUIRED; // ???

     /* GC knows that events are special and follows the pointer i.e. */
     /* events are valid even if they moved. An EXIT is triggered */
     /* if there is not enough heap after GC. */
    }
  } while (rc == OutOfHeap);
}

//@cindex do_the_fetchreply
void 
do_the_fetchreply(rtsEvent* event)
{
  PEs proc = event->proc,       /* proc that requested node */
      creator = event->creator; /* proc that holds the requested node */
  StgTSO* tso = event->tso;
  StgClosure* node = event->node;  /* requested, remote node */
  StgClosure* closure=(StgClosure*)NULL;

  ASSERT(CurrentProc==proc);
  ASSERT(RtsFlags.GranFlags.DoAsyncFetch || procStatus[proc]==Fetching);

  IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the FetchReply\n"));
  /* There should be no FETCHREPLYs in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);

  /* assign message unpack costs *before* dumping the event */
  CurrentTime[proc] += RtsFlags.GranFlags.Costs.munpacktime;
  
  /* ToDo: check whether this is the right place for dumping the event */
  if (RtsFlags.GranFlags.GranSimStats.Full)
    DumpRawGranEvent(proc, creator, GR_REPLY, tso, node, 
  		      tso->gran.sparkname, spark_queue_len(proc));

  /* THIS SHOULD NEVER HAPPEN 
     If tso is in the BQ of node this means that it actually entered the 
     remote closure, due to a missing GranSimFetch at the beginning of the 
     entry code; therefore, this is actually a faked fetch, triggered from 
     within GranSimBlock; 
     since tso is both in the EVQ and the BQ for node, we have to take it out 
     of the BQ first before we can handle the FetchReply;
     ToDo: special cases in awakenBlockedQueue, since the BQ magically moved.
  */
  if (tso->block_info.closure!=(StgClosure*)NULL) {
    IF_GRAN_DEBUG(bq,
		  belch("## ghuH: TSO %d (%p) in FetchReply is blocked on node %p (shouldn't happen AFAIK)",
			tso->id, tso, node));
    // unlink_from_bq(tso, node);
  }
    
  if (RtsFlags.GranFlags.DoBulkFetching) {      /* bulk (packet) fetching */
    rtsPackBuffer *buffer = (rtsPackBuffer*)node;
    nat size = buffer->size;
  
    /* NB: Fetch misses can't occur with GUM fetching, as */
    /* updatable closure are turned into RBHs and therefore locked */
    /* for other processors that try to grab them. */
  
    closure = UnpackGraph(buffer);
    CurrentTime[proc] += size * RtsFlags.GranFlags.Costs.munpacktime;
  } else  // incremental fetching
      /* Copy or  move node to CurrentProc */
      if (fetchNode(node, creator, proc)) {
        /* Fetch has failed i.e. node has been grabbed by another PE */
        PEs p = where_is(node);
        rtsTime fetchtime;
     
	if (RtsFlags.GranFlags.GranSimStats.Global)
	  globalGranStats.fetch_misses++;

	IF_GRAN_DEBUG(thunkStealing,
		 belch("== Qu'vatlh! fetch miss @ %u: node %p is at proc %u (rather than proc %u)\n",
		       CurrentTime[proc],node,p,creator));

	CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mpacktime;
	
	/* Count fetch again !? */
	++(tso->gran.fetchcount);
	tso->gran.fetchtime += RtsFlags.GranFlags.Costs.fetchtime;
        
	fetchtime = stg_max(CurrentTime[CurrentProc],CurrentTime[p]) +
		    RtsFlags.GranFlags.Costs.latency;
	
	/* Chase the grabbed node */
	new_event(p, proc, fetchtime,
		  FetchNode,
		  tso, node, (rtsSpark*)NULL);

# if 0 && defined(GRAN_CHECK) && defined(GRAN) /* Just for testing */
       IF_GRAN_DEBUG(blockOnFetch,
		     BlockedOnFetch[CurrentProc] = tso;) /*-rtsTrue;-*/
	
       IF_GRAN_DEBUG(blockOnFetch_sanity,
		     tso->type |= FETCH_MASK_TSO;)
# endif

        CurrentTime[proc] += RtsFlags.GranFlags.Costs.mtidytime;
	
        return; /* NB: no REPLy has been processed; tso still sleeping */
    }

    /* -- Qapla'! Fetch has been successful; node is here, now  */
    ++(event->tso->gran.fetchcount);
    event->tso->gran.fetchtime += RtsFlags.GranFlags.Costs.fetchtime;

    /* this is now done at the beginning of this routine
    if (RtsFlags.GranFlags.GranSimStats.Full)
       DumpRawGranEvent(proc,event->creator, GR_REPLY, event->tso,
			(RtsFlags.GranFlags.DoBulkFetching ? 
			       closure : 
			       event->node),
                        tso->gran.sparkname, spark_queue_len(proc));
    */

    ASSERT(OutstandingFetches[proc] > 0);
    --OutstandingFetches[proc];
    new_event(proc, proc, CurrentTime[proc],
	      ResumeThread,
	      event->tso, (RtsFlags.GranFlags.DoBulkFetching ? 
			   closure : 
			   event->node),
	      (rtsSpark*)NULL);
}

//@cindex do_the_movethread

void
do_the_movethread(rtsEvent* event) {
  PEs proc = event->proc,       /* proc that requested node */
      creator = event->creator; /* proc that holds the requested node */
  StgTSO* tso = event->tso;

 IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the MoveThread\n"));

 ASSERT(CurrentProc==proc);
 /* There should be no MOVETHREADs in GrAnSim Light setup */
 ASSERT(!RtsFlags.GranFlags.Light);
 /* MOVETHREAD events should never occur without -bM */
 ASSERT(RtsFlags.GranFlags.DoThreadMigration);
 /* Bitmask of moved thread should be 0 */
 ASSERT(PROCS(tso)==0);
 ASSERT(procStatus[proc] == Fishing ||
	RtsFlags.GranFlags.DoAsyncFetch);
 ASSERT(OutstandingFishes[proc]>0);

 /* ToDo: exact costs for unpacking the whole TSO  */
 CurrentTime[proc] +=  5l * RtsFlags.GranFlags.Costs.munpacktime;

 /* ToDo: check whether this is the right place for dumping the event */
 if (RtsFlags.GranFlags.GranSimStats.Full)
   DumpRawGranEvent(proc, creator, 
		    GR_STOLEN, tso, (StgClosure*)NULL, (StgInt)0, 0);

 // ToDo: check cost functions
 --OutstandingFishes[proc];
 SET_GRAN_HDR(tso, ThisPE);         // adjust the bitmask for the TSO
 insertThread(tso, proc);

 if (procStatus[proc]==Fishing)
   procStatus[proc] = Idle;

 if (RtsFlags.GranFlags.GranSimStats.Global)
   globalGranStats.tot_TSOs_migrated++;
}

//@cindex do_the_movespark

void
do_the_movespark(rtsEvent* event) {
 PEs proc = event->proc,       /* proc that requested spark */
     creator = event->creator; /* proc that holds the requested spark */
 StgTSO* tso = event->tso;
 rtsSparkQ spark = event->spark;

 IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the MoveSpark\n"))

 ASSERT(CurrentProc==proc);
 ASSERT(spark!=NULL);
 ASSERT(procStatus[proc] == Fishing ||
	RtsFlags.GranFlags.DoAsyncFetch);
 ASSERT(OutstandingFishes[proc]>0); 

 CurrentTime[proc] += RtsFlags.GranFlags.Costs.munpacktime;
          
 /* record movement of spark only if spark profiling is turned on */
 if (RtsFlags.GranFlags.GranSimStats.Sparks)
    DumpRawGranEvent(proc, creator,
		     SP_ACQUIRED,
		     tso, spark->node, spark->name, spark_queue_len(proc));

 /* global statistics */
 if ( RtsFlags.GranFlags.GranSimStats.Global &&
      !closure_SHOULD_SPARK(spark->node))
   globalGranStats.withered_sparks++;
   /* Not adding the spark to the spark queue would be the right */
   /* thing here, but it also would be cheating, as this info can't be */
   /* available in a real system. -- HWL */

 --OutstandingFishes[proc];

 add_to_spark_queue(spark);

 IF_GRAN_DEBUG(randomSteal, // ToDo: spark-distribution flag
	       print_sparkq_stats());

 /* Should we treat stolen sparks specially? Currently, we don't. */

 if (procStatus[proc]==Fishing)
   procStatus[proc] = Idle;

 /* add_to_spark_queue will increase the time of the current proc. */
 /*
   If proc was fishing, it is Idle now with the new spark in its spark
   pool. This means that the next time handleIdlePEs is called, a local
   FindWork will be created on this PE to turn the spark into a thread. Of
   course another PE might steal the spark in the meantime (that's why we
   are using events rather than inlining all the operations in the first
   place). */
}

/*
  In the Constellation class version of GranSim the semantics of StarThread
  events has changed. Now, StartThread has to perform 3 basic operations:
   - create a new thread (previously this was done in ActivateSpark);
   - insert the thread into the run queue of the current processor
   - generate a new event for actually running the new thread
  Note that the insertThread is called via createThread. 
*/
  
//@cindex do_the_startthread

void
do_the_startthread(rtsEvent *event)
{
  PEs proc          = event->proc;        /* proc that requested node */
  StgTSO *tso       = event->tso;         /* tso that requested node */
  StgClosure  *node = event->node;        /* requested, remote node */
  rtsSpark *spark   = event->spark;
  GranEventType gr_evttype;

  ASSERT(CurrentProc==proc);
  ASSERT(!RtsFlags.GranFlags.Light || CurrentProc==0);
  ASSERT(event->evttype == ResumeThread || event->evttype == StartThread);
  /* if this was called via StartThread: */
  ASSERT(event->evttype!=StartThread || tso == END_TSO_QUEUE); // not yet created
  // ToDo: check: ASSERT(event->evttype!=StartThread || procStatus[proc]==Starting);
  /* if this was called via ResumeThread: */
  ASSERT(event->evttype!=ResumeThread || 
	   RtsFlags.GranFlags.DoAsyncFetch ||!is_on_queue(tso,proc)); 

  /* startThread may have been called from the main event handler upon
     finding either a ResumeThread or a StartThread event; set the
     gr_evttype (needed for writing to .gr file) accordingly */
  // gr_evttype = (event->evttype == ResumeThread) ? GR_RESUME : GR_START;

  if ( event->evttype == StartThread ) {
    GranEventType gr_evttype = (run_queue_hds[proc]==END_TSO_QUEUE) ? 
                                 GR_START : GR_STARTQ;

    tso = createThread(BLOCK_SIZE_W, spark->gran_info);// implicit insertThread!
    pushClosure(tso, node);

    // ToDo: fwd info on local/global spark to thread -- HWL
    // tso->gran.exported =  spark->exported;
    // tso->gran.locked =   !spark->global;
    tso->gran.sparkname = spark->name;

    ASSERT(CurrentProc==proc);
    if (RtsFlags.GranFlags.GranSimStats.Full)
      DumpGranEvent(gr_evttype,tso);

    CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadcreatetime;
  } else { // event->evttype == ResumeThread
    GranEventType gr_evttype = (run_queue_hds[proc]==END_TSO_QUEUE) ? 
                                 GR_RESUME : GR_RESUMEQ;

    insertThread(tso, proc);

    ASSERT(CurrentProc==proc);
    if (RtsFlags.GranFlags.GranSimStats.Full)
      DumpGranEvent(gr_evttype,tso);
  }

  ASSERT(run_queue_hds[proc]!=END_TSO_QUEUE); // non-empty run queue
  procStatus[proc] = Busy;
  /* make sure that this thread is actually run */
  new_event(proc, proc, 
	    CurrentTime[proc],
	    ContinueThread,
	    tso, node, (rtsSpark*)NULL);
  
  /* A wee bit of statistics gathering */
  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_add_threads++;
    globalGranStats.tot_tq_len += thread_queue_len(CurrentProc);
  }

}

//@cindex do_the_findwork
void
do_the_findwork(rtsEvent* event) 
{
  PEs proc = event->proc,       /* proc to search for work */
      creator = event->creator; /* proc that requested work */
  rtsSparkQ spark = event->spark;
  /* ToDo: check that this size is safe -- HWL */
#if 0
 ToDo: check available heap

  nat req_heap = sizeofW(StgTSO) + MIN_STACK_WORDS;
                 // add this? -- HWL:RtsFlags.ConcFlags.stkChunkSize;
#endif

  IF_DEBUG(gran, fprintf(stderr, "GRAN: doing the Findwork\n"));

  /* If GUM style fishing is enabled, the contents of the spark field says
     what to steal (spark(1) or thread(2)); */
  ASSERT(!(RtsFlags.GranFlags.Fishing && event->spark==(rtsSpark*)0));

  /* Make sure that we have enough heap for creating a new
     thread. This is a conservative estimate of the required heap.
     This eliminates special checks for GC around NewThread within
     ActivateSpark.                                                 */

#if 0
 ToDo: check available heap

  if (Hp + req_heap > HpLim ) {
    IF_DEBUG(gc, 
	     belch("GC: Doing GC from within Findwork handling (that's bloody dangerous if you ask me)");)
      GarbageCollect(GetRoots);
      // ReallyPerformThreadGC(req_heap, rtsFalse);   old -- HWL
      Hp -= req_heap;
      if (procStatus[CurrentProc]==Sparking) 
	procStatus[CurrentProc]=Idle;
      return;
  }
#endif
  
  if ( RtsFlags.GranFlags.DoAlwaysCreateThreads ||
       RtsFlags.GranFlags.Fishing ||
       ((procStatus[proc]==Idle || procStatus[proc]==Sparking) &&
	(RtsFlags.GranFlags.FetchStrategy >= 2 || 
	 OutstandingFetches[proc] == 0)) ) 
   {
    rtsBool found;
    rtsSparkQ  prev, spark;
    
    /* ToDo: check */
    ASSERT(procStatus[proc]==Sparking ||
	   RtsFlags.GranFlags.DoAlwaysCreateThreads ||
	   RtsFlags.GranFlags.Fishing);
    
    /* SImmoHwI' yInej! Search spark queue! */
    /* gimme_spark (event, &found, &spark); */
    findLocalSpark(event, &found, &spark);

    if (!found) { /* pagh vumwI' */
      /*
        If no spark has been found this can mean 2 things:
	 1/ The FindWork was a fish (i.e. a message sent by another PE) and 
	    the spark pool of the receiver is empty
	    --> the fish has to be forwarded to another PE
         2/ The FindWork was local to this PE (i.e. no communication; in this
            case creator==proc) and the spark pool of the PE is not empty 
	    contains only sparks of closures that should not be sparked 
	    (note: if the spark pool were empty, handleIdlePEs wouldn't have 
	    generated a FindWork in the first place)
	    --> the PE has to be made idle to trigger stealing sparks the next
	        time handleIdlePEs is performed
      */ 

      ASSERT(pending_sparks_hds[proc]==(rtsSpark*)NULL);
      if (creator==proc) {
	/* local FindWork */
	if (procStatus[proc]==Busy) {
	  belch("ghuH: PE %d in Busy state while processing local FindWork (spark pool is empty!) @ %lx",
		proc, CurrentTime[proc]);
	  procStatus[proc] = Idle;
	}
      } else {
	/* global FindWork i.e. a Fish */
	ASSERT(RtsFlags.GranFlags.Fishing);
	/* actually this generates another request from the originating PE */
	ASSERT(OutstandingFishes[creator]>0);
	OutstandingFishes[creator]--;
	/* ToDo: assign costs for sending fish to proc not to creator */
	stealSpark(creator); /* might steal from same PE; ToDo: fix */
	ASSERT(RtsFlags.GranFlags.maxFishes!=1 || procStatus[creator] == Fishing);
	/* any assertions on state of proc possible here? */
      }
    } else {
      /* DaH chu' Qu' yIchen! Now create new work! */ 
      IF_GRAN_DEBUG(findWork,
		    belch("+- munching spark %p; creating thread for node %p",
			  spark, spark->node));
      activateSpark (event, spark);
      ASSERT(spark != (rtsSpark*)NULL);
      spark = delete_from_sparkq (spark, proc, rtsTrue);
    }

    IF_GRAN_DEBUG(findWork,
		  belch("+- Contents of spark queues at the end of FindWork @ %lx",
			CurrentTime[proc]); 
		  print_sparkq_stats());

    /* ToDo: check ; not valid if GC occurs in ActivateSpark */
    ASSERT(!found ||
	    /* forward fish  or */
	    (proc!=creator ||
	    /* local spark  or */
            (proc==creator && procStatus[proc]==Starting)) || 
	   //(!found && procStatus[proc]==Idle) ||
	   RtsFlags.GranFlags.DoAlwaysCreateThreads); 
   } else {
    IF_GRAN_DEBUG(findWork,
		  belch("+- RTS refuses to findWork on PE %d @ %lx",
			proc, CurrentTime[proc]);
		  belch("  procStatus[%d]=%s, fetch strategy=%d, outstanding fetches[%d]=%d", 
			proc, proc_status_names[procStatus[proc]],
			RtsFlags.GranFlags.FetchStrategy, 
			proc, OutstandingFetches[proc]));
   }  
}
 
//@node GranSimLight routines, Code for Fetching Nodes, GranSim functions, GranSim specific code
//@subsection GranSimLight routines

/* 
   This code is called from the central scheduler after having rgabbed a
   new event and is only needed for GranSim-Light. It mainly adjusts the
   ActiveTSO so that all costs that have to be assigned from within the
   scheduler are assigned to the right TSO. The choice of ActiveTSO depends
   on the type of event that has been found.  
*/

void
GranSimLight_enter_system(event, ActiveTSOp)
rtsEvent *event;
StgTSO **ActiveTSOp;
{
  StgTSO *ActiveTSO = *ActiveTSOp;

  ASSERT (RtsFlags.GranFlags.Light);
  
  /* Restore local clock of the virtual processor attached to CurrentTSO.
     All costs will be associated to the `virt. proc' on which the tso
     is living. */
  if (ActiveTSO != NULL) {                     /* already in system area */
    ActiveTSO->gran.clock = CurrentTime[CurrentProc];
    if (RtsFlags.GranFlags.DoFairSchedule)
      {
	if (RtsFlags.GranFlags.GranSimStats.Full &&
	    RtsFlags.GranFlags.Debug.checkLight)
	  DumpGranEvent(GR_SYSTEM_END,ActiveTSO);
      }
  }
  switch (event->evttype)
    { 
    case ContinueThread: 
    case FindWork:       /* inaccurate this way */
      ActiveTSO = run_queue_hd;
      break;
    case ResumeThread:   
    case StartThread:
    case MoveSpark:      /* has tso of virt proc in tso field of event */
      ActiveTSO = event->tso;
      break;
    default: barf("Illegal event type %s (%d) in GrAnSim Light setup\n",
		  event_names[event->evttype],event->evttype);
    }
  CurrentTime[CurrentProc] = ActiveTSO->gran.clock;
  if (RtsFlags.GranFlags.DoFairSchedule) {
      if (RtsFlags.GranFlags.GranSimStats.Full &&
	  RtsFlags.GranFlags.Debug.checkLight)
	DumpGranEvent(GR_SYSTEM_START,ActiveTSO);
  }
}

void
GranSimLight_leave_system(event, ActiveTSOp)
rtsEvent *event;
StgTSO **ActiveTSOp;
{
  StgTSO *ActiveTSO = *ActiveTSOp;

  ASSERT(RtsFlags.GranFlags.Light);

  /* Save time of `virt. proc' which was active since last getevent and
     restore time of `virt. proc' where CurrentTSO is living on. */
  if(RtsFlags.GranFlags.DoFairSchedule) {
    if (RtsFlags.GranFlags.GranSimStats.Full &&
	RtsFlags.GranFlags.Debug.checkLight) // ToDo: clean up flags
      DumpGranEvent(GR_SYSTEM_END,ActiveTSO);
  }
  ActiveTSO->gran.clock = CurrentTime[CurrentProc];
  ActiveTSO = (StgTSO*)NULL;
  CurrentTime[CurrentProc] = CurrentTSO->gran.clock;
  if (RtsFlags.GranFlags.DoFairSchedule /* &&  resched */ ) {
    // resched = rtsFalse;
    if (RtsFlags.GranFlags.GranSimStats.Full &&
	RtsFlags.GranFlags.Debug.checkLight)
      DumpGranEvent(GR_SCHEDULE,run_queue_hd);
  }
  /* 
     if (TSO_LINK(ThreadQueueHd)!=PrelBase_Z91Z93_closure &&
     (TimeOfNextEvent == 0 ||
     TSO_CLOCK(TSO_LINK(ThreadQueueHd))+1000<TimeOfNextEvent)) {
     new_event(CurrentProc,CurrentProc,TSO_CLOCK(TSO_LINK(ThreadQueueHd))+1000,
     CONTINUETHREAD,TSO_LINK(ThreadQueueHd),PrelBase_Z91Z93_closure,NULL);
     TimeOfNextEvent = get_time_of_next_event();
     }
  */
}

//@node Code for Fetching Nodes, Idle PEs, GranSimLight routines, GranSim specific code
//@subsection Code for Fetching Nodes

/*
   The following GrAnSim routines simulate the fetching of nodes from a
   remote processor. We use a 1 word bitmask to indicate on which processor
   a node is lying. Thus, moving or copying a node from one processor to
   another just requires an appropriate change in this bitmask (using
   @SET_GA@).  Additionally, the clocks have to be updated.

   A special case arises when the node that is needed by processor A has
   been moved from a processor B to a processor C between sending out a
   @FETCH@ (from A) and its arrival at B. In that case the @FETCH@ has to
   be forwarded to C. This is simulated by issuing another FetchNode event
   on processor C with A as creator.
*/
 
/* ngoqvam che' {GrAnSim}! */

/* Fetch node "node" to processor "p" */

//@cindex fetchNode

rtsFetchReturnCode
fetchNode(node,from,to)
StgClosure* node;
PEs from, to;
{
  /* In case of RtsFlags.GranFlags.DoBulkFetching this fct should never be 
     entered! Instead, UnpackGraph is used in ReSchedule */
  StgClosure* closure;

  ASSERT(to==CurrentProc);
  /* Should never be entered  in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);
  /* fetchNode should never be entered with DoBulkFetching */
  ASSERT(!RtsFlags.GranFlags.DoBulkFetching);

  /* Now fetch the node */
  if (!IS_LOCAL_TO(PROCS(node),from) &&
      !IS_LOCAL_TO(PROCS(node),to) ) 
    return NodeHasMoved;
  
  if (closure_HNF(node))                /* node already in head normal form? */
    node->header.gran.procs |= PE_NUMBER(to);           /* Copy node */
  else
    node->header.gran.procs = PE_NUMBER(to);            /* Move node */

  return Ok;
}

/* 
   Process a fetch request. 
   
   Cost of sending a packet of size n = C + P*n
   where C = packet construction constant, 
         P = cost of packing one word into a packet
   [Should also account for multiple packets].
*/

//@cindex handleFetchRequest

rtsFetchReturnCode
handleFetchRequest(node,to,from,tso)
StgClosure* node;   // the node which is requested
PEs to, from;       // fetch request: from -> to
StgTSO* tso;        // the tso which needs the node
{
  ASSERT(!RtsFlags.GranFlags.Light);
  /* ToDo: check assertion */
  ASSERT(OutstandingFetches[from]>0);

  /* probably wrong place; */
  ASSERT(CurrentProc==to);

  if (IS_LOCAL_TO(PROCS(node), from)) /* Somebody else moved node already => */
    {                                 /* start tso */
      IF_GRAN_DEBUG(thunkStealing,
		    fprintf(stderr,"ghuH: handleFetchRequest entered with local node %p (%s) (PE %d)\n", 
			    node, info_type(node), from));

      if (RtsFlags.GranFlags.DoBulkFetching) {
	nat size;
	rtsPackBuffer *graph;

	/* Create a 1-node-buffer and schedule a FETCHREPLY now */
	graph = PackOneNode(node, tso, &size); 
	new_event(from, to, CurrentTime[to],
		  FetchReply,
		  tso, (StgClosure *)graph, (rtsSpark*)NULL);
      } else {
	new_event(from, to, CurrentTime[to],
		  FetchReply,
		  tso, node, (rtsSpark*)NULL);
      }
      IF_GRAN_DEBUG(thunkStealing,
		    belch("== majQa'! closure %p is local on PE %d already (this is a good thing)", node, from));
      return (NodeIsLocal);
    }
  else if (IS_LOCAL_TO(PROCS(node), to) )   /* Is node still here? */
    {
      if (RtsFlags.GranFlags.DoBulkFetching) { /* {GUM}vo' ngoqvam vInIHta' */
	nat size;                              /* (code from GUM) */
	StgClosure* graph;

	if (IS_BLACK_HOLE(node)) {   /* block on BH or RBH */
	  new_event(from, to, CurrentTime[to],
		    GlobalBlock,
		    tso, node, (rtsSpark*)NULL);
	  /* Note: blockFetch is done when handling GLOBALBLOCK event; 
	           make sure the TSO stays out of the run queue */
          /* When this thread is reawoken it does the usual: it tries to 
             enter the updated node and issues a fetch if it's remote.
             It has forgotten that it has sent a fetch already (i.e. a
             FETCHNODE is swallowed by a BH, leaving the thread in a BQ) */
          --OutstandingFetches[from];

	  IF_GRAN_DEBUG(thunkStealing,
			belch("== majQa'! closure %p on PE %d is a BH (demander=PE %d); faking a FMBQ", 
			      node, to, from));
	  if (RtsFlags.GranFlags.GranSimStats.Global) {
	    globalGranStats.tot_FMBQs++;
	  }
	  return (NodeIsBH);
	}

	/* The tso requesting the node is blocked and cannot be on a run queue */
	ASSERT(!is_on_queue(tso, from));
	
	// ToDo: check whether graph is ever used as an rtsPackBuffer!!
	if ((graph = (StgClosure *)PackNearbyGraph(node, tso, &size, 0)) == NULL) 
	  return (OutOfHeap);  /* out of heap */

	/* Actual moving/copying of node is done on arrival; see FETCHREPLY */
	/* Send a reply to the originator */
	/* ToDo: Replace that by software costs for doing graph packing! */
	CurrentTime[to] += size * RtsFlags.GranFlags.Costs.mpacktime;

	new_event(from, to,
		  CurrentTime[to]+RtsFlags.GranFlags.Costs.latency,
		  FetchReply,
		  tso, (StgClosure *)graph, (rtsSpark*)NULL);
        
	CurrentTime[to] += RtsFlags.GranFlags.Costs.mtidytime;
	return (Ok);
      } else {                   /* incremental (single closure) fetching */
	/* Actual moving/copying of node is done on arrival; see FETCHREPLY */
	/* Send a reply to the originator */
	CurrentTime[to] += RtsFlags.GranFlags.Costs.mpacktime;

	new_event(from, to,
		  CurrentTime[to]+RtsFlags.GranFlags.Costs.latency,
		  FetchReply,
		  tso, node, (rtsSpark*)NULL);
      
	CurrentTime[to] += RtsFlags.GranFlags.Costs.mtidytime;
	return (Ok);
      }
    }
  else       /* Qu'vatlh! node has been grabbed by another proc => forward */
    {    
      PEs node_loc = where_is(node);
      rtsTime fetchtime;

      IF_GRAN_DEBUG(thunkStealing,
		    belch("== Qu'vatlh! node %p has been grabbed by PE %d from PE %d (demander=%d) @ %d\n",
			  node,node_loc,to,from,CurrentTime[to]));
      if (RtsFlags.GranFlags.GranSimStats.Global) {
	globalGranStats.fetch_misses++;
      }

      /* Prepare FORWARD message to proc p_new */
      CurrentTime[to] += RtsFlags.GranFlags.Costs.mpacktime;
      
      fetchtime = stg_max(CurrentTime[to], CurrentTime[node_loc]) +
                  RtsFlags.GranFlags.Costs.latency;
          
      new_event(node_loc, from, fetchtime,
		FetchNode,
		tso, node, (rtsSpark*)NULL);

      CurrentTime[to] += RtsFlags.GranFlags.Costs.mtidytime;

      return (NodeHasMoved);
    }
}

/*
   blockFetch blocks a BlockedFetch node on some kind of black hole.

   Taken from gum/HLComms.lc.   [find a  better  place for that ?] --  HWL  

   {\bf Note:} In GranSim we don't have @FETCHME@ nodes and therefore don't
   create @FMBQ@'s (FetchMe blocking queues) to cope with global
   blocking. Instead, non-local TSO are put into the BQ in the same way as
   local TSOs. However, we have to check if a TSO is local or global in
   order to account for the latencies involved and for keeping track of the
   number of fetches that are really going on.  
*/

//@cindex blockFetch

rtsFetchReturnCode
blockFetch(tso, proc, bh)
StgTSO* tso;                        /* TSO which gets blocked */
PEs proc;                           /* PE where that tso was running */
StgClosure* bh;                     /* closure to block on (BH, RBH, BQ) */
{
  StgInfoTable *info;

  IF_GRAN_DEBUG(bq,
		fprintf(stderr,"## blockFetch: blocking TSO %p (%d)[PE %d] on node %p (%s) [PE %d]. No graph is packed!\n", 
		tso, tso->id, proc, bh, info_type(bh), where_is(bh)));

    if (!IS_BLACK_HOLE(bh)) {                      /* catches BHs and RBHs */
      IF_GRAN_DEBUG(bq,
		    fprintf(stderr,"## blockFetch: node %p (%s) is not a BH => awakening TSO %p (%d) [PE %u]\n", 
			    bh, info_type(bh), tso, tso->id, proc));

      /* No BH anymore => immediately unblock tso */
      new_event(proc, proc, CurrentTime[proc],
	        UnblockThread,
                tso, bh, (rtsSpark*)NULL);

      /* Is this always a REPLY to a FETCH in the profile ? */
      if (RtsFlags.GranFlags.GranSimStats.Full)
	DumpRawGranEvent(proc, proc, GR_REPLY, tso, bh, (StgInt)0, 0);
      return (NodeIsNoBH);
    }

    /* DaH {BQ}Daq Qu' Suq 'e' wISov!
       Now we know that we have to put the tso into the BQ.
       2 cases: If block-on-fetch, tso is at head of threadq => 
                => take it out of threadq and into BQ
                If reschedule-on-fetch, tso is only pointed to be event
                => just put it into BQ

    ngoq ngo'!!
    if (!RtsFlags.GranFlags.DoAsyncFetch) {
      GranSimBlock(tso, proc, bh);
    } else {
      if (RtsFlags.GranFlags.GranSimStats.Full)
	DumpRawGranEvent(proc, where_is(bh), GR_BLOCK, tso, bh, (StgInt)0, 0);
      ++(tso->gran.blockcount);
      tso->gran.blockedat = CurrentTime[proc];
    }
    */

    /* after scheduling the GlobalBlock event the TSO is not put into the
       run queue again; it is only pointed to via the event we are
       processing now; in GranSim 4.xx there is no difference between
       synchr and asynchr comm here */
    ASSERT(!is_on_queue(tso, proc));
    ASSERT(tso->link == END_TSO_QUEUE);

    GranSimBlock(tso, proc, bh);  /* GranSim statistics gathering */

    /* Now, put tso into BQ (similar to blocking entry codes) */
    info = get_itbl(bh);
    switch (info -> type) {
      case RBH:
      case BLACKHOLE:
      case CAF_BLACKHOLE: // ToDo: check whether this is a possibly ITBL here
      case SE_BLACKHOLE:   // ToDo: check whether this is a possibly ITBL here
      case SE_CAF_BLACKHOLE:// ToDo: check whether this is a possibly ITBL here
	/* basically an inlined version of BLACKHOLE_entry -- HWL */
	/* Change the BLACKHOLE into a BLACKHOLE_BQ */
	((StgBlockingQueue *)bh)->header.info = &BLACKHOLE_BQ_info;
	/* Put ourselves on the blocking queue for this black hole */
	// tso->link=END_TSO_QUEUE;   not necessary; see assertion above
	((StgBlockingQueue *)bh)->blocking_queue = (StgBlockingQueueElement *)tso;
	tso->block_info.closure = bh;
	recordMutable((StgMutClosure *)bh);
	break;

    case BLACKHOLE_BQ:
	/* basically an inlined version of BLACKHOLE_BQ_entry -- HWL */
	tso->link = (StgTSO *) (((StgBlockingQueue*)bh)->blocking_queue); 
	((StgBlockingQueue*)bh)->blocking_queue = (StgBlockingQueueElement *)tso;
	recordMutable((StgMutClosure *)bh);

# if 0 && defined(GC_MUT_REQUIRED)
	ToDo: check whether recordMutable is necessary -- HWL
	/*
	 * If we modify a black hole in the old generation, we have to make 
	 * sure it goes on the mutables list
	 */

	if (bh <= StorageMgrInfo.OldLim) {
	    MUT_LINK(bh) = (W_) StorageMgrInfo.OldMutables;
	    StorageMgrInfo.OldMutables = bh;
	} else
	    MUT_LINK(bh) = MUT_NOT_LINKED;
# endif
	break;

    case FETCH_ME_BQ:
	barf("Qagh: FMBQ closure (%p) found in GrAnSim (TSO=%p (%d))\n",
	     bh, tso, tso->id);

    default:
	{
	  G_PRINT_NODE(bh);
	  barf("Qagh: thought %p was a black hole (IP %p (%s))",
		  bh, info, info_type(bh));
	}
      }
    return (Ok);
}


//@node Idle PEs, Routines directly called from Haskell world, Code for Fetching Nodes, GranSim specific code
//@subsection Idle PEs

/*
   Export work to idle PEs. This function is called from @ReSchedule@
   before dispatching on the current event. @HandleIdlePEs@ iterates over
   all PEs, trying to get work for idle PEs. Note, that this is a
   simplification compared to GUM's fishing model. We try to compensate for
   that by making the cost for stealing work dependent on the number of
   idle processors and thereby on the probability with which a randomly
   sent fish would find work.  
*/

//@cindex handleIdlePEs

void
handleIdlePEs(void)
{
  PEs p;

  IF_DEBUG(gran, fprintf(stderr, "GRAN: handling Idle PEs\n"))

  /* Should never be entered in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);

  /* Could check whether there are idle PEs if it's a cheap check */
  for (p = 0; p < RtsFlags.GranFlags.proc; p++) 
    if (procStatus[p]==Idle)  /*  && IS_SPARKING(p) && IS_STARTING(p) */
      /* First look for local work i.e. examine local spark pool! */
      if (pending_sparks_hds[p]!=(rtsSpark *)NULL) {
	new_event(p, p, CurrentTime[p],
		  FindWork,
		  (StgTSO*)NULL, (StgClosure*)NULL, (rtsSpark*)NULL);
	procStatus[p] = Sparking;
      } else if ((RtsFlags.GranFlags.maxFishes==0 ||
		  OutstandingFishes[p]<RtsFlags.GranFlags.maxFishes) ) {

	/* If no local work then try to get remote work! 
	   Qu' Hopbe' pagh tu'lu'pu'chugh Qu' Hop yISuq ! */
	if (RtsFlags.GranFlags.DoStealThreadsFirst && 
	    (RtsFlags.GranFlags.FetchStrategy >= 4 || OutstandingFetches[p] == 0))
	  {
	    if (SurplusThreads > 0l)                    /* Steal a thread */
	      stealThread(p);
          
	    if (procStatus[p]!=Idle)
	      break;
	  }
	
	if (SparksAvail > 0 && 
	    (RtsFlags.GranFlags.FetchStrategy >= 3 || OutstandingFetches[p] == 0)) /* Steal a spark */
	  stealSpark(p);
	
	if (SurplusThreads > 0 && 
	    (RtsFlags.GranFlags.FetchStrategy >= 4 || OutstandingFetches[p] == 0)) /* Steal a thread */
	  stealThread(p);
      }
}

/*
   Steal a spark and schedule moving it to proc. We want to look at PEs in
   clock order -- most retarded first.  Currently sparks are only stolen
   from the @ADVISORY_POOL@ never from the @REQUIRED_POOL@. Eventually,
   this should be changed to first steal from the former then from the
   latter.

   We model a sort of fishing mechanism by counting the number of sparks
   and threads we are currently stealing.  */

/* 
   Return a random nat value in the intervall [from, to) 
*/
static nat 
natRandom(from, to)
nat from, to;
{
  nat r, d;

  ASSERT(from<=to);
  d = to - from;
  /* random returns a value in [0, RAND_MAX] */
  r = (nat) ((float)from + ((float)random()*(float)d)/(float)RAND_MAX);
  r = (r==to) ? from : r;
  ASSERT(from<=r && (r<to || from==to));
  return r;  
}

/* 
   Find any PE other than proc. Used for GUM style fishing only.
*/
static PEs 
findRandomPE (proc)
PEs proc;
{
  nat p;

  ASSERT(RtsFlags.GranFlags.Fishing);
  if (RtsFlags.GranFlags.RandomSteal) {
    p = natRandom(0,RtsFlags.GranFlags.proc);  /* full range of PEs */
  } else {
    p = 0;
  }
  IF_GRAN_DEBUG(randomSteal,
		belch("^^ RANDOM_STEAL (fishing): stealing from PE %d (current proc is %d)",
		      p, proc));
    
  return (PEs)p;
}

/*
  Magic code for stealing sparks/threads makes use of global knowledge on
  spark queues.  
*/
static void
sortPEsByTime (proc, pes_by_time, firstp, np) 
PEs proc;
PEs *pes_by_time;
nat *firstp, *np;
{
  PEs p, temp, n, i, j;
  nat first, upb, r=0, q=0;

  ASSERT(!RtsFlags.GranFlags.Fishing);

#if 0  
  upb = RtsFlags.GranFlags.proc;            /* full range of PEs */

  if (RtsFlags.GranFlags.RandomSteal) {
    r = natRandom(0,RtsFlags.GranFlags.proc);  /* full range of PEs */
  } else {
    r = 0;
  }
#endif

  /* pes_by_time shall contain processors from which we may steal sparks */ 
  for(n=0, p=0; p < RtsFlags.GranFlags.proc; ++p)
    if ((proc != p) &&                       // not the current proc
        (pending_sparks_hds[p] != (rtsSpark *)NULL) && // non-empty spark pool
        (CurrentTime[p] <= CurrentTime[CurrentProc]))
      pes_by_time[n++] = p;

  /* sort pes_by_time */
  for(i=0; i < n; ++i)
    for(j=i+1; j < n; ++j)
      if (CurrentTime[pes_by_time[i]] > CurrentTime[pes_by_time[j]]) {
	rtsTime temp = pes_by_time[i];
	pes_by_time[i] = pes_by_time[j];
	pes_by_time[j] = temp;
      }

  /* Choose random processor to steal spark from; first look at processors */
  /* that are earlier than the current one (i.e. proc) */
  for(first=0; 
      (first < n) && (CurrentTime[pes_by_time[first]] <= CurrentTime[proc]);
      ++first)
    /* nothing */ ;

  /* if the assertion below is true we can get rid of first */
  /* ASSERT(first==n); */
  /* ToDo: check if first is really needed; find cleaner solution */

  *firstp = first;
  *np = n;
}

/* 
   Steal a spark (piece of work) from any processor and bring it to proc.
*/
//@cindex stealSpark
static rtsBool 
stealSpark(PEs proc) { stealSomething(proc, rtsTrue, rtsFalse); }

/* 
   Steal a thread from any processor and bring it to proc i.e. thread migration
*/
//@cindex stealThread
static rtsBool 
stealThread(PEs proc) { stealSomething(proc, rtsFalse, rtsTrue); }

/* 
   Steal a spark or a thread and schedule moving it to proc.
*/
//@cindex stealSomething
static rtsBool
stealSomething(proc, steal_spark, steal_thread)
PEs proc;                           // PE that needs work (stealer)
rtsBool steal_spark, steal_thread;  // should a spark and/or thread be stolen
{
  PEs p;
  rtsTime fish_arrival_time;
  rtsSpark *spark, *prev, *next;
  rtsBool stolen = rtsFalse;

  ASSERT(steal_spark || steal_thread);

  /* Should never be entered in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);
  ASSERT(!steal_thread || RtsFlags.GranFlags.DoThreadMigration);

  if (!RtsFlags.GranFlags.Fishing) {
    // ToDo: check if stealing threads is prefered over stealing sparks
    if (steal_spark) {
      if (stealSparkMagic(proc))
	return rtsTrue;
      else                             // no spark found
	if (steal_thread)
	  return stealThreadMagic(proc);
        else                           // no thread found
	  return rtsFalse;             
    } else {                           // ASSERT(steal_thread);
      return stealThreadMagic(proc);
    }
    barf("stealSomething: never reached");
  }

  /* The rest of this function does GUM style fishing */
  
  p = findRandomPE(proc); /* find a random PE other than proc */
  
  /* Message packing costs for sending a Fish; qeq jabbI'ID */
  CurrentTime[proc] += RtsFlags.GranFlags.Costs.mpacktime;
  
  /* use another GranEvent for requesting a thread? */
  if (steal_spark && RtsFlags.GranFlags.GranSimStats.Sparks)
    DumpRawGranEvent(p, proc, SP_REQUESTED,
		     (StgTSO*)NULL, (StgClosure *)NULL, (StgInt)0, 0);

  /* time of the fish arrival on the remote PE */
  fish_arrival_time = CurrentTime[proc] + RtsFlags.GranFlags.Costs.latency;
  
  /* Phps use an own Fish event for that? */
  /* The contents of the spark component is a HACK:
      1 means give me a spark;
      2 means give me a thread
      0 means give me nothing (this should never happen)
  */
  new_event(p, proc, fish_arrival_time,
	    FindWork,
	    (StgTSO*)NULL, (StgClosure*)NULL, 
	    (steal_spark ? (rtsSpark*)1 : steal_thread ? (rtsSpark*)2 : (rtsSpark*)0));
  
  ++OutstandingFishes[proc];
  /* only with Async fetching? */
  if (procStatus[proc]==Idle)  
    procStatus[proc]=Fishing;
  
  /* time needed to clean up buffers etc after sending a message */
  CurrentTime[proc] += RtsFlags.GranFlags.Costs.mtidytime;

  /* If GUM style fishing stealing always succeeds because it only consists
     of sending out a fish; of course, when the fish may return
     empty-handed! */
  return rtsTrue;
}

/* 
   This version of stealing a spark makes use of the global info on all
   spark pools etc which is not available in a real parallel system.
   This could be extended to test e.g. the impact of perfect load information.
*/
//@cindex stealSparkMagic
static rtsBool
stealSparkMagic(proc)
PEs proc;
{
  PEs p=0, i=0, j=0, n=0, first, upb;
  rtsSpark *spark=NULL, *next;
  PEs pes_by_time[MAX_PROC];
  rtsBool stolen = rtsFalse;
  rtsTime stealtime;

  /* Should never be entered in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);

  sortPEsByTime(proc, pes_by_time, &first, &n);

  while (!stolen && n>0) {
    upb = (first==0) ? n : first;
    i = natRandom(0,upb);                /* choose a random eligible PE */
    p = pes_by_time[i];

    IF_GRAN_DEBUG(randomSteal,
		  belch("^^ stealSparkMagic (random_steal, not fishing): stealing spark from PE %d (current proc is %d)",
			p, proc));
      
    ASSERT(pending_sparks_hds[p]!=(rtsSpark *)NULL); /* non-empty spark pool */

    /* Now go through rtsSparkQ and steal the first eligible spark */
    
    spark = pending_sparks_hds[p]; 
    while (!stolen && spark != (rtsSpark*)NULL)
      {
	/* NB: no prev pointer is needed here because all sparks that are not 
	   chosen are pruned
	*/
	if ((procStatus[p]==Idle || procStatus[p]==Sparking || procStatus[p] == Fishing) &&
	    spark->next==(rtsSpark*)NULL) 
	  {
	    /* Be social! Don't steal the only spark of an idle processor 
	       not {spark} neH yInIH !! */
	    break; /* next PE */
	  } 
	else if (closure_SHOULD_SPARK(spark->node))
	  {
	    /* Don't Steal local sparks; 
	       ToDo: optionally prefer local over global sparks
	    if (!spark->global) {
	      prev=spark;
	      continue;                  next spark
	    }
	    */
	    /* found a spark! */

	    /* Prepare message for sending spark */
	    CurrentTime[p] += RtsFlags.GranFlags.Costs.mpacktime;

	    if (RtsFlags.GranFlags.GranSimStats.Sparks)
	      DumpRawGranEvent(p, (PEs)0, SP_EXPORTED,
			       (StgTSO*)NULL, spark->node,
			       spark->name, spark_queue_len(p));

	    stealtime = (CurrentTime[p] > CurrentTime[proc] ? 
			   CurrentTime[p] : 
			   CurrentTime[proc])
	                + sparkStealTime();

	    new_event(proc, p /* CurrentProc */, stealtime,
		      MoveSpark,
		      (StgTSO*)NULL, spark->node, spark);
	    
	    stolen = rtsTrue;
	    ++OutstandingFishes[proc]; /* no. of sparks currently on the fly */
	    if (procStatus[proc]==Idle)
	      procStatus[proc] = Fishing;
	    ++(spark->global);         /* record that this is a global spark */
	    ASSERT(SparksAvail>0);
	    --SparksAvail;            /* on-the-fly sparks are not available */
	    next = delete_from_sparkq(spark, p, rtsFalse); // don't dispose!
	    CurrentTime[p] += RtsFlags.GranFlags.Costs.mtidytime;
	  }
	else   /* !(closure_SHOULD_SPARK(SPARK_NODE(spark))) */
	  {
	   IF_GRAN_DEBUG(checkSparkQ,
			 belch("^^ pruning spark %p (node %p) in stealSparkMagic",
			       spark, spark->node));

	    /* if the spark points to a node that should not be sparked,
	       prune the spark queue at this point */
	    if (RtsFlags.GranFlags.GranSimStats.Sparks)
	      DumpRawGranEvent(p, (PEs)0, SP_PRUNED,
			       (StgTSO*)NULL, spark->node,
			       spark->name, spark_queue_len(p));
	    if (RtsFlags.GranFlags.GranSimStats.Global)
	      globalGranStats.pruned_sparks++;
	    
	    ASSERT(SparksAvail>0);
	    --SparksAvail;
	    spark = delete_from_sparkq(spark, p, rtsTrue);
	  }
	/* unlink spark (may have been freed!) from sparkq;
	if (prev == NULL) // spark was head of spark queue
	  pending_sparks_hds[p] = spark->next;
        else  
	  prev->next = spark->next;
	if (spark->next == NULL)
	  pending_sparks_tls[p] = prev;
        else  
	  next->prev = prev;
	*/
      }                    /* while ...    iterating over sparkq */

    /* ToDo: assert that PE p still has work left after stealing the spark */

    if (!stolen && (n>0)) {  /* nothing stealable from proc p :( */
      ASSERT(pes_by_time[i]==p);

      /* remove p from the list (at pos i) */
      for (j=i; j+1<n; j++)
	pes_by_time[j] = pes_by_time[j+1];
      n--;
      
      /* update index to first proc which is later (or equal) than proc */
      for ( ;
	    (first>0) &&
	      (CurrentTime[pes_by_time[first-1]]>CurrentTime[proc]);
	    first--)
	/* nothing */ ;
    } 
  }  /* while ... iterating over PEs in pes_by_time */

  IF_GRAN_DEBUG(randomSteal,
		if (stolen)
		  belch("^^ stealSparkMagic: spark %p (node=%p) stolen by PE %d from PE %d (SparksAvail=%d; idlers=%d)",
		       spark, spark->node, proc, p, 
		       SparksAvail, idlers());
		else  
		  belch("^^ stealSparkMagic: nothing stolen by PE %d (sparkq len after pruning=%d)(SparksAvail=%d; idlers=%d)",
		        proc, SparksAvail, idlers()));

  if (RtsFlags.GranFlags.GranSimStats.Global &&
      stolen && (i!=0)) {                          /* only for statistics */
    globalGranStats.rs_sp_count++;
    globalGranStats.ntimes_total += n;
    globalGranStats.fl_total += first;
    globalGranStats.no_of_steals++;
  }

  return stolen;
}

/* 
   The old stealThread code, which makes use of global info and does not
   send out fishes.  
   NB: most of this is the same as in stealSparkMagic;
       only the pieces specific to processing thread queues are different; 
       long live polymorphism!  
*/

//@cindex stealThreadMagic
static rtsBool
stealThreadMagic(proc)
PEs proc;
{
  PEs p=0, i=0, j=0, n=0, first, upb;
  StgTSO *tso=END_TSO_QUEUE;
  PEs pes_by_time[MAX_PROC];
  rtsBool stolen = rtsFalse;
  rtsTime stealtime;

  /* Should never be entered in GrAnSim Light setup */
  ASSERT(!RtsFlags.GranFlags.Light);

  sortPEsByTime(proc, pes_by_time, &first, &n);

  while (!stolen && n>0) {
    upb = (first==0) ? n : first;
    i = natRandom(0,upb);                /* choose a random eligible PE */
    p = pes_by_time[i];

    IF_GRAN_DEBUG(randomSteal,
		  belch("^^ stealThreadMagic (random_steal, not fishing): stealing thread from PE %d (current proc is %d)",
			p, proc));
      
    /* Steal the first exportable thread in the runnable queue but
       never steal the first in the queue for social reasons;
       not Qu' wa'DIch yInIH !!
    */
    /* Would be better to search through queue and have options which of
       the threads to pick when stealing */
    if (run_queue_hds[p] == END_TSO_QUEUE) {
      IF_GRAN_DEBUG(randomSteal,
		    belch("^^ stealThreadMagic: No thread to steal from PE %d (stealer=PE %d)", 
			  p, proc));
    } else {
      tso = run_queue_hds[p]->link;  /* tso is *2nd* thread in thread queue */
      /* Found one */
      stolen = rtsTrue;

      /* update links in queue */
      run_queue_hds[p]->link = tso->link;
      if (run_queue_tls[p] == tso)
	run_queue_tls[p] = run_queue_hds[p];
      
      /* ToDo: Turn magic constants into params */
      
      CurrentTime[p] += 5l * RtsFlags.GranFlags.Costs.mpacktime;
      
      stealtime = (CurrentTime[p] > CurrentTime[proc] ? 
		   CurrentTime[p] : 
		   CurrentTime[proc])
	+ sparkStealTime() 
	+ 4l * RtsFlags.GranFlags.Costs.additional_latency
	+ 5l * RtsFlags.GranFlags.Costs.munpacktime;

      /* Move the thread; set bitmask to 0 while TSO is `on-the-fly' */
      SET_GRAN_HDR(tso,Nowhere /* PE_NUMBER(proc) */); 

      /* Move from one queue to another */
      new_event(proc, p, stealtime,
		MoveThread,
		tso, (StgClosure*)NULL, (rtsSpark*)NULL);

      /* MAKE_BUSY(proc);  not yet; only when thread is in threadq */
      ++OutstandingFishes[proc];
      if (procStatus[proc])
	procStatus[proc] = Fishing;
      --SurplusThreads;

      if(RtsFlags.GranFlags.GranSimStats.Full)
	DumpRawGranEvent(p, proc, 
			 GR_STEALING, 
			 tso, (StgClosure*)NULL, (StgInt)0, 0);
      
      /* costs for tidying up buffer after having sent it */
      CurrentTime[p] += 5l * RtsFlags.GranFlags.Costs.mtidytime;
    }

    /* ToDo: assert that PE p still has work left after stealing the spark */

    if (!stolen && (n>0)) {  /* nothing stealable from proc p :( */
      ASSERT(pes_by_time[i]==p);

      /* remove p from the list (at pos i) */
      for (j=i; j+1<n; j++)
	pes_by_time[j] = pes_by_time[j+1];
      n--;
      
      /* update index to first proc which is later (or equal) than proc */
      for ( ;
	    (first>0) &&
	      (CurrentTime[pes_by_time[first-1]]>CurrentTime[proc]);
	    first--)
	/* nothing */ ;
    } 
  }  /* while ... iterating over PEs in pes_by_time */

  IF_GRAN_DEBUG(randomSteal,
		if (stolen)
  		  belch("^^ stealThreadMagic: stolen TSO %d (%p) by PE %d from PE %d (SparksAvail=%d; idlers=%d)",
		        tso->id, tso, proc, p,
		        SparksAvail, idlers());
		else
		  belch("stealThreadMagic: nothing stolen by PE %d (SparksAvail=%d; idlers=%d)",
			proc, SparksAvail, idlers()));

  if (RtsFlags.GranFlags.GranSimStats.Global &&
      stolen && (i!=0)) { /* only for statistics */
    /* ToDo: more statistics on avg thread queue lenght etc */
    globalGranStats.rs_t_count++;
    globalGranStats.no_of_migrates++;
  }

  return stolen;
}

//@cindex sparkStealTime
static rtsTime
sparkStealTime(void)
{
  double fishdelay, sparkdelay, latencydelay;
  fishdelay =  (double)RtsFlags.GranFlags.proc/2;
  sparkdelay = fishdelay - 
          ((fishdelay-1.0)/(double)(RtsFlags.GranFlags.proc-1))*((double)idlers());
  latencydelay = sparkdelay*((double)RtsFlags.GranFlags.Costs.latency);

  return((rtsTime)latencydelay);
}

//@node Routines directly called from Haskell world, Emiting profiling info for GrAnSim, Idle PEs, GranSim specific code
//@subsection Routines directly called from Haskell world
/* 
The @GranSim...@ routines in here are directly called via macros from the
threaded world. 

First some auxiliary routines.
*/

/* Take the current thread off the thread queue and thereby activate the 
   next thread. It's assumed that the next ReSchedule after this uses 
   NEW_THREAD as param. 
   This fct is called from GranSimBlock and GranSimFetch 
*/

//@cindex ActivateNextThread

void 
ActivateNextThread (proc)
PEs proc;
{
  StgTSO *t;
  /*
    This routine is entered either via GranSimFetch or via GranSimBlock.
    It has to prepare the CurrentTSO for being blocked and update the
    run queue and other statistics on PE proc. The actual enqueuing to the 
    blocking queue (if coming from GranSimBlock) is done in the entry code 
    of the BLACKHOLE and BLACKHOLE_BQ closures (see StgMiscClosures.hc).
  */
  /* ToDo: add assertions here!! */
  //ASSERT(run_queue_hds[proc]!=END_TSO_QUEUE);

  // Only necessary if the running thread is at front of the queue
  // run_queue_hds[proc] = run_queue_hds[proc]->link;
  ASSERT(CurrentProc==proc);
  ASSERT(!is_on_queue(CurrentTSO,proc));
  if (run_queue_hds[proc]==END_TSO_QUEUE) {
    /* NB: this routine is only entered with asynchr comm (see assertion) */
    procStatus[proc] = Idle;
  } else {
    /* ToDo: check cost assignment */
    CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadcontextswitchtime;
    if (RtsFlags.GranFlags.GranSimStats.Full && 
	(!RtsFlags.GranFlags.Light || RtsFlags.GranFlags.Debug.checkLight)) 
                                      /* right flag !?? ^^^ */ 
      DumpRawGranEvent(proc, 0, GR_SCHEDULE, run_queue_hds[proc],
                       (StgClosure*)NULL, (StgInt)0, 0);
  }
}

/* 
   The following GranSim fcts are stg-called from the threaded world.    
*/

/* Called from HP_CHK and friends (see StgMacros.h)  */
//@cindex GranSimAllocate
void 
GranSimAllocate(n)
StgInt n;
{
  CurrentTSO->gran.allocs += n;
  ++(CurrentTSO->gran.basicblocks);

  if (RtsFlags.GranFlags.GranSimStats.Heap) {
      DumpRawGranEvent(CurrentProc, 0, GR_ALLOC, CurrentTSO,
                       (StgClosure*)NULL, (StgInt)0, n);
  }
  
  CurrentTSO->gran.exectime += RtsFlags.GranFlags.Costs.heapalloc_cost;
  CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.heapalloc_cost;
}

/*
  Subtract the values added above, if a heap check fails and
  so has to be redone.
*/
//@cindex GranSimUnallocate
void 
GranSimUnallocate(n)
StgInt n;
{
  CurrentTSO->gran.allocs -= n;
  --(CurrentTSO->gran.basicblocks);
  
  CurrentTSO->gran.exectime -= RtsFlags.GranFlags.Costs.heapalloc_cost;
  CurrentTime[CurrentProc] -= RtsFlags.GranFlags.Costs.heapalloc_cost;
}

/* NB: We now inline this code via GRAN_EXEC rather than calling this fct */
//@cindex GranSimExec
void 
GranSimExec(ariths,branches,loads,stores,floats)
StgWord ariths,branches,loads,stores,floats;
{
  StgWord cost = RtsFlags.GranFlags.Costs.arith_cost*ariths + 
            RtsFlags.GranFlags.Costs.branch_cost*branches + 
            RtsFlags.GranFlags.Costs.load_cost * loads +
            RtsFlags.GranFlags.Costs.store_cost*stores + 
            RtsFlags.GranFlags.Costs.float_cost*floats;

  CurrentTSO->gran.exectime += cost;
  CurrentTime[CurrentProc] += cost;
}

/* 
   Fetch the node if it isn't local
   -- result indicates whether fetch has been done.

   This is GRIP-style single item fetching.
*/

//@cindex GranSimFetch
StgInt 
GranSimFetch(node /* , liveness_mask */ )
StgClosure *node;
/* StgInt liveness_mask; */
{
  /* reset the return value (to be checked within STG land) */
  NeedToReSchedule = rtsFalse;   

  if (RtsFlags.GranFlags.Light) {
     /* Always reschedule in GrAnSim-Light to prevent one TSO from
        running off too far 
     new_event(CurrentProc,CurrentProc,CurrentTime[CurrentProc],
	      ContinueThread,CurrentTSO,node,NULL);
     */
     return(0); 
  }

  /* Faking an RBH closure:
     If the bitmask of the closure is 0 then this node is a fake RBH;
  */
  if (node->header.gran.procs == Nowhere) {
    IF_GRAN_DEBUG(bq,
		  belch("## Found fake RBH (node %p); delaying TSO %d (%p)", 
			node, CurrentTSO->id, CurrentTSO));
		  
    new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc]+10000,
	      ContinueThread, CurrentTSO, node, (rtsSpark*)NULL);

    /* Rescheduling (GranSim internal) is necessary */
    NeedToReSchedule = rtsTrue;
    
    return(1); 
  }

  /* Note: once a node has been fetched, this test will be passed */
  if (!IS_LOCAL_TO(PROCS(node),CurrentProc))
    {
      PEs p = where_is(node);
      rtsTime fetchtime;
      
      IF_GRAN_DEBUG(thunkStealing,
		    if (p==CurrentProc) 
		      belch("GranSimFetch: Trying to fetch from own processor%u\n", p););
      
      CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mpacktime;
      /* NB: Fetch is counted on arrival (FetchReply) */
      
      fetchtime = stg_max(CurrentTime[CurrentProc],CurrentTime[p]) +
	RtsFlags.GranFlags.Costs.latency;
      
      new_event(p, CurrentProc, fetchtime,
		FetchNode, CurrentTSO, node, (rtsSpark*)NULL);
      
      if (fetchtime<TimeOfNextEvent)
	TimeOfNextEvent = fetchtime;
      
      /* About to block */
      CurrentTSO->gran.blockedat = CurrentTime[CurrentProc];
      
      ++OutstandingFetches[CurrentProc];
      
      if (RtsFlags.GranFlags.DoAsyncFetch) 
	/* if asynchr comm is turned on, activate the next thread in the q */
	ActivateNextThread(CurrentProc);
      else
	procStatus[CurrentProc] = Fetching;

#if 0 
      /* ToDo: nuke the entire if (anything special for fair schedule?) */
      if (RtsFlags.GranFlags.DoAsyncFetch) 
	{
	  /* Remove CurrentTSO from the queue -- assumes head of queue == CurrentTSO */
	  if(!RtsFlags.GranFlags.DoFairSchedule)
	    {
	      /* now done in do_the_fetchnode 
	      if (RtsFlags.GranFlags.GranSimStats.Full)
		DumpRawGranEvent(CurrentProc, p, GR_FETCH, CurrentTSO,
				 node, (StgInt)0, 0);
	      */				
	      ActivateNextThread(CurrentProc);
              
# if 0 && defined(GRAN_CHECK)
	      if (RtsFlags.GranFlags.Debug.blockOnFetch_sanity) {
		if (TSO_TYPE(CurrentTSO) & FETCH_MASK_TSO) {
		  fprintf(stderr,"FetchNode: TSO 0x%x has fetch-mask set @ %d\n",
			  CurrentTSO,CurrentTime[CurrentProc]);
		  stg_exit(EXIT_FAILURE);
		} else {
		  TSO_TYPE(CurrentTSO) |= FETCH_MASK_TSO;
		}
	      }
# endif
	      CurrentTSO->link = END_TSO_QUEUE;
	      /* CurrentTSO = END_TSO_QUEUE; */
	      
	      /* CurrentTSO is pointed to by the FetchNode event; it is
		 on no run queue any more */
	  } else {  /* fair scheduling currently not supported -- HWL */
	    barf("Asynchr communication is not yet compatible with fair scheduling\n");
	  }
	} else {                /* !RtsFlags.GranFlags.DoAsyncFetch */
	  procStatus[CurrentProc] = Fetching; // ToDo: BlockedOnFetch;
	  /* now done in do_the_fetchnode 
	  if (RtsFlags.GranFlags.GranSimStats.Full)
	    DumpRawGranEvent(CurrentProc, p,
			     GR_FETCH, CurrentTSO, node, (StgInt)0, 0);
	  */
	  IF_GRAN_DEBUG(blockOnFetch, 
			BlockedOnFetch[CurrentProc] = CurrentTSO;); /*- rtsTrue; -*/
	}
#endif /* 0 */

      CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mtidytime;
      
      /* Rescheduling (GranSim internal) is necessary */
      NeedToReSchedule = rtsTrue;
      
      return(1); 
    }
  return(0);
}

//@cindex GranSimSpark
void 
GranSimSpark(local,node)
StgInt local;
StgClosure *node;
{
  /* ++SparksAvail;  Nope; do that in add_to_spark_queue */
  if (RtsFlags.GranFlags.GranSimStats.Sparks)
    DumpRawGranEvent(CurrentProc, (PEs)0, SP_SPARK,
		     END_TSO_QUEUE, node, (StgInt)0, spark_queue_len(CurrentProc)-1);

  /* Force the PE to take notice of the spark */
  if(RtsFlags.GranFlags.DoAlwaysCreateThreads) {
    new_event(CurrentProc,CurrentProc,CurrentTime[CurrentProc],
	      FindWork,
	      END_TSO_QUEUE, (StgClosure*)NULL, (rtsSpark*)NULL);
    if (CurrentTime[CurrentProc]<TimeOfNextEvent)
      TimeOfNextEvent = CurrentTime[CurrentProc];
  }

  if(local)
    ++CurrentTSO->gran.localsparks;
  else
    ++CurrentTSO->gran.globalsparks;
}

//@cindex GranSimSparkAt
void 
GranSimSparkAt(spark,where,identifier)
rtsSpark *spark;
StgClosure *where;    /* This should be a node; alternatively could be a GA */
StgInt identifier;
{
  PEs p = where_is(where);
  GranSimSparkAtAbs(spark,p,identifier);
}

//@cindex GranSimSparkAtAbs
void 
GranSimSparkAtAbs(spark,proc,identifier)
rtsSpark *spark;
PEs proc;        
StgInt identifier;
{
  rtsTime exporttime;

  if (spark == (rtsSpark *)NULL) /* Note: Granularity control might have */
    return;                          /* turned a spark into a NULL. */

  /* ++SparksAvail; Nope; do that in add_to_spark_queue */
  if(RtsFlags.GranFlags.GranSimStats.Sparks)
    DumpRawGranEvent(proc,0,SP_SPARKAT,
		     END_TSO_QUEUE, spark->node, (StgInt)0, spark_queue_len(proc));

  if (proc!=CurrentProc) {
    CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mpacktime;
    exporttime = (CurrentTime[proc] > CurrentTime[CurrentProc]? 
                  CurrentTime[proc]: CurrentTime[CurrentProc])
                 + RtsFlags.GranFlags.Costs.latency;
  } else {
    exporttime = CurrentTime[CurrentProc];
  }

  if ( RtsFlags.GranFlags.Light )
    /* Need CurrentTSO in event field to associate costs with creating
       spark even in a GrAnSim Light setup */
    new_event(proc, CurrentProc, exporttime,
	      MoveSpark,
	      CurrentTSO, spark->node, spark);
  else
    new_event(proc, CurrentProc, exporttime,
	      MoveSpark, (StgTSO*)NULL, spark->node, spark);
  /* Bit of a hack to treat placed sparks the same as stolen sparks */
  ++OutstandingFishes[proc];

  /* Force the PE to take notice of the spark (FINDWORK is put after a
     MoveSpark into the sparkq!) */
  if (RtsFlags.GranFlags.DoAlwaysCreateThreads) {
    new_event(CurrentProc,CurrentProc,exporttime+1,
              FindWork,
	      (StgTSO*)NULL, (StgClosure*)NULL, (rtsSpark*)NULL);
  }

  if (exporttime<TimeOfNextEvent)
    TimeOfNextEvent = exporttime;

  if (proc!=CurrentProc) {
    CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mtidytime;
    ++CurrentTSO->gran.globalsparks;
  } else { 
    ++CurrentTSO->gran.localsparks;
  }
}

/* 
   This function handles local and global blocking.  It's called either
   from threaded code (RBH_entry, BH_entry etc) or from blockFetch when
   trying to fetch an BH or RBH 
*/

//@cindex GranSimBlock
void 
GranSimBlock(tso, proc, node)
StgTSO *tso;
PEs proc;
StgClosure *node;
{
  PEs node_proc = where_is(node), 
      tso_proc = where_is((StgClosure *)tso);

  ASSERT(tso_proc==CurrentProc);
  // ASSERT(node_proc==CurrentProc);
  IF_GRAN_DEBUG(bq,
		if (node_proc!=CurrentProc) 
		  belch("## ghuH: TSO %d (%lx) [PE %d] blocks on non-local node %p [PE %d] (no simulation of FETCHMEs)",
		        tso->id, tso, tso_proc, node, node_proc)); 
  ASSERT(tso->link==END_TSO_QUEUE);
  ASSERT(!is_on_queue(tso,proc)); // tso must not be on run queue already!
  //ASSERT(tso==run_queue_hds[proc]);

  IF_DEBUG(gran,
	   belch("GRAN: TSO %d (%p) [PE %d] blocks on closure %p @ %lx",
		 tso->id, tso, proc, node, CurrentTime[proc]));


    /* THIS SHOULD NEVER HAPPEN!
       If tso tries to block on a remote node (i.e. node_proc!=CurrentProc)
       we have missed a GranSimFetch before entering this closure;
       we hack around it for now, faking a FetchNode; 
       because GranSimBlock is entered via a BLACKHOLE(_BQ) closure,
       tso will be blocked on this closure until the FetchReply occurs.

       ngoq Dogh! 

    if (node_proc!=CurrentProc) {
      StgInt ret;
      ret = GranSimFetch(node);
      IF_GRAN_DEBUG(bq,
                    if (ret)
		      belch(".. GranSimBlock: faking a FetchNode of node %p from %d to %d",
			    node, node_proc, CurrentProc););
      return;
    }
    */

  if (RtsFlags.GranFlags.GranSimStats.Full)
    DumpRawGranEvent(proc,node_proc,GR_BLOCK,tso,node,(StgInt)0,0);

  ++(tso->gran.blockcount);
  /* Distinction  between local and global block is made in blockFetch */
  tso->gran.blockedat = CurrentTime[proc];

  CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadqueuetime;
  ActivateNextThread(proc);
  /* tso->link = END_TSO_QUEUE;    not really necessary; only for testing */
}

#endif /* GRAN */

//@node Index,  , Dumping routines, GranSim specific code
//@subsection Index

//@index
//* ActivateNextThread::  @cindex\s-+ActivateNextThread
//* CurrentProc::  @cindex\s-+CurrentProc
//* CurrentTime::  @cindex\s-+CurrentTime
//* GranSimAllocate::  @cindex\s-+GranSimAllocate
//* GranSimBlock::  @cindex\s-+GranSimBlock
//* GranSimExec::  @cindex\s-+GranSimExec
//* GranSimFetch::  @cindex\s-+GranSimFetch
//* GranSimLight_insertThread::  @cindex\s-+GranSimLight_insertThread
//* GranSimSpark::  @cindex\s-+GranSimSpark
//* GranSimSparkAt::  @cindex\s-+GranSimSparkAt
//* GranSimSparkAtAbs::  @cindex\s-+GranSimSparkAtAbs
//* GranSimUnallocate::  @cindex\s-+GranSimUnallocate
//* any_idle::  @cindex\s-+any_idle
//* blockFetch::  @cindex\s-+blockFetch
//* do_the_fetchnode::  @cindex\s-+do_the_fetchnode
//* do_the_fetchreply::  @cindex\s-+do_the_fetchreply
//* do_the_findwork::  @cindex\s-+do_the_findwork
//* do_the_globalblock::  @cindex\s-+do_the_globalblock
//* do_the_movespark::  @cindex\s-+do_the_movespark
//* do_the_movethread::  @cindex\s-+do_the_movethread
//* do_the_startthread::  @cindex\s-+do_the_startthread
//* do_the_unblock::  @cindex\s-+do_the_unblock
//* fetchNode::  @cindex\s-+fetchNode
//* ga_to_proc::  @cindex\s-+ga_to_proc
//* get_next_event::  @cindex\s-+get_next_event
//* get_time_of_next_event::  @cindex\s-+get_time_of_next_event
//* grab_event::  @cindex\s-+grab_event
//* handleFetchRequest::  @cindex\s-+handleFetchRequest
//* handleIdlePEs::  @cindex\s-+handleIdlePEs
//* idlers::  @cindex\s-+idlers
//* insertThread::  @cindex\s-+insertThread
//* insert_event::  @cindex\s-+insert_event
//* is_on_queue::  @cindex\s-+is_on_queue
//* is_unique::  @cindex\s-+is_unique
//* new_event::  @cindex\s-+new_event
//* prepend_event::  @cindex\s-+prepend_event
//* print_event::  @cindex\s-+print_event
//* print_eventq::  @cindex\s-+print_eventq
//* prune_eventq ::  @cindex\s-+prune_eventq 
//* spark queue::  @cindex\s-+spark queue
//* sparkStealTime::  @cindex\s-+sparkStealTime
//* stealSomething::  @cindex\s-+stealSomething
//* stealSpark::  @cindex\s-+stealSpark
//* stealSparkMagic::  @cindex\s-+stealSparkMagic
//* stealThread::  @cindex\s-+stealThread
//* stealThreadMagic::  @cindex\s-+stealThreadMagic
//* thread_queue_len::  @cindex\s-+thread_queue_len
//* traverse_eventq_for_gc::  @cindex\s-+traverse_eventq_for_gc
//* where_is::  @cindex\s-+where_is
//@end index
