/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "SchedAPI.h"
#include "Storage.h"
#include "Threads.h"
#include "RtsFlags.h"
#include "STM.h"
#include "Schedule.h"
#include "Trace.h"
#include "ThreadLabels.h"

/* Next thread ID to allocate.
 * LOCK: sched_mutex
 */
static StgThreadID next_thread_id = 1;

/* The smallest stack size that makes any sense is:
 *    RESERVED_STACK_WORDS    (so we can get back from the stack overflow)
 *  + sizeofW(StgStopFrame)   (the stg_stop_thread_info frame)
 *  + 1                       (the closure to enter)
 *  + 1			      (stg_ap_v_ret)
 *  + 1			      (spare slot req'd by stg_ap_v_ret)
 *
 * A thread with this stack will bomb immediately with a stack
 * overflow, which will increase its stack size.  
 */
#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 3)

/* ---------------------------------------------------------------------------
   Create a new thread.

   The new thread starts with the given stack size.  Before the
   scheduler can run, however, this thread needs to have a closure
   (and possibly some arguments) pushed on its stack.  See
   pushClosure() in Schedule.h.

   createGenThread() and createIOThread() (in SchedAPI.h) are
   convenient packaged versions of this function.

   currently pri (priority) is only used in a GRAN setup -- HWL
   ------------------------------------------------------------------------ */
#if defined(GRAN)
/*   currently pri (priority) is only used in a GRAN setup -- HWL */
StgTSO *
createThread(nat size, StgInt pri)
#else
StgTSO *
createThread(Capability *cap, nat size)
#endif
{
    StgTSO *tso;
    nat stack_size;

    /* sched_mutex is *not* required */

    /* First check whether we should create a thread at all */
#if defined(PARALLEL_HASKELL)
    /* check that no more than RtsFlags.ParFlags.maxThreads threads are created */
    if (advisory_thread_count >= RtsFlags.ParFlags.maxThreads) {
	threadsIgnored++;
	debugBelch("{createThread}Daq ghuH: refusing to create another thread; no more than %d threads allowed (currently %d)\n",
		   RtsFlags.ParFlags.maxThreads, advisory_thread_count);
	return END_TSO_QUEUE;
    }
    threadsCreated++;
#endif

#if defined(GRAN)
    ASSERT(!RtsFlags.GranFlags.Light || CurrentProc==0);
#endif

    // ToDo: check whether size = stack_size - TSO_STRUCT_SIZEW

    /* catch ridiculously small stack sizes */
    if (size < MIN_STACK_WORDS + TSO_STRUCT_SIZEW) {
	size = MIN_STACK_WORDS + TSO_STRUCT_SIZEW;
    }

    stack_size = size - TSO_STRUCT_SIZEW;
    
    tso = (StgTSO *)allocateLocal(cap, size);
    TICK_ALLOC_TSO(stack_size, 0);

    SET_HDR(tso, &stg_TSO_info, CCS_SYSTEM);
#if defined(GRAN)
    SET_GRAN_HDR(tso, ThisPE);
#endif

    // Always start with the compiled code evaluator
    tso->what_next = ThreadRunGHC;

    tso->why_blocked  = NotBlocked;
    tso->blocked_exceptions = END_TSO_QUEUE;
    tso->flags = TSO_DIRTY;
    
    tso->saved_errno = 0;
    tso->bound = NULL;
    tso->cap = cap;
    
    tso->stack_size     = stack_size;
    tso->max_stack_size = round_to_mblocks(RtsFlags.GcFlags.maxStkSize) 
	                  - TSO_STRUCT_SIZEW;
    tso->sp             = (P_)&(tso->stack) + stack_size;

    tso->trec = NO_TREC;
    
#ifdef PROFILING
    tso->prof.CCCS = CCS_MAIN;
#endif
    
  /* put a stop frame on the stack */
    tso->sp -= sizeofW(StgStopFrame);
    SET_HDR((StgClosure*)tso->sp,(StgInfoTable *)&stg_stop_thread_info,CCS_SYSTEM);
    tso->link = END_TSO_QUEUE;
    
  // ToDo: check this
#if defined(GRAN)
    /* uses more flexible routine in GranSim */
    insertThread(tso, CurrentProc);
#else
    /* In a non-GranSim setup the pushing of a TSO onto the runq is separated
     * from its creation
     */
#endif
    
#if defined(GRAN) 
    if (RtsFlags.GranFlags.GranSimStats.Full) 
	DumpGranEvent(GR_START,tso);
#elif defined(PARALLEL_HASKELL)
    if (RtsFlags.ParFlags.ParStats.Full) 
	DumpGranEvent(GR_STARTQ,tso);
    /* HACk to avoid SCHEDULE 
       LastTSO = tso; */
#endif
    
    /* Link the new thread on the global thread list.
     */
    ACQUIRE_LOCK(&sched_mutex);
    tso->id = next_thread_id++;  // while we have the mutex
    tso->global_link = all_threads;
    all_threads = tso;
    RELEASE_LOCK(&sched_mutex);
    
#if defined(DIST)
    tso->dist.priority = MandatoryPriority; //by default that is...
#endif
    
#if defined(GRAN)
    tso->gran.pri = pri;
# if defined(DEBUG)
    tso->gran.magic = TSO_MAGIC; // debugging only
# endif
    tso->gran.sparkname   = 0;
    tso->gran.startedat   = CURRENT_TIME; 
    tso->gran.exported    = 0;
    tso->gran.basicblocks = 0;
    tso->gran.allocs      = 0;
    tso->gran.exectime    = 0;
    tso->gran.fetchtime   = 0;
    tso->gran.fetchcount  = 0;
    tso->gran.blocktime   = 0;
    tso->gran.blockcount  = 0;
    tso->gran.blockedat   = 0;
    tso->gran.globalsparks = 0;
    tso->gran.localsparks  = 0;
    if (RtsFlags.GranFlags.Light)
	tso->gran.clock  = Now; /* local clock */
    else
	tso->gran.clock  = 0;
    
    IF_DEBUG(gran,printTSO(tso));
#elif defined(PARALLEL_HASKELL)
# if defined(DEBUG)
    tso->par.magic = TSO_MAGIC; // debugging only
# endif
    tso->par.sparkname   = 0;
    tso->par.startedat   = CURRENT_TIME; 
    tso->par.exported    = 0;
    tso->par.basicblocks = 0;
    tso->par.allocs      = 0;
    tso->par.exectime    = 0;
    tso->par.fetchtime   = 0;
    tso->par.fetchcount  = 0;
    tso->par.blocktime   = 0;
    tso->par.blockcount  = 0;
    tso->par.blockedat   = 0;
    tso->par.globalsparks = 0;
    tso->par.localsparks  = 0;
#endif
    
#if defined(GRAN)
    globalGranStats.tot_threads_created++;
    globalGranStats.threads_created_on_PE[CurrentProc]++;
    globalGranStats.tot_sq_len += spark_queue_len(CurrentProc);
    globalGranStats.tot_sq_probes++;
#elif defined(PARALLEL_HASKELL)
    // collect parallel global statistics (currently done together with GC stats)
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	//debugBelch("Creating thread %d @ %11.2f\n", tso->id, usertime()); 
	globalParStats.tot_threads_created++;
    }
#endif 
    
#if defined(GRAN)
    debugTrace(GRAN_DEBUG_pri,
	       "==__ schedule: Created TSO %d (%p);",
	       CurrentProc, tso, tso->id);
#elif defined(PARALLEL_HASKELL)
    debugTrace(PAR_DEBUG_verbose,
	       "==__ schedule: Created TSO %d (%p); %d threads active",
	       (long)tso->id, tso, advisory_thread_count);
#else
    debugTrace(DEBUG_sched,
	       "created thread %ld, stack size = %lx words", 
	       (long)tso->id, (long)tso->stack_size);
#endif    
    return tso;
}

#if defined(PAR)
/* RFP:
   all parallel thread creation calls should fall through the following routine.
*/
StgTSO *
createThreadFromSpark(rtsSpark spark) 
{ StgTSO *tso;
  ASSERT(spark != (rtsSpark)NULL);
// JB: TAKE CARE OF THIS COUNTER! BUGGY
  if (advisory_thread_count >= RtsFlags.ParFlags.maxThreads) 
  { threadsIgnored++;
    barf("{createSparkThread}Daq ghuH: refusing to create another thread; no more than %d threads allowed (currently %d)",
	  RtsFlags.ParFlags.maxThreads, advisory_thread_count);    
    return END_TSO_QUEUE;
  }
  else
  { threadsCreated++;
    tso = createThread(RtsFlags.GcFlags.initialStkSize);
    if (tso==END_TSO_QUEUE)	
      barf("createSparkThread: Cannot create TSO");
#if defined(DIST)
    tso->priority = AdvisoryPriority;
#endif
    pushClosure(tso,spark);
    addToRunQueue(tso);
    advisory_thread_count++;  // JB: TAKE CARE OF THIS COUNTER! BUGGY
  }
  return tso;
}
#endif

/* ---------------------------------------------------------------------------
 * Comparing Thread ids.
 *
 * This is used from STG land in the implementation of the
 * instances of Eq/Ord for ThreadIds.
 * ------------------------------------------------------------------------ */

int
cmp_thread(StgPtr tso1, StgPtr tso2) 
{ 
  StgThreadID id1 = ((StgTSO *)tso1)->id; 
  StgThreadID id2 = ((StgTSO *)tso2)->id;
 
  if (id1 < id2) return (-1);
  if (id1 > id2) return 1;
  return 0;
}

/* ---------------------------------------------------------------------------
 * Fetching the ThreadID from an StgTSO.
 *
 * This is used in the implementation of Show for ThreadIds.
 * ------------------------------------------------------------------------ */
int
rts_getThreadId(StgPtr tso) 
{
  return ((StgTSO *)tso)->id;
}

/* -----------------------------------------------------------------------------
   Remove a thread from a queue.
   Fails fatally if the TSO is not on the queue.
   -------------------------------------------------------------------------- */

void
removeThreadFromQueue (StgTSO **queue, StgTSO *tso)
{
    StgTSO *t, *prev;

    prev = NULL;
    for (t = *queue; t != END_TSO_QUEUE; prev = t, t = t->link) {
	if (t == tso) {
	    if (prev) {
		prev->link = t->link;
	    } else {
		*queue = t->link;
	    }
	    return;
	}
    }
    barf("removeThreadFromQueue: not found");
}

void
removeThreadFromDeQueue (StgTSO **head, StgTSO **tail, StgTSO *tso)
{
    StgTSO *t, *prev;

    prev = NULL;
    for (t = *head; t != END_TSO_QUEUE; prev = t, t = t->link) {
	if (t == tso) {
	    if (prev) {
		prev->link = t->link;
	    } else {
		*head = t->link;
	    }
	    if (*tail == tso) {
		if (prev) {
		    *tail = prev;
		} else {
		    *tail = END_TSO_QUEUE;
		}
	    }
	    return;
	}
    }
    barf("removeThreadFromMVarQueue: not found");
}

void
removeThreadFromMVarQueue (StgMVar *mvar, StgTSO *tso)
{
    removeThreadFromDeQueue (&mvar->head, &mvar->tail, tso);
}

/* ----------------------------------------------------------------------------
   unblockOne()

   unblock a single thread.
   ------------------------------------------------------------------------- */

#if defined(GRAN)
STATIC_INLINE void
unblockCount ( StgBlockingQueueElement *bqe, StgClosure *node )
{
}
#elif defined(PARALLEL_HASKELL)
STATIC_INLINE void
unblockCount ( StgBlockingQueueElement *bqe, StgClosure *node )
{
  /* write RESUME events to log file and
     update blocked and fetch time (depending on type of the orig closure) */
  if (RtsFlags.ParFlags.ParStats.Full) {
    DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC, 
		     GR_RESUMEQ, ((StgTSO *)bqe), ((StgTSO *)bqe)->block_info.closure,
		     0, 0 /* spark_queue_len(ADVISORY_POOL) */);
    if (emptyRunQueue())
      emitSchedule = rtsTrue;

    switch (get_itbl(node)->type) {
	case FETCH_ME_BQ:
	  ((StgTSO *)bqe)->par.fetchtime += CURRENT_TIME-((StgTSO *)bqe)->par.blockedat;
	  break;
	case RBH:
	case FETCH_ME:
	case BLACKHOLE_BQ:
	  ((StgTSO *)bqe)->par.blocktime += CURRENT_TIME-((StgTSO *)bqe)->par.blockedat;
	  break;
#ifdef DIST
        case MVAR:
          break;
#endif	  
	default:
	  barf("{unblockOne}Daq Qagh: unexpected closure in blocking queue");
	}
      }
}
#endif

#if defined(GRAN)
StgBlockingQueueElement *
unblockOne(StgBlockingQueueElement *bqe, StgClosure *node)
{
    StgTSO *tso;
    PEs node_loc, tso_loc;

    node_loc = where_is(node); // should be lifted out of loop
    tso = (StgTSO *)bqe;  // wastes an assignment to get the type right
    tso_loc = where_is((StgClosure *)tso);
    if (IS_LOCAL_TO(PROCS(node),tso_loc)) { // TSO is local
      /* !fake_fetch => TSO is on CurrentProc is same as IS_LOCAL_TO */
      ASSERT(CurrentProc!=node_loc || tso_loc==CurrentProc);
      CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.lunblocktime;
      // insertThread(tso, node_loc);
      new_event(tso_loc, tso_loc, CurrentTime[CurrentProc],
		ResumeThread,
		tso, node, (rtsSpark*)NULL);
      tso->link = END_TSO_QUEUE; // overwrite link just to be sure 
      // len_local++;
      // len++;
    } else { // TSO is remote (actually should be FMBQ)
      CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mpacktime +
                                  RtsFlags.GranFlags.Costs.gunblocktime +
	                          RtsFlags.GranFlags.Costs.latency;
      new_event(tso_loc, CurrentProc, CurrentTime[CurrentProc],
		UnblockThread,
		tso, node, (rtsSpark*)NULL);
      tso->link = END_TSO_QUEUE; // overwrite link just to be sure 
      // len++;
    }
    /* the thread-queue-overhead is accounted for in either Resume or UnblockThread */
    IF_GRAN_DEBUG(bq,
		  debugBelch(" %s TSO %d (%p) [PE %d] (block_info.closure=%p) (next=%p) ,",
			  (node_loc==tso_loc ? "Local" : "Global"), 
			  tso->id, tso, CurrentProc, tso->block_info.closure, tso->link));
    tso->block_info.closure = NULL;
    debugTrace(DEBUG_sched, "-- waking up thread %ld (%p)", 
	       tso->id, tso));
}
#elif defined(PARALLEL_HASKELL)
StgBlockingQueueElement *
unblockOne(StgBlockingQueueElement *bqe, StgClosure *node)
{
    StgBlockingQueueElement *next;

    switch (get_itbl(bqe)->type) {
    case TSO:
      ASSERT(((StgTSO *)bqe)->why_blocked != NotBlocked);
      /* if it's a TSO just push it onto the run_queue */
      next = bqe->link;
      ((StgTSO *)bqe)->link = END_TSO_QUEUE; // debugging?
      APPEND_TO_RUN_QUEUE((StgTSO *)bqe); 
      threadRunnable();
      unblockCount(bqe, node);
      /* reset blocking status after dumping event */
      ((StgTSO *)bqe)->why_blocked = NotBlocked;
      break;

    case BLOCKED_FETCH:
      /* if it's a BLOCKED_FETCH put it on the PendingFetches list */
      next = bqe->link;
      bqe->link = (StgBlockingQueueElement *)PendingFetches;
      PendingFetches = (StgBlockedFetch *)bqe;
      break;

# if defined(DEBUG)
      /* can ignore this case in a non-debugging setup; 
	 see comments on RBHSave closures above */
    case CONSTR:
      /* check that the closure is an RBHSave closure */
      ASSERT(get_itbl((StgClosure *)bqe) == &stg_RBH_Save_0_info ||
	     get_itbl((StgClosure *)bqe) == &stg_RBH_Save_1_info ||
	     get_itbl((StgClosure *)bqe) == &stg_RBH_Save_2_info);
      break;

    default:
      barf("{unblockOne}Daq Qagh: Unexpected IP (%#lx; %s) in blocking queue at %#lx\n",
	   get_itbl((StgClosure *)bqe), info_type((StgClosure *)bqe), 
	   (StgClosure *)bqe);
# endif
    }
  IF_PAR_DEBUG(bq, debugBelch(", %p (%s)\n", bqe, info_type((StgClosure*)bqe)));
  return next;
}
#endif

StgTSO *
unblockOne (Capability *cap, StgTSO *tso)
{
    return unblockOne_(cap,tso,rtsTrue); // allow migration
}

StgTSO *
unblockOne_ (Capability *cap, StgTSO *tso, 
	     rtsBool allow_migrate USED_IF_THREADS)
{
  StgTSO *next;

  // NO, might be a WHITEHOLE: ASSERT(get_itbl(tso)->type == TSO);
  ASSERT(tso->why_blocked != NotBlocked);

  tso->why_blocked = NotBlocked;
  next = tso->link;
  tso->link = END_TSO_QUEUE;

#if defined(THREADED_RTS)
  if (tso->cap == cap || (!tsoLocked(tso) && 
			  allow_migrate && 
			  RtsFlags.ParFlags.wakeupMigrate)) {
      // We are waking up this thread on the current Capability, which
      // might involve migrating it from the Capability it was last on.
      if (tso->bound) {
	  ASSERT(tso->bound->cap == tso->cap);
	  tso->bound->cap = cap;
      }
      tso->cap = cap;
      appendToRunQueue(cap,tso);
      // we're holding a newly woken thread, make sure we context switch
      // quickly so we can migrate it if necessary.
      context_switch = 1;
  } else {
      // we'll try to wake it up on the Capability it was last on.
      wakeupThreadOnCapability_lock(tso->cap, tso);
  }
#else
  appendToRunQueue(cap,tso);
  context_switch = 1;
#endif

  debugTrace(DEBUG_sched,
	     "waking up thread %ld on cap %d",
	     (long)tso->id, tso->cap->no);

  return next;
}

/* ----------------------------------------------------------------------------
   awakenBlockedQueue

   wakes up all the threads on the specified queue.
   ------------------------------------------------------------------------- */

#if defined(GRAN)
void
awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  PEs node_loc;
  nat len = 0; 

  IF_GRAN_DEBUG(bq, 
		debugBelch("##-_ AwBQ for node %p on PE %d @ %ld by TSO %d (%p): \n", \
		      node, CurrentProc, CurrentTime[CurrentProc], 
		      CurrentTSO->id, CurrentTSO));

  node_loc = where_is(node);

  ASSERT(q == END_BQ_QUEUE ||
	 get_itbl(q)->type == TSO ||   // q is either a TSO or an RBHSave
	 get_itbl(q)->type == CONSTR); // closure (type constructor)
  ASSERT(is_unique(node));

  /* FAKE FETCH: magically copy the node to the tso's proc;
     no Fetch necessary because in reality the node should not have been 
     moved to the other PE in the first place
  */
  if (CurrentProc!=node_loc) {
    IF_GRAN_DEBUG(bq, 
		  debugBelch("## node %p is on PE %d but CurrentProc is %d (TSO %d); assuming fake fetch and adjusting bitmask (old: %#x)\n",
			node, node_loc, CurrentProc, CurrentTSO->id, 
			// CurrentTSO, where_is(CurrentTSO),
			node->header.gran.procs));
    node->header.gran.procs = (node->header.gran.procs) | PE_NUMBER(CurrentProc);
    IF_GRAN_DEBUG(bq, 
		  debugBelch("## new bitmask of node %p is %#x\n",
			node, node->header.gran.procs));
    if (RtsFlags.GranFlags.GranSimStats.Global) {
      globalGranStats.tot_fake_fetches++;
    }
  }

  bqe = q;
  // ToDo: check: ASSERT(CurrentProc==node_loc);
  while (get_itbl(bqe)->type==TSO) { // q != END_TSO_QUEUE) {
    //next = bqe->link;
    /* 
       bqe points to the current element in the queue
       next points to the next element in the queue
    */
    //tso = (StgTSO *)bqe;  // wastes an assignment to get the type right
    //tso_loc = where_is(tso);
    len++;
    bqe = unblockOne(bqe, node);
  }

  /* if this is the BQ of an RBH, we have to put back the info ripped out of
     the closure to make room for the anchor of the BQ */
  if (bqe!=END_BQ_QUEUE) {
    ASSERT(get_itbl(node)->type == RBH && get_itbl(bqe)->type == CONSTR);
    /*
    ASSERT((info_ptr==&RBH_Save_0_info) ||
	   (info_ptr==&RBH_Save_1_info) ||
	   (info_ptr==&RBH_Save_2_info));
    */
    /* cf. convertToRBH in RBH.c for writing the RBHSave closure */
    ((StgRBH *)node)->blocking_queue = (StgBlockingQueueElement *)((StgRBHSave *)bqe)->payload[0];
    ((StgRBH *)node)->mut_link       = (StgMutClosure *)((StgRBHSave *)bqe)->payload[1];

    IF_GRAN_DEBUG(bq,
		  debugBelch("## Filled in RBH_Save for %p (%s) at end of AwBQ\n",
			node, info_type(node)));
  }

  /* statistics gathering */
  if (RtsFlags.GranFlags.GranSimStats.Global) {
    // globalGranStats.tot_bq_processing_time += bq_processing_time;
    globalGranStats.tot_bq_len += len;      // total length of all bqs awakened
    // globalGranStats.tot_bq_len_local += len_local;  // same for local TSOs only
    globalGranStats.tot_awbq++;             // total no. of bqs awakened
  }
  IF_GRAN_DEBUG(bq,
		debugBelch("## BQ Stats of %p: [%d entries] %s\n",
			node, len, (bqe!=END_BQ_QUEUE) ? "RBH" : ""));
}
#elif defined(PARALLEL_HASKELL)
void 
awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node)
{
  StgBlockingQueueElement *bqe;

  IF_PAR_DEBUG(verbose, 
	       debugBelch("##-_ AwBQ for node %p on [%x]: \n",
		     node, mytid));
#ifdef DIST  
  //RFP
  if(get_itbl(q)->type == CONSTR || q==END_BQ_QUEUE) {
    IF_PAR_DEBUG(verbose, debugBelch("## ... nothing to unblock so lets just return. RFP (BUG?)\n"));
    return;
  }
#endif
  
  ASSERT(q == END_BQ_QUEUE ||
	 get_itbl(q)->type == TSO ||           
  	 get_itbl(q)->type == BLOCKED_FETCH || 
  	 get_itbl(q)->type == CONSTR); 

  bqe = q;
  while (get_itbl(bqe)->type==TSO || 
	 get_itbl(bqe)->type==BLOCKED_FETCH) {
    bqe = unblockOne(bqe, node);
  }
}

#else   /* !GRAN && !PARALLEL_HASKELL */

void
awakenBlockedQueue(Capability *cap, StgTSO *tso)
{
    while (tso != END_TSO_QUEUE) {
	tso = unblockOne(cap,tso);
    }
}
#endif


/* ---------------------------------------------------------------------------
 * rtsSupportsBoundThreads(): is the RTS built to support bound threads?
 * used by Control.Concurrent for error checking.
 * ------------------------------------------------------------------------- */
 
HsBool
rtsSupportsBoundThreads(void)
{
#if defined(THREADED_RTS)
  return HS_BOOL_TRUE;
#else
  return HS_BOOL_FALSE;
#endif
}

/* ---------------------------------------------------------------------------
 * isThreadBound(tso): check whether tso is bound to an OS thread.
 * ------------------------------------------------------------------------- */
 
StgBool
isThreadBound(StgTSO* tso USED_IF_THREADS)
{
#if defined(THREADED_RTS)
  return (tso->bound != NULL);
#endif
  return rtsFalse;
}

/* ----------------------------------------------------------------------------
 * Debugging: why is a thread blocked
 * ------------------------------------------------------------------------- */

#if DEBUG
void
printThreadBlockage(StgTSO *tso)
{
  switch (tso->why_blocked) {
  case BlockedOnRead:
    debugBelch("is blocked on read from fd %d", (int)(tso->block_info.fd));
    break;
  case BlockedOnWrite:
    debugBelch("is blocked on write to fd %d", (int)(tso->block_info.fd));
    break;
#if defined(mingw32_HOST_OS)
    case BlockedOnDoProc:
    debugBelch("is blocked on proc (request: %ld)", tso->block_info.async_result->reqID);
    break;
#endif
  case BlockedOnDelay:
    debugBelch("is blocked until %ld", (long)(tso->block_info.target));
    break;
  case BlockedOnMVar:
    debugBelch("is blocked on an MVar @ %p", tso->block_info.closure);
    break;
  case BlockedOnException:
    debugBelch("is blocked on delivering an exception to thread %lu",
	       (unsigned long)tso->block_info.tso->id);
    break;
  case BlockedOnBlackHole:
    debugBelch("is blocked on a black hole");
    break;
  case NotBlocked:
    debugBelch("is not blocked");
    break;
#if defined(PARALLEL_HASKELL)
  case BlockedOnGA:
    debugBelch("is blocked on global address; local FM_BQ is %p (%s)",
	    tso->block_info.closure, info_type(tso->block_info.closure));
    break;
  case BlockedOnGA_NoSend:
    debugBelch("is blocked on global address (no send); local FM_BQ is %p (%s)",
	    tso->block_info.closure, info_type(tso->block_info.closure));
    break;
#endif
  case BlockedOnCCall:
    debugBelch("is blocked on an external call");
    break;
  case BlockedOnCCall_NoUnblockExc:
    debugBelch("is blocked on an external call (exceptions were already blocked)");
    break;
  case BlockedOnSTM:
    debugBelch("is blocked on an STM operation");
    break;
  default:
    barf("printThreadBlockage: strange tso->why_blocked: %d for TSO %d (%d)",
	 tso->why_blocked, tso->id, tso);
  }
}

void
printThreadStatus(StgTSO *t)
{
  debugBelch("\tthread %4lu @ %p ", (unsigned long)t->id, (void *)t);
    {
      void *label = lookupThreadLabel(t->id);
      if (label) debugBelch("[\"%s\"] ",(char *)label);
    }
    if (t->what_next == ThreadRelocated) {
	debugBelch("has been relocated...\n");
    } else {
	switch (t->what_next) {
	case ThreadKilled:
	    debugBelch("has been killed");
	    break;
	case ThreadComplete:
	    debugBelch("has completed");
	    break;
	default:
	    printThreadBlockage(t);
	}
	debugBelch("\n");
    }
}

void
printAllThreads(void)
{
  StgTSO *t, *next;
  nat i;
  Capability *cap;

# if defined(GRAN)
  char time_string[TIME_STR_LEN], node_str[NODE_STR_LEN];
  ullong_format_string(TIME_ON_PROC(CurrentProc), 
		       time_string, rtsFalse/*no commas!*/);

  debugBelch("all threads at [%s]:\n", time_string);
# elif defined(PARALLEL_HASKELL)
  char time_string[TIME_STR_LEN], node_str[NODE_STR_LEN];
  ullong_format_string(CURRENT_TIME,
		       time_string, rtsFalse/*no commas!*/);

  debugBelch("all threads at [%s]:\n", time_string);
# else
  debugBelch("all threads:\n");
# endif

  for (i = 0; i < n_capabilities; i++) {
      cap = &capabilities[i];
      debugBelch("threads on capability %d:\n", cap->no);
      for (t = cap->run_queue_hd; t != END_TSO_QUEUE; t = t->link) {
	  printThreadStatus(t);
      }
  }

  debugBelch("other threads:\n");
  for (t = all_threads; t != END_TSO_QUEUE; t = next) {
      if (t->why_blocked != NotBlocked) {
	  printThreadStatus(t);
      }
      if (t->what_next == ThreadRelocated) {
	  next = t->link;
      } else {
	  next = t->global_link;
      }
  }
}

// useful from gdb
void 
printThreadQueue(StgTSO *t)
{
    nat i = 0;
    for (; t != END_TSO_QUEUE; t = t->link) {
	printThreadStatus(t);
	i++;
    }
    debugBelch("%d threads on queue\n", i);
}

/* 
   Print a whole blocking queue attached to node (debugging only).
*/
# if defined(PARALLEL_HASKELL)
void 
print_bq (StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  StgTSO *tso;
  rtsBool end;

  debugBelch("## BQ of closure %p (%s): ",
	  node, info_type(node));

  /* should cover all closures that may have a blocking queue */
  ASSERT(get_itbl(node)->type == BLACKHOLE_BQ ||
	 get_itbl(node)->type == FETCH_ME_BQ ||
	 get_itbl(node)->type == RBH ||
	 get_itbl(node)->type == MVAR);
    
  ASSERT(node!=(StgClosure*)NULL);         // sanity check

  print_bqe(((StgBlockingQueue*)node)->blocking_queue);
}

/* 
   Print a whole blocking queue starting with the element bqe.
*/
void 
print_bqe (StgBlockingQueueElement *bqe)
{
  rtsBool end;

  /* 
     NB: In a parallel setup a BQ of an RBH must end with an RBH_Save closure;
  */
  for (end = (bqe==END_BQ_QUEUE);
       !end; // iterate until bqe points to a CONSTR
       end = (get_itbl(bqe)->type == CONSTR) || (bqe->link==END_BQ_QUEUE), 
       bqe = end ? END_BQ_QUEUE : bqe->link) {
    ASSERT(bqe != END_BQ_QUEUE);                               // sanity check
    ASSERT(bqe != (StgBlockingQueueElement *)NULL);            // sanity check
    /* types of closures that may appear in a blocking queue */
    ASSERT(get_itbl(bqe)->type == TSO ||           
	   get_itbl(bqe)->type == BLOCKED_FETCH || 
	   get_itbl(bqe)->type == CONSTR); 
    /* only BQs of an RBH end with an RBH_Save closure */
    //ASSERT(get_itbl(bqe)->type != CONSTR || get_itbl(node)->type == RBH);

    switch (get_itbl(bqe)->type) {
    case TSO:
      debugBelch(" TSO %u (%x),",
	      ((StgTSO *)bqe)->id, ((StgTSO *)bqe));
      break;
    case BLOCKED_FETCH:
      debugBelch(" BF (node=%p, ga=((%x, %d, %x)),",
	      ((StgBlockedFetch *)bqe)->node, 
	      ((StgBlockedFetch *)bqe)->ga.payload.gc.gtid,
	      ((StgBlockedFetch *)bqe)->ga.payload.gc.slot,
	      ((StgBlockedFetch *)bqe)->ga.weight);
      break;
    case CONSTR:
      debugBelch(" %s (IP %p),",
	      (get_itbl(bqe) == &stg_RBH_Save_0_info ? "RBH_Save_0" :
	       get_itbl(bqe) == &stg_RBH_Save_1_info ? "RBH_Save_1" :
	       get_itbl(bqe) == &stg_RBH_Save_2_info ? "RBH_Save_2" :
	       "RBH_Save_?"), get_itbl(bqe));
      break;
    default:
      barf("Unexpected closure type %s in blocking queue", // of %p (%s)",
	   info_type((StgClosure *)bqe)); // , node, info_type(node));
      break;
    }
  } /* for */
  debugBelch("\n");
}
# elif defined(GRAN)
void 
print_bq (StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  PEs node_loc, tso_loc;
  rtsBool end;

  /* should cover all closures that may have a blocking queue */
  ASSERT(get_itbl(node)->type == BLACKHOLE_BQ ||
	 get_itbl(node)->type == FETCH_ME_BQ ||
	 get_itbl(node)->type == RBH);
    
  ASSERT(node!=(StgClosure*)NULL);         // sanity check
  node_loc = where_is(node);

  debugBelch("## BQ of closure %p (%s) on [PE %d]: ",
	  node, info_type(node), node_loc);

  /* 
     NB: In a parallel setup a BQ of an RBH must end with an RBH_Save closure;
  */
  for (bqe = ((StgBlockingQueue*)node)->blocking_queue, end = (bqe==END_BQ_QUEUE);
       !end; // iterate until bqe points to a CONSTR
       end = (get_itbl(bqe)->type == CONSTR) || (bqe->link==END_BQ_QUEUE), bqe = end ? END_BQ_QUEUE : bqe->link) {
    ASSERT(bqe != END_BQ_QUEUE);             // sanity check
    ASSERT(bqe != (StgBlockingQueueElement *)NULL);  // sanity check
    /* types of closures that may appear in a blocking queue */
    ASSERT(get_itbl(bqe)->type == TSO ||           
	   get_itbl(bqe)->type == CONSTR); 
    /* only BQs of an RBH end with an RBH_Save closure */
    ASSERT(get_itbl(bqe)->type != CONSTR || get_itbl(node)->type == RBH);

    tso_loc = where_is((StgClosure *)bqe);
    switch (get_itbl(bqe)->type) {
    case TSO:
      debugBelch(" TSO %d (%p) on [PE %d],",
	      ((StgTSO *)bqe)->id, (StgTSO *)bqe, tso_loc);
      break;
    case CONSTR:
      debugBelch(" %s (IP %p),",
	      (get_itbl(bqe) == &stg_RBH_Save_0_info ? "RBH_Save_0" :
	       get_itbl(bqe) == &stg_RBH_Save_1_info ? "RBH_Save_1" :
	       get_itbl(bqe) == &stg_RBH_Save_2_info ? "RBH_Save_2" :
	       "RBH_Save_?"), get_itbl(bqe));
      break;
    default:
      barf("Unexpected closure type %s in blocking queue of %p (%s)",
	   info_type((StgClosure *)bqe), node, info_type(node));
      break;
    }
  } /* for */
  debugBelch("\n");
}
# endif

#if defined(PARALLEL_HASKELL)
nat
run_queue_len(void)
{
    nat i;
    StgTSO *tso;
    
    for (i=0, tso=run_queue_hd; 
	 tso != END_TSO_QUEUE;
	 i++, tso=tso->link) {
	/* nothing */
    }
	
    return i;
}
#endif

#endif /* DEBUG */
