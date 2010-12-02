/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Updates.h"
#include "Threads.h"
#include "STM.h"
#include "Schedule.h"
#include "Trace.h"
#include "ThreadLabels.h"
#include "Updates.h"
#include "Messages.h"
#include "sm/Storage.h"

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
StgTSO *
createThread(Capability *cap, nat size)
{
    StgTSO *tso;
    nat stack_size;

    /* sched_mutex is *not* required */

    /* First check whether we should create a thread at all */

    // ToDo: check whether size = stack_size - TSO_STRUCT_SIZEW

    /* catch ridiculously small stack sizes */
    if (size < MIN_STACK_WORDS + TSO_STRUCT_SIZEW) {
	size = MIN_STACK_WORDS + TSO_STRUCT_SIZEW;
    }

    size = round_to_mblocks(size);
    tso = (StgTSO *)allocate(cap, size);

    stack_size = size - TSO_STRUCT_SIZEW;
    TICK_ALLOC_TSO(stack_size, 0);

    SET_HDR(tso, &stg_TSO_info, CCS_SYSTEM);

    // Always start with the compiled code evaluator
    tso->what_next = ThreadRunGHC;

    tso->why_blocked  = NotBlocked;
    tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
    tso->blocked_exceptions = END_BLOCKED_EXCEPTIONS_QUEUE;
    tso->bq = (StgBlockingQueue *)END_TSO_QUEUE;
    tso->flags = 0;
    tso->dirty = 1;
    
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
    tso->_link = END_TSO_QUEUE;
    
    /* Link the new thread on the global thread list.
     */
    ACQUIRE_LOCK(&sched_mutex);
    tso->id = next_thread_id++;  // while we have the mutex
    tso->global_link = g0->threads;
    g0->threads = tso;
    RELEASE_LOCK(&sched_mutex);
    
    // ToDo: report the stack size in the event?
    traceEventCreateThread(cap, tso);

    return tso;
}

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

rtsBool // returns True if we modified queue
removeThreadFromQueue (Capability *cap, StgTSO **queue, StgTSO *tso)
{
    StgTSO *t, *prev;

    prev = NULL;
    for (t = *queue; t != END_TSO_QUEUE; prev = t, t = t->_link) {
	if (t == tso) {
	    if (prev) {
		setTSOLink(cap,prev,t->_link);
                t->_link = END_TSO_QUEUE;
                return rtsFalse;
	    } else {
		*queue = t->_link;
                t->_link = END_TSO_QUEUE;
                return rtsTrue;
	    }
	}
    }
    barf("removeThreadFromQueue: not found");
}

rtsBool // returns True if we modified head or tail
removeThreadFromDeQueue (Capability *cap, 
                         StgTSO **head, StgTSO **tail, StgTSO *tso)
{
    StgTSO *t, *prev;
    rtsBool flag = rtsFalse;

    prev = NULL;
    for (t = *head; t != END_TSO_QUEUE; prev = t, t = t->_link) {
	if (t == tso) {
	    if (prev) {
		setTSOLink(cap,prev,t->_link);
                flag = rtsFalse;
	    } else {
		*head = t->_link;
                flag = rtsTrue;
	    }
            t->_link = END_TSO_QUEUE;
            if (*tail == tso) {
		if (prev) {
		    *tail = prev;
		} else {
		    *tail = END_TSO_QUEUE;
		}
                return rtsTrue;
	    } else {
                return flag;
            }
	}
    }
    barf("removeThreadFromMVarQueue: not found");
}

/* ----------------------------------------------------------------------------
   tryWakeupThread()

   Attempt to wake up a thread.  tryWakeupThread is idempotent: it is
   always safe to call it too many times, but it is not safe in
   general to omit a call.

   ------------------------------------------------------------------------- */

void
tryWakeupThread (Capability *cap, StgTSO *tso)
{
    tryWakeupThread_(cap, deRefTSO(tso));
}

void
tryWakeupThread_ (Capability *cap, StgTSO *tso)
{
    traceEventThreadWakeup (cap, tso, tso->cap->no);

#ifdef THREADED_RTS
    if (tso->cap != cap)
    {
        MessageWakeup *msg;
        msg = (MessageWakeup *)allocate(cap,sizeofW(MessageWakeup));
        SET_HDR(msg, &stg_MSG_TRY_WAKEUP_info, CCS_SYSTEM);
        msg->tso = tso;
        sendMessage(cap, tso->cap, (Message*)msg);
        debugTraceCap(DEBUG_sched, cap, "message: try wakeup thread %ld on cap %d",
                      (lnat)tso->id, tso->cap->no);
        return;
    }
#endif

    switch (tso->why_blocked)
    {
    case BlockedOnMVar:
    {
        if (tso->_link == END_TSO_QUEUE) {
            tso->block_info.closure = (StgClosure*)END_TSO_QUEUE;
            goto unblock;
        } else {
            return;
        }
    }

    case BlockedOnMsgThrowTo:
    {
        const StgInfoTable *i;
        
        i = lockClosure(tso->block_info.closure);
        unlockClosure(tso->block_info.closure, i);
        if (i != &stg_MSG_NULL_info) {
            debugTraceCap(DEBUG_sched, cap, "thread %ld still blocked on throwto (%p)",
                          (lnat)tso->id, tso->block_info.throwto->header.info);
            return;
        }

        // remove the block frame from the stack
        ASSERT(tso->sp[0] == (StgWord)&stg_block_throwto_info);
        tso->sp += 3;
        goto unblock;
    }

    case BlockedOnBlackHole:
    case BlockedOnSTM:
    case ThreadMigrating:
        goto unblock;

    default:
        // otherwise, do nothing
        return;
    }

unblock:
    // just run the thread now, if the BH is not really available,
    // we'll block again.
    tso->why_blocked = NotBlocked;
    appendToRunQueue(cap,tso);

    // We used to set the context switch flag here, which would
    // trigger a context switch a short time in the future (at the end
    // of the current nursery block).  The idea is that we have just
    // woken up a thread, so we may need to load-balance and migrate
    // threads to other CPUs.  On the other hand, setting the context
    // switch flag here unfairly penalises the current thread by
    // yielding its time slice too early.
    //
    // The synthetic benchmark nofib/smp/chan can be used to show the
    // difference quite clearly.

    // cap->context_switch = 1;
}

/* ----------------------------------------------------------------------------
   migrateThread
   ------------------------------------------------------------------------- */

void
migrateThread (Capability *from, StgTSO *tso, Capability *to)
{
    traceEventMigrateThread (from, tso, to->no);
    // ThreadMigrating tells the target cap that it needs to be added to
    // the run queue when it receives the MSG_TRY_WAKEUP.
    tso->why_blocked = ThreadMigrating;
    tso->cap = to;
    tryWakeupThread(from, tso);
}

/* ----------------------------------------------------------------------------
   awakenBlockedQueue

   wakes up all the threads on the specified queue.
   ------------------------------------------------------------------------- */

void
wakeBlockingQueue(Capability *cap, StgBlockingQueue *bq)
{
    MessageBlackHole *msg;
    const StgInfoTable *i;

    ASSERT(bq->header.info == &stg_BLOCKING_QUEUE_DIRTY_info  ||
           bq->header.info == &stg_BLOCKING_QUEUE_CLEAN_info  );

    for (msg = bq->queue; msg != (MessageBlackHole*)END_TSO_QUEUE; 
         msg = msg->link) {
        i = msg->header.info;
        if (i != &stg_IND_info) {
            ASSERT(i == &stg_MSG_BLACKHOLE_info);
            tryWakeupThread(cap,msg->tso);
        }
    }

    // overwrite the BQ with an indirection so it will be
    // collected at the next GC.
#if defined(DEBUG) && !defined(THREADED_RTS)
    // XXX FILL_SLOP, but not if THREADED_RTS because in that case
    // another thread might be looking at this BLOCKING_QUEUE and
    // checking the owner field at the same time.
    bq->bh = 0; bq->queue = 0; bq->owner = 0;
#endif
    OVERWRITE_INFO(bq, &stg_IND_info);
}

// If we update a closure that we know we BLACKHOLE'd, and the closure
// no longer points to the current TSO as its owner, then there may be
// an orphaned BLOCKING_QUEUE closure with blocked threads attached to
// it.  We therefore traverse the BLOCKING_QUEUEs attached to the
// current TSO to see if any can now be woken up.
void
checkBlockingQueues (Capability *cap, StgTSO *tso)
{
    StgBlockingQueue *bq, *next;
    StgClosure *p;

    debugTraceCap(DEBUG_sched, cap,
                  "collision occurred; checking blocking queues for thread %ld",
                  (lnat)tso->id);
    
    for (bq = tso->bq; bq != (StgBlockingQueue*)END_TSO_QUEUE; bq = next) {
        next = bq->link;

        if (bq->header.info == &stg_IND_info) {
            // ToDo: could short it out right here, to avoid
            // traversing this IND multiple times.
            continue;
        }
        
        p = bq->bh;

        if (p->header.info != &stg_BLACKHOLE_info ||
            ((StgInd *)p)->indirectee != (StgClosure*)bq)
        {
            wakeBlockingQueue(cap,bq);
        }   
    }
}

/* ----------------------------------------------------------------------------
   updateThunk

   Update a thunk with a value.  In order to do this, we need to know
   which TSO owns (or is evaluating) the thunk, in case we need to
   awaken any threads that are blocked on it.
   ------------------------------------------------------------------------- */

void
updateThunk (Capability *cap, StgTSO *tso, StgClosure *thunk, StgClosure *val)
{
    StgClosure *v;
    StgTSO *owner;
    const StgInfoTable *i;

    i = thunk->header.info;
    if (i != &stg_BLACKHOLE_info &&
        i != &stg_CAF_BLACKHOLE_info &&
        i != &__stg_EAGER_BLACKHOLE_info &&
        i != &stg_WHITEHOLE_info) {
        updateWithIndirection(cap, thunk, val);
        return;
    }
    
    v = ((StgInd*)thunk)->indirectee;

    updateWithIndirection(cap, thunk, val);

    i = v->header.info;
    if (i == &stg_TSO_info) {
        owner = deRefTSO((StgTSO*)v);
        if (owner != tso) {
            checkBlockingQueues(cap, tso);
        }
        return;
    }

    if (i != &stg_BLOCKING_QUEUE_CLEAN_info &&
        i != &stg_BLOCKING_QUEUE_DIRTY_info) {
        checkBlockingQueues(cap, tso);
        return;
    }

    owner = deRefTSO(((StgBlockingQueue*)v)->owner);

    if (owner != tso) {
        checkBlockingQueues(cap, tso);
    } else {
        wakeBlockingQueue(cap, (StgBlockingQueue*)v);
    }
}

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
    debugBelch("is blocked on proc (request: %u)", tso->block_info.async_result->reqID);
    break;
#endif
  case BlockedOnDelay:
    debugBelch("is blocked until %ld", (long)(tso->block_info.target));
    break;
  case BlockedOnMVar:
    debugBelch("is blocked on an MVar @ %p", tso->block_info.closure);
    break;
  case BlockedOnBlackHole:
      debugBelch("is blocked on a black hole %p", 
                 ((StgBlockingQueue*)tso->block_info.bh->bh));
    break;
  case BlockedOnMsgThrowTo:
    debugBelch("is blocked on a throwto message");
    break;
  case NotBlocked:
    debugBelch("is not blocked");
    break;
  case ThreadMigrating:
    debugBelch("is runnable, but not on the run queue");
    break;
  case BlockedOnCCall:
    debugBelch("is blocked on an external call");
    break;
  case BlockedOnCCall_Interruptible:
    debugBelch("is blocked on an external call (but may be interrupted)");
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
        if (t->dirty) {
            debugBelch(" (TSO_DIRTY)");
        } else if (t->flags & TSO_LINK_DIRTY) {
            debugBelch(" (TSO_LINK_DIRTY)");
        }
	debugBelch("\n");
    }
}

void
printAllThreads(void)
{
  StgTSO *t, *next;
  nat i, g;
  Capability *cap;

  debugBelch("all threads:\n");

  for (i = 0; i < n_capabilities; i++) {
      cap = &capabilities[i];
      debugBelch("threads on capability %d:\n", cap->no);
      for (t = cap->run_queue_hd; t != END_TSO_QUEUE; t = t->_link) {
	  printThreadStatus(t);
      }
  }

  debugBelch("other threads:\n");
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (t = generations[g].threads; t != END_TSO_QUEUE; t = next) {
      if (t->why_blocked != NotBlocked) {
	  printThreadStatus(t);
      }
      if (t->what_next == ThreadRelocated) {
	  next = t->_link;
      } else {
	  next = t->global_link;
      }
    }
  }
}

// useful from gdb
void 
printThreadQueue(StgTSO *t)
{
    nat i = 0;
    for (; t != END_TSO_QUEUE; t = t->_link) {
	printThreadStatus(t);
	i++;
    }
    debugBelch("%d threads on queue\n", i);
}

#endif /* DEBUG */
