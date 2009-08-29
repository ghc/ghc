/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Threads.h"
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
    tso = (StgTSO *)allocateLocal(cap, size);

    stack_size = size - TSO_STRUCT_SIZEW;
    TICK_ALLOC_TSO(stack_size, 0);

    SET_HDR(tso, &stg_TSO_info, CCS_SYSTEM);

    // Always start with the compiled code evaluator
    tso->what_next = ThreadRunGHC;

    tso->why_blocked  = NotBlocked;
    tso->blocked_exceptions = END_TSO_QUEUE;
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
    tso->global_link = g0s0->threads;
    g0s0->threads = tso;
    RELEASE_LOCK(&sched_mutex);
    
    // ToDo: report the stack size in the event?
    traceSchedEvent (cap, EVENT_CREATE_THREAD, tso, tso->stack_size);

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

void
removeThreadFromQueue (Capability *cap, StgTSO **queue, StgTSO *tso)
{
    StgTSO *t, *prev;

    prev = NULL;
    for (t = *queue; t != END_TSO_QUEUE; prev = t, t = t->_link) {
	if (t == tso) {
	    if (prev) {
		setTSOLink(cap,prev,t->_link);
	    } else {
		*queue = t->_link;
	    }
	    return;
	}
    }
    barf("removeThreadFromQueue: not found");
}

void
removeThreadFromDeQueue (Capability *cap, 
                         StgTSO **head, StgTSO **tail, StgTSO *tso)
{
    StgTSO *t, *prev;

    prev = NULL;
    for (t = *head; t != END_TSO_QUEUE; prev = t, t = t->_link) {
	if (t == tso) {
	    if (prev) {
		setTSOLink(cap,prev,t->_link);
	    } else {
		*head = t->_link;
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
removeThreadFromMVarQueue (Capability *cap, StgMVar *mvar, StgTSO *tso)
{
    removeThreadFromDeQueue (cap, &mvar->head, &mvar->tail, tso);
}

/* ----------------------------------------------------------------------------
   unblockOne()

   unblock a single thread.
   ------------------------------------------------------------------------- */

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
  next = tso->_link;
  tso->_link = END_TSO_QUEUE;

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

      // context-switch soonish so we can migrate the new thread if
      // necessary.  NB. not contextSwitchCapability(cap), which would
      // force a context switch immediately.
      cap->context_switch = 1;
  } else {
      // we'll try to wake it up on the Capability it was last on.
      wakeupThreadOnCapability(cap, tso->cap, tso);
  }
#else
  appendToRunQueue(cap,tso);

  // context-switch soonish so we can migrate the new thread if
  // necessary.  NB. not contextSwitchCapability(cap), which would
  // force a context switch immediately.
  cap->context_switch = 1;
#endif

  traceSchedEvent (cap, EVENT_THREAD_WAKEUP, tso, tso->cap->no);

  return next;
}

/* ----------------------------------------------------------------------------
   awakenBlockedQueue

   wakes up all the threads on the specified queue.
   ------------------------------------------------------------------------- */

void
awakenBlockedQueue(Capability *cap, StgTSO *tso)
{
    while (tso != END_TSO_QUEUE) {
	tso = unblockOne(cap,tso);
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
  nat i, s;
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
      for (t = cap->run_queue_hd; t != END_TSO_QUEUE; t = t->_link) {
	  printThreadStatus(t);
      }
  }

  debugBelch("other threads:\n");
  for (s = 0; s < total_steps; s++) {
    for (t = all_steps[s].threads; t != END_TSO_QUEUE; t = next) {
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
