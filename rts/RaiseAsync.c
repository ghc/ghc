/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * Asynchronous exceptions
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Threads.h"
#include "Trace.h"
#include "RaiseAsync.h"
#include "SMP.h"
#include "Schedule.h"
#include "LdvProfile.h"
#include "Updates.h"
#include "STM.h"
#include "Sanity.h"
#include "Profiling.h"
#if defined(mingw32_HOST_OS)
#include "win32/IOManager.h"
#endif

static void raiseAsync (Capability *cap,
			StgTSO *tso,
			StgClosure *exception, 
			rtsBool stop_at_atomically,
			StgPtr stop_here);

static void removeFromQueues(Capability *cap, StgTSO *tso);

static void blockedThrowTo (StgTSO *source, StgTSO *target);

static void performBlockedException (Capability *cap, 
				     StgTSO *source, StgTSO *target);

/* -----------------------------------------------------------------------------
   throwToSingleThreaded

   This version of throwTo is safe to use if and only if one of the
   following holds:
   
     - !THREADED_RTS

     - all the other threads in the system are stopped (eg. during GC).

     - we surely own the target TSO (eg. we just took it from the
       run queue of the current capability, or we are running it).

   It doesn't cater for blocking the source thread until the exception
   has been raised.
   -------------------------------------------------------------------------- */

void
throwToSingleThreaded(Capability *cap, StgTSO *tso, StgClosure *exception)
{
    throwToSingleThreaded_(cap, tso, exception, rtsFalse, NULL);
}

void
throwToSingleThreaded_(Capability *cap, StgTSO *tso, StgClosure *exception, 
		       rtsBool stop_at_atomically, StgPtr stop_here)
{
    // Thread already dead?
    if (tso->what_next == ThreadComplete || tso->what_next == ThreadKilled) {
	return;
    }

    // Remove it from any blocking queues
    removeFromQueues(cap,tso);

    raiseAsync(cap, tso, exception, stop_at_atomically, stop_here);
}

void
suspendComputation(Capability *cap, StgTSO *tso, StgPtr stop_here)
{
    // Thread already dead?
    if (tso->what_next == ThreadComplete || tso->what_next == ThreadKilled) {
	return;
    }

    // Remove it from any blocking queues
    removeFromQueues(cap,tso);

    raiseAsync(cap, tso, NULL, rtsFalse, stop_here);
}

/* -----------------------------------------------------------------------------
   throwTo

   This function may be used to throw an exception from one thread to
   another, during the course of normal execution.  This is a tricky
   task: the target thread might be running on another CPU, or it
   may be blocked and could be woken up at any point by another CPU.
   We have some delicate synchronisation to do.

   There is a completely safe fallback scheme: it is always possible
   to just block the source TSO on the target TSO's blocked_exceptions
   queue.  This queue is locked using lockTSO()/unlockTSO().  It is
   checked at regular intervals: before and after running a thread
   (schedule() and threadPaused() respectively), and just before GC
   (scheduleDoGC()).  Activating a thread on this queue should be done
   using maybePerformBlockedException(): this is done in the context
   of the target thread, so the exception can be raised eagerly.

   This fallback scheme works even if the target thread is complete or
   killed: scheduleDoGC() will discover the blocked thread before the
   target is GC'd.

   Blocking the source thread on the target thread's blocked_exception
   queue is also employed when the target thread is currently blocking
   exceptions (ie. inside Control.Exception.block).

   We could use the safe fallback scheme exclusively, but that
   wouldn't be ideal: most calls to throwTo would block immediately,
   possibly until the next GC, which might require the deadlock
   detection mechanism to kick in.  So we try to provide promptness
   wherever possible.

   We can promptly deliver the exception if the target thread is:

     - runnable, on the same Capability as the source thread (because
       we own the run queue and therefore the target thread).
   
     - blocked, and we can obtain exclusive access to it.  Obtaining
       exclusive access to the thread depends on how it is blocked.

   We must also be careful to not trip over threadStackOverflow(),
   which might be moving the TSO to enlarge its stack.
   lockTSO()/unlockTSO() are used here too.

   Returns: 

   THROWTO_SUCCESS    exception was raised, ok to continue

   THROWTO_BLOCKED    exception was not raised; block the source
                      thread then call throwToReleaseTarget() when
		      the source thread is properly tidied away.

   -------------------------------------------------------------------------- */

nat
throwTo (Capability *cap,	// the Capability we hold 
	 StgTSO *source,	// the TSO sending the exception
	 StgTSO *target,        // the TSO receiving the exception
	 StgClosure *exception, // the exception closure
	 /*[out]*/ void **out USED_IF_THREADS)
{
    StgWord status;

    // follow ThreadRelocated links in the target first
    while (target->what_next == ThreadRelocated) {
	target = target->link;
	// No, it might be a WHITEHOLE:
	// ASSERT(get_itbl(target)->type == TSO);
    }

    debugTrace(DEBUG_sched, "throwTo: from thread %lu to thread %lu",
	       (unsigned long)source->id, (unsigned long)target->id);

#ifdef DEBUG
    if (traceClass(DEBUG_sched)) {
	debugTraceBegin("throwTo: target");
	printThreadStatus(target);
	debugTraceEnd();
    }
#endif

    goto check_target;
retry:
    debugTrace(DEBUG_sched, "throwTo: retrying...");

check_target:
    // Thread already dead?
    if (target->what_next == ThreadComplete 
	|| target->what_next == ThreadKilled) {
	return THROWTO_SUCCESS;
    }

    status = target->why_blocked;
    
    switch (status) {
    case NotBlocked:
	/* if status==NotBlocked, and target->cap == cap, then
	   we own this TSO and can raise the exception.
	   
	   How do we establish this condition?  Very carefully.

	   Let 
	       P = (status == NotBlocked)
	       Q = (tso->cap == cap)
	       
	   Now, if P & Q are true, then the TSO is locked and owned by
	   this capability.  No other OS thread can steal it.

	   If P==0 and Q==1: the TSO is blocked, but attached to this
	   capabilty, and it can be stolen by another capability.
	   
	   If P==1 and Q==0: the TSO is runnable on another
	   capability.  At any time, the TSO may change from runnable
	   to blocked and vice versa, while it remains owned by
	   another capability.

	   Suppose we test like this:

	      p = P
	      q = Q
	      if (p && q) ...

	    this is defeated by another capability stealing a blocked
	    TSO from us to wake it up (Schedule.c:unblockOne()).  The
	    other thread is doing

	      Q = 0
	      P = 1

	    assuming arbitrary reordering, we could see this
	    interleaving:

	      start: P==0 && Q==1 
	      P = 1
	      p = P
	      q = Q
	      Q = 0
	      if (p && q) ...
	       
	    so we need a memory barrier:

	      p = P
	      mb()
	      q = Q
	      if (p && q) ...

	    this avoids the problematic case.  There are other cases
	    to consider, but this is the tricky one.

	    Note that we must be sure that unblockOne() does the
	    writes in the correct order: Q before P.  The memory
	    barrier ensures that if we have seen the write to P, we
	    have also seen the write to Q.
	*/
    {
	Capability *target_cap;

	write_barrier();
	target_cap = target->cap;
	if (target_cap == cap && (target->flags & TSO_BLOCKEX) == 0) {
	    // It's on our run queue and not blocking exceptions
	    raiseAsync(cap, target, exception, rtsFalse, NULL);
	    return THROWTO_SUCCESS;
	} else {
	    // Otherwise, just block on the blocked_exceptions queue
	    // of the target thread.  The queue will get looked at
	    // soon enough: it is checked before and after running a
	    // thread, and during GC.
	    lockTSO(target);

	    // Avoid race with threadStackOverflow, which may have
	    // just moved this TSO.
	    if (target->what_next == ThreadRelocated) {
		unlockTSO(target);
		target = target->link;
		goto retry;
	    }
	    blockedThrowTo(source,target);
	    *out = target;
	    return THROWTO_BLOCKED;
	}
    }

    case BlockedOnMVar:
    {
	/*
	  To establish ownership of this TSO, we need to acquire a
	  lock on the MVar that it is blocked on.
	*/
	StgMVar *mvar;
	StgInfoTable *info USED_IF_THREADS;
	
	mvar = (StgMVar *)target->block_info.closure;

	// ASSUMPTION: tso->block_info must always point to a
	// closure.  In the threaded RTS it does.
        switch (get_itbl(mvar)->type) {
        case MVAR_CLEAN:
        case MVAR_DIRTY:
            break;
        default:
            goto retry;
        }

	info = lockClosure((StgClosure *)mvar);

	if (target->what_next == ThreadRelocated) {
	    target = target->link;
	    unlockClosure((StgClosure *)mvar,info);
	    goto retry;
	}
	// we have the MVar, let's check whether the thread
	// is still blocked on the same MVar.
	if (target->why_blocked != BlockedOnMVar
	    || (StgMVar *)target->block_info.closure != mvar) {
	    unlockClosure((StgClosure *)mvar, info);
	    goto retry;
	}

	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
	    lockClosure((StgClosure *)target);
	    blockedThrowTo(source,target);
	    unlockClosure((StgClosure *)mvar, info);
	    *out = target;
	    return THROWTO_BLOCKED; // caller releases TSO
	} else {
	    removeThreadFromMVarQueue(mvar, target);
	    raiseAsync(cap, target, exception, rtsFalse, NULL);
	    unblockOne(cap, target);
	    unlockClosure((StgClosure *)mvar, info);
	    return THROWTO_SUCCESS;
	}
    }

    case BlockedOnBlackHole:
    {
	ACQUIRE_LOCK(&sched_mutex);
	// double checking the status after the memory barrier:
	if (target->why_blocked != BlockedOnBlackHole) {
	    RELEASE_LOCK(&sched_mutex);
	    goto retry;
	}

	if (target->flags & TSO_BLOCKEX) {
	    lockTSO(target);
	    blockedThrowTo(source,target);
	    RELEASE_LOCK(&sched_mutex);
	    *out = target;
	    return THROWTO_BLOCKED; // caller releases TSO
	} else {
	    removeThreadFromQueue(&blackhole_queue, target);
	    raiseAsync(cap, target, exception, rtsFalse, NULL);
	    unblockOne(cap, target);
	    RELEASE_LOCK(&sched_mutex);
	    return THROWTO_SUCCESS;
	}
    }

    case BlockedOnException:
    {
	StgTSO *target2;
	StgInfoTable *info;

	/*
	  To obtain exclusive access to a BlockedOnException thread,
	  we must call lockClosure() on the TSO on which it is blocked.
	  Since the TSO might change underneath our feet, after we
	  call lockClosure() we must check that 
	   
             (a) the closure we locked is actually a TSO
	     (b) the original thread is still  BlockedOnException,
	     (c) the original thread is still blocked on the TSO we locked
	     and (d) the target thread has not been relocated.

	  We synchronise with threadStackOverflow() (which relocates
	  threads) using lockClosure()/unlockClosure().
	*/
	target2 = target->block_info.tso;

	info = lockClosure((StgClosure *)target2);
	if (info != &stg_TSO_info) {
	    unlockClosure((StgClosure *)target2, info);
	    goto retry;
	}
	if (target->what_next == ThreadRelocated) {
	    target = target->link;
	    unlockTSO(target2);
	    goto retry;
	}
	if (target2->what_next == ThreadRelocated) {
	    target->block_info.tso = target2->link;
	    unlockTSO(target2);
	    goto retry;
	}
	if (target->why_blocked != BlockedOnException
	    || target->block_info.tso != target2) {
	    unlockTSO(target2);
	    goto retry;
	}
	
	/* 
	   Now we have exclusive rights to the target TSO...

	   If it is blocking exceptions, add the source TSO to its
	   blocked_exceptions queue.  Otherwise, raise the exception.
	*/
	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
	    lockTSO(target);
	    blockedThrowTo(source,target);
	    unlockTSO(target2);
	    *out = target;
	    return THROWTO_BLOCKED;
	} else {
	    removeThreadFromQueue(&target2->blocked_exceptions, target);
	    raiseAsync(cap, target, exception, rtsFalse, NULL);
	    unblockOne(cap, target);
	    unlockTSO(target2);
	    return THROWTO_SUCCESS;
	}
    }	

    case BlockedOnSTM:
	lockTSO(target);
	// Unblocking BlockedOnSTM threads requires the TSO to be
	// locked; see STM.c:unpark_tso().
	if (target->why_blocked != BlockedOnSTM) {
	    goto retry;
	}
	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
	    blockedThrowTo(source,target);
	    *out = target;
	    return THROWTO_BLOCKED;
	} else {
	    raiseAsync(cap, target, exception, rtsFalse, NULL);
	    unblockOne(cap, target);
	    unlockTSO(target);
	    return THROWTO_SUCCESS;
	}

    case BlockedOnCCall:
    case BlockedOnCCall_NoUnblockExc:
	// I don't think it's possible to acquire ownership of a
	// BlockedOnCCall thread.  We just assume that the target
	// thread is blocking exceptions, and block on its
	// blocked_exception queue.
	lockTSO(target);
	blockedThrowTo(source,target);
	*out = target;
	return THROWTO_BLOCKED;

#ifndef THREADEDED_RTS
    case BlockedOnRead:
    case BlockedOnWrite:
    case BlockedOnDelay:
#if defined(mingw32_HOST_OS)
    case BlockedOnDoProc:
#endif
	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
	    blockedThrowTo(source,target);
	    return THROWTO_BLOCKED;
	} else {
	    removeFromQueues(cap,target);
	    raiseAsync(cap, target, exception, rtsFalse, NULL);
	    return THROWTO_SUCCESS;
	}
#endif

    default:
	barf("throwTo: unrecognised why_blocked value");
    }
    barf("throwTo");
}

// Block a TSO on another TSO's blocked_exceptions queue.
// Precondition: we hold an exclusive lock on the target TSO (this is
// complex to achieve as there's no single lock on a TSO; see
// throwTo()).
static void
blockedThrowTo (StgTSO *source, StgTSO *target)
{
    debugTrace(DEBUG_sched, "throwTo: blocking on thread %lu", (unsigned long)target->id);
    source->link = target->blocked_exceptions;
    target->blocked_exceptions = source;
    dirtyTSO(target); // we modified the blocked_exceptions queue
    
    source->block_info.tso = target;
    write_barrier(); // throwTo_exception *must* be visible if BlockedOnException is.
    source->why_blocked = BlockedOnException;
}


#ifdef THREADED_RTS
void
throwToReleaseTarget (void *tso)
{
    unlockTSO((StgTSO *)tso);
}
#endif

/* -----------------------------------------------------------------------------
   Waking up threads blocked in throwTo

   There are two ways to do this: maybePerformBlockedException() will
   perform the throwTo() for the thread at the head of the queue
   immediately, and leave the other threads on the queue.
   maybePerformBlockedException() also checks the TSO_BLOCKEX flag
   before raising an exception.

   awakenBlockedExceptionQueue() will wake up all the threads in the
   queue, but not perform any throwTo() immediately.  This might be
   more appropriate when the target thread is the one actually running
   (see Exception.cmm).

   Returns: non-zero if an exception was raised, zero otherwise.
   -------------------------------------------------------------------------- */

int
maybePerformBlockedException (Capability *cap, StgTSO *tso)
{
    StgTSO *source;
    
    if (tso->blocked_exceptions != END_TSO_QUEUE
	&& ((tso->flags & TSO_BLOCKEX) == 0
	    || ((tso->flags & TSO_INTERRUPTIBLE) && interruptible(tso)))) {

	// Lock the TSO, this gives us exclusive access to the queue
	lockTSO(tso);

	// Check the queue again; it might have changed before we
	// locked it.
	if (tso->blocked_exceptions == END_TSO_QUEUE) {
	    unlockTSO(tso);
	    return 0;
	}

	// We unblock just the first thread on the queue, and perform
	// its throw immediately.
	source = tso->blocked_exceptions;
	performBlockedException(cap, source, tso);
	tso->blocked_exceptions = unblockOne_(cap, source, 
					      rtsFalse/*no migrate*/);
	unlockTSO(tso);
        return 1;
    }
    return 0;
}

void
awakenBlockedExceptionQueue (Capability *cap, StgTSO *tso)
{
    if (tso->blocked_exceptions != END_TSO_QUEUE) {
	lockTSO(tso);
	awakenBlockedQueue(cap, tso->blocked_exceptions);
	tso->blocked_exceptions = END_TSO_QUEUE;
	unlockTSO(tso);
    }
}    

static void
performBlockedException (Capability *cap, StgTSO *source, StgTSO *target)
{
    StgClosure *exception;

    ASSERT(source->why_blocked == BlockedOnException);
    ASSERT(source->block_info.tso->id == target->id);
    ASSERT(source->sp[0] == (StgWord)&stg_block_throwto_info);
    ASSERT(((StgTSO *)source->sp[1])->id == target->id);
    // check ids not pointers, because the thread might be relocated

    exception = (StgClosure *)source->sp[2];
    throwToSingleThreaded(cap, target, exception);
    source->sp += 3;
}

/* -----------------------------------------------------------------------------
   Remove a thread from blocking queues.

   This is for use when we raise an exception in another thread, which
   may be blocked.
   This has nothing to do with the UnblockThread event in GranSim. -- HWL
   -------------------------------------------------------------------------- */

#if defined(GRAN) || defined(PARALLEL_HASKELL)
/*
  NB: only the type of the blocking queue is different in GranSim and GUM
      the operations on the queue-elements are the same
      long live polymorphism!

  Locks: sched_mutex is held upon entry and exit.

*/
static void
removeFromQueues(Capability *cap, StgTSO *tso)
{
  StgBlockingQueueElement *t, **last;

  switch (tso->why_blocked) {

  case NotBlocked:
    return;  /* not blocked */

  case BlockedOnSTM:
    // Be careful: nothing to do here!  We tell the scheduler that the thread
    // is runnable and we leave it to the stack-walking code to abort the 
    // transaction while unwinding the stack.  We should perhaps have a debugging
    // test to make sure that this really happens and that the 'zombie' transaction
    // does not get committed.
    goto done;

  case BlockedOnMVar:
    ASSERT(get_itbl(tso->block_info.closure)->type == MVAR);
    {
      StgBlockingQueueElement *last_tso = END_BQ_QUEUE;
      StgMVar *mvar = (StgMVar *)(tso->block_info.closure);

      last = (StgBlockingQueueElement **)&mvar->head;
      for (t = (StgBlockingQueueElement *)mvar->head; 
	   t != END_BQ_QUEUE; 
	   last = &t->link, last_tso = t, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  *last = (StgBlockingQueueElement *)tso->link;
	  if (mvar->tail == tso) {
	    mvar->tail = (StgTSO *)last_tso;
	  }
	  goto done;
	}
      }
      barf("removeFromQueues (MVAR): TSO not found");
    }

  case BlockedOnBlackHole:
    ASSERT(get_itbl(tso->block_info.closure)->type == BLACKHOLE_BQ);
    {
      StgBlockingQueue *bq = (StgBlockingQueue *)(tso->block_info.closure);

      last = &bq->blocking_queue;
      for (t = bq->blocking_queue; 
	   t != END_BQ_QUEUE; 
	   last = &t->link, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  *last = (StgBlockingQueueElement *)tso->link;
	  goto done;
	}
      }
      barf("removeFromQueues (BLACKHOLE): TSO not found");
    }

  case BlockedOnException:
    {
      StgTSO *target  = tso->block_info.tso;

      ASSERT(get_itbl(target)->type == TSO);

      while (target->what_next == ThreadRelocated) {
	  target = target2->link;
	  ASSERT(get_itbl(target)->type == TSO);
      }

      last = (StgBlockingQueueElement **)&target->blocked_exceptions;
      for (t = (StgBlockingQueueElement *)target->blocked_exceptions; 
	   t != END_BQ_QUEUE; 
	   last = &t->link, t = t->link) {
	ASSERT(get_itbl(t)->type == TSO);
	if (t == (StgBlockingQueueElement *)tso) {
	  *last = (StgBlockingQueueElement *)tso->link;
	  goto done;
	}
      }
      barf("removeFromQueues (Exception): TSO not found");
    }

  case BlockedOnRead:
  case BlockedOnWrite:
#if defined(mingw32_HOST_OS)
  case BlockedOnDoProc:
#endif
    {
      /* take TSO off blocked_queue */
      StgBlockingQueueElement *prev = NULL;
      for (t = (StgBlockingQueueElement *)blocked_queue_hd; t != END_BQ_QUEUE; 
	   prev = t, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  if (prev == NULL) {
	    blocked_queue_hd = (StgTSO *)t->link;
	    if ((StgBlockingQueueElement *)blocked_queue_tl == t) {
	      blocked_queue_tl = END_TSO_QUEUE;
	    }
	  } else {
	    prev->link = t->link;
	    if ((StgBlockingQueueElement *)blocked_queue_tl == t) {
	      blocked_queue_tl = (StgTSO *)prev;
	    }
	  }
#if defined(mingw32_HOST_OS)
	  /* (Cooperatively) signal that the worker thread should abort
	   * the request.
	   */
	  abandonWorkRequest(tso->block_info.async_result->reqID);
#endif
	  goto done;
	}
      }
      barf("removeFromQueues (I/O): TSO not found");
    }

  case BlockedOnDelay:
    {
      /* take TSO off sleeping_queue */
      StgBlockingQueueElement *prev = NULL;
      for (t = (StgBlockingQueueElement *)sleeping_queue; t != END_BQ_QUEUE; 
	   prev = t, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  if (prev == NULL) {
	    sleeping_queue = (StgTSO *)t->link;
	  } else {
	    prev->link = t->link;
	  }
	  goto done;
	}
      }
      barf("removeFromQueues (delay): TSO not found");
    }

  default:
    barf("removeFromQueues");
  }

 done:
  tso->link = END_TSO_QUEUE;
  tso->why_blocked = NotBlocked;
  tso->block_info.closure = NULL;
  pushOnRunQueue(cap,tso);
}
#else
static void
removeFromQueues(Capability *cap, StgTSO *tso)
{
  switch (tso->why_blocked) {

  case NotBlocked:
      return;

  case BlockedOnSTM:
    // Be careful: nothing to do here!  We tell the scheduler that the
    // thread is runnable and we leave it to the stack-walking code to
    // abort the transaction while unwinding the stack.  We should
    // perhaps have a debugging test to make sure that this really
    // happens and that the 'zombie' transaction does not get
    // committed.
    goto done;

  case BlockedOnMVar:
      removeThreadFromMVarQueue((StgMVar *)tso->block_info.closure, tso);
      goto done;

  case BlockedOnBlackHole:
      removeThreadFromQueue(&blackhole_queue, tso);
      goto done;

  case BlockedOnException:
    {
      StgTSO *target  = tso->block_info.tso;

      // NO: when called by threadPaused(), we probably have this
      // TSO already locked (WHITEHOLEd) because we just placed
      // ourselves on its queue.
      // ASSERT(get_itbl(target)->type == TSO);

      while (target->what_next == ThreadRelocated) {
	  target = target->link;
      }
      
      removeThreadFromQueue(&target->blocked_exceptions, tso);
      goto done;
    }

#if !defined(THREADED_RTS)
  case BlockedOnRead:
  case BlockedOnWrite:
#if defined(mingw32_HOST_OS)
  case BlockedOnDoProc:
#endif
      removeThreadFromDeQueue(&blocked_queue_hd, &blocked_queue_tl, tso);
#if defined(mingw32_HOST_OS)
      /* (Cooperatively) signal that the worker thread should abort
       * the request.
       */
      abandonWorkRequest(tso->block_info.async_result->reqID);
#endif
      goto done;

  case BlockedOnDelay:
	removeThreadFromQueue(&sleeping_queue, tso);
	goto done;
#endif

  default:
      barf("removeFromQueues");
  }

 done:
  tso->link = END_TSO_QUEUE;
  tso->why_blocked = NotBlocked;
  tso->block_info.closure = NULL;
  appendToRunQueue(cap,tso);

  // We might have just migrated this TSO to our Capability:
  if (tso->bound) {
      tso->bound->cap = cap;
  }
  tso->cap = cap;
}
#endif

/* -----------------------------------------------------------------------------
 * raiseAsync()
 *
 * The following function implements the magic for raising an
 * asynchronous exception in an existing thread.
 *
 * We first remove the thread from any queue on which it might be
 * blocked.  The possible blockages are MVARs and BLACKHOLE_BQs.
 *
 * We strip the stack down to the innermost CATCH_FRAME, building
 * thunks in the heap for all the active computations, so they can 
 * be restarted if necessary.  When we reach a CATCH_FRAME, we build
 * an application of the handler to the exception, and push it on
 * the top of the stack.
 * 
 * How exactly do we save all the active computations?  We create an
 * AP_STACK for every UpdateFrame on the stack.  Entering one of these
 * AP_STACKs pushes everything from the corresponding update frame
 * upwards onto the stack.  (Actually, it pushes everything up to the
 * next update frame plus a pointer to the next AP_STACK object.
 * Entering the next AP_STACK object pushes more onto the stack until we
 * reach the last AP_STACK object - at which point the stack should look
 * exactly as it did when we killed the TSO and we can continue
 * execution by entering the closure on top of the stack.
 *
 * We can also kill a thread entirely - this happens if either (a) the 
 * exception passed to raiseAsync is NULL, or (b) there's no
 * CATCH_FRAME on the stack.  In either case, we strip the entire
 * stack and replace the thread with a zombie.
 *
 * ToDo: in THREADED_RTS mode, this function is only safe if either
 * (a) we hold all the Capabilities (eg. in GC, or if there is only
 * one Capability), or (b) we own the Capability that the TSO is
 * currently blocked on or on the run queue of.
 *
 * -------------------------------------------------------------------------- */

static void
raiseAsync(Capability *cap, StgTSO *tso, StgClosure *exception, 
	   rtsBool stop_at_atomically, StgPtr stop_here)
{
    StgRetInfoTable *info;
    StgPtr sp, frame;
    nat i;

    debugTrace(DEBUG_sched,
	       "raising exception in thread %ld.", (long)tso->id);
    
#if defined(PROFILING)
    /* 
     * Debugging tool: on raising an  exception, show where we are.
     * See also Exception.cmm:raisezh_fast.
     * This wasn't done for asynchronous exceptions originally; see #1450 
     */
    if (RtsFlags.ProfFlags.showCCSOnException)
    {
        fprintCCS_stderr(tso->prof.CCCS);
    }
#endif

    // mark it dirty; we're about to change its stack.
    dirtyTSO(tso);

    sp = tso->sp;
    
    // ASSUMES: the thread is not already complete or dead.  Upper
    // layers should deal with that.
    ASSERT(tso->what_next != ThreadComplete && tso->what_next != ThreadKilled);

    // The stack freezing code assumes there's a closure pointer on
    // the top of the stack, so we have to arrange that this is the case...
    //
    if (sp[0] == (W_)&stg_enter_info) {
	sp++;
    } else {
	sp--;
	sp[0] = (W_)&stg_dummy_ret_closure;
    }

    frame = sp + 1;
    while (stop_here == NULL || frame < stop_here) {

	// 1. Let the top of the stack be the "current closure"
	//
	// 2. Walk up the stack until we find either an UPDATE_FRAME or a
	// CATCH_FRAME.
	//
	// 3. If it's an UPDATE_FRAME, then make an AP_STACK containing the
	// current closure applied to the chunk of stack up to (but not
	// including) the update frame.  This closure becomes the "current
	// closure".  Go back to step 2.
	//
	// 4. If it's a CATCH_FRAME, then leave the exception handler on
	// top of the stack applied to the exception.
	// 
	// 5. If it's a STOP_FRAME, then kill the thread.
        // 
        // NB: if we pass an ATOMICALLY_FRAME then abort the associated 
        // transaction
       
	info = get_ret_itbl((StgClosure *)frame);

	switch (info->i.type) {

	case UPDATE_FRAME:
	{
	    StgAP_STACK * ap;
	    nat words;
	    
	    // First build an AP_STACK consisting of the stack chunk above the
	    // current update frame, with the top word on the stack as the
	    // fun field.
	    //
	    words = frame - sp - 1;
	    ap = (StgAP_STACK *)allocateLocal(cap,AP_STACK_sizeW(words));
	    
	    ap->size = words;
	    ap->fun  = (StgClosure *)sp[0];
	    sp++;
	    for(i=0; i < (nat)words; ++i) {
		ap->payload[i] = (StgClosure *)*sp++;
	    }
	    
	    SET_HDR(ap,&stg_AP_STACK_info,
		    ((StgClosure *)frame)->header.prof.ccs /* ToDo */); 
	    TICK_ALLOC_UP_THK(words+1,0);
	    
	    //IF_DEBUG(scheduler,
	    //	     debugBelch("sched: Updating ");
	    //	     printPtr((P_)((StgUpdateFrame *)frame)->updatee); 
	    //	     debugBelch(" with ");
	    //	     printObj((StgClosure *)ap);
	    //	);

	    // Replace the updatee with an indirection
	    //
	    // Warning: if we're in a loop, more than one update frame on
	    // the stack may point to the same object.  Be careful not to
	    // overwrite an IND_OLDGEN in this case, because we'll screw
	    // up the mutable lists.  To be on the safe side, don't
	    // overwrite any kind of indirection at all.  See also
	    // threadSqueezeStack in GC.c, where we have to make a similar
	    // check.
	    //
	    if (!closure_IND(((StgUpdateFrame *)frame)->updatee)) {
		// revert the black hole
		UPD_IND_NOLOCK(((StgUpdateFrame *)frame)->updatee,
			       (StgClosure *)ap);
	    }
	    sp += sizeofW(StgUpdateFrame) - 1;
	    sp[0] = (W_)ap; // push onto stack
	    frame = sp + 1;
	    continue; //no need to bump frame
	}

	case STOP_FRAME:
	{
	    // We've stripped the entire stack, the thread is now dead.
	    tso->what_next = ThreadKilled;
	    tso->sp = frame + sizeofW(StgStopFrame);
	    return;
	}

	case CATCH_FRAME:
	    // If we find a CATCH_FRAME, and we've got an exception to raise,
	    // then build the THUNK raise(exception), and leave it on
	    // top of the CATCH_FRAME ready to enter.
	    //
	{
#ifdef PROFILING
	    StgCatchFrame *cf = (StgCatchFrame *)frame;
#endif
	    StgThunk *raise;
	    
	    if (exception == NULL) break;

	    // we've got an exception to raise, so let's pass it to the
	    // handler in this frame.
	    //
	    raise = (StgThunk *)allocateLocal(cap,sizeofW(StgThunk)+1);
	    TICK_ALLOC_SE_THK(1,0);
	    SET_HDR(raise,&stg_raise_info,cf->header.prof.ccs);
	    raise->payload[0] = exception;
	    
	    // throw away the stack from Sp up to the CATCH_FRAME.
	    //
	    sp = frame - 1;
	    
	    /* Ensure that async excpetions are blocked now, so we don't get
	     * a surprise exception before we get around to executing the
	     * handler.
	     */
	    tso->flags |= TSO_BLOCKEX | TSO_INTERRUPTIBLE;

	    /* Put the newly-built THUNK on top of the stack, ready to execute
	     * when the thread restarts.
	     */
	    sp[0] = (W_)raise;
	    sp[-1] = (W_)&stg_enter_info;
	    tso->sp = sp-1;
	    tso->what_next = ThreadRunGHC;
	    IF_DEBUG(sanity, checkTSO(tso));
	    return;
	}
	    
	case ATOMICALLY_FRAME:
	    if (stop_at_atomically) {
		ASSERT(stmGetEnclosingTRec(tso->trec) == NO_TREC);
		stmCondemnTransaction(cap, tso -> trec);
#ifdef REG_R1
		tso->sp = frame;
#else
		// R1 is not a register: the return convention for IO in
		// this case puts the return value on the stack, so we
		// need to set up the stack to return to the atomically
		// frame properly...
		tso->sp = frame - 2;
		tso->sp[1] = (StgWord) &stg_NO_FINALIZER_closure; // why not?
		tso->sp[0] = (StgWord) &stg_ut_1_0_unreg_info;
#endif
		tso->what_next = ThreadRunGHC;
		return;
	    }
	    // Not stop_at_atomically... fall through and abort the
	    // transaction.
	    
	case CATCH_RETRY_FRAME:
	    // IF we find an ATOMICALLY_FRAME then we abort the
	    // current transaction and propagate the exception.  In
	    // this case (unlike ordinary exceptions) we do not care
	    // whether the transaction is valid or not because its
	    // possible validity cannot have caused the exception
	    // and will not be visible after the abort.

		{
            StgTRecHeader *trec = tso -> trec;
            StgTRecHeader *outer = stmGetEnclosingTRec(trec);
	    debugTrace(DEBUG_stm, 
		       "found atomically block delivering async exception");
            stmAbortTransaction(cap, trec);
	    stmFreeAbortedTRec(cap, trec);
            tso -> trec = outer;
	    break;
	    };
	    
	default:
	    break;
	}

	// move on to the next stack frame
	frame += stack_frame_sizeW((StgClosure *)frame);
    }

    // if we got here, then we stopped at stop_here
    ASSERT(stop_here != NULL);
}


