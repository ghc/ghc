/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * Asynchronous exceptions
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "sm/Storage.h"
#include "Threads.h"
#include "Trace.h"
#include "RaiseAsync.h"
#include "Schedule.h"
#include "Updates.h"
#include "STM.h"
#include "sm/Sanity.h"
#include "Profiling.h"
#include "Messages.h"
#if defined(mingw32_HOST_OS)
#include "win32/IOManager.h"
#endif

static StgTSO* raiseAsync (Capability *cap,
                           StgTSO *tso,
                           StgClosure *exception,
                           rtsBool stop_at_atomically,
                           StgUpdateFrame *stop_here);

static void removeFromQueues(Capability *cap, StgTSO *tso);

static void removeFromMVarBlockedQueue (StgTSO *tso);

static void throwToSendMsg (Capability *cap USED_IF_THREADS,
                            Capability *target_cap USED_IF_THREADS, 
                            MessageThrowTo *msg USED_IF_THREADS);

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

static void
throwToSingleThreaded__ (Capability *cap, StgTSO *tso, StgClosure *exception, 
                         rtsBool stop_at_atomically, StgUpdateFrame *stop_here)
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
throwToSingleThreaded (Capability *cap, StgTSO *tso, StgClosure *exception)
{
    throwToSingleThreaded__(cap, tso, exception, rtsFalse, NULL);
}

void
throwToSingleThreaded_ (Capability *cap, StgTSO *tso, StgClosure *exception,
                        rtsBool stop_at_atomically)
{
    throwToSingleThreaded__ (cap, tso, exception, stop_at_atomically, NULL);
}

void // cannot return a different TSO
suspendComputation (Capability *cap, StgTSO *tso, StgUpdateFrame *stop_here)
{
    throwToSingleThreaded__ (cap, tso, NULL, rtsFalse, stop_here);
}

/* -----------------------------------------------------------------------------
   throwTo

   This function may be used to throw an exception from one thread to
   another, during the course of normal execution.  This is a tricky
   task: the target thread might be running on another CPU, or it
   may be blocked and could be woken up at any point by another CPU.
   We have some delicate synchronisation to do.

   The underlying scheme when multiple Capabilities are in use is
   message passing: when the target of a throwTo is on another
   Capability, we send a message (a MessageThrowTo closure) to that
   Capability.

   If the throwTo needs to block because the target TSO is masking
   exceptions (the TSO_BLOCKEX flag), then the message is placed on
   the blocked_exceptions queue attached to the target TSO.  When the
   target TSO enters the unmasked state again, it must check the
   queue.  The blocked_exceptions queue is not locked; only the
   Capability owning the TSO may modify it.

   To make things simpler for throwTo, we always create the message
   first before deciding what to do.  The message may get sent, or it
   may get attached to a TSO's blocked_exceptions queue, or the
   exception may get thrown immediately and the message dropped,
   depending on the current state of the target.

   Currently we send a message if the target belongs to another
   Capability, and it is

     - NotBlocked, BlockedOnMsgThrowTo,
       BlockedOnCCall_Interruptible

     - or it is masking exceptions (TSO_BLOCKEX)

   Currently, if the target is BlockedOnMVar, BlockedOnSTM, or
   BlockedOnBlackHole then we acquire ownership of the TSO by locking
   its parent container (e.g. the MVar) and then raise the exception.
   We might change these cases to be more message-passing-like in the
   future.
  
   Returns: 

   NULL               exception was raised, ok to continue

   MessageThrowTo *   exception was not raised; the source TSO
                      should now put itself in the state 
                      BlockedOnMsgThrowTo, and when it is ready
                      it should unlock the mssage using
                      unlockClosure(msg, &stg_MSG_THROWTO_info);
                      If it decides not to raise the exception after
                      all, it can revoke it safely with
                      unlockClosure(msg, &stg_MSG_NULL_info);

   -------------------------------------------------------------------------- */

MessageThrowTo *
throwTo (Capability *cap,	// the Capability we hold 
	 StgTSO *source,	// the TSO sending the exception (or NULL)
	 StgTSO *target,        // the TSO receiving the exception
	 StgClosure *exception) // the exception closure
{
    MessageThrowTo *msg;

    msg = (MessageThrowTo *) allocate(cap, sizeofW(MessageThrowTo));
    // the message starts locked; see below
    SET_HDR(msg, &stg_WHITEHOLE_info, CCS_SYSTEM);
    msg->source      = source;
    msg->target      = target;
    msg->exception   = exception;

    switch (throwToMsg(cap, msg))
    {
    case THROWTO_SUCCESS:
        // unlock the message now, otherwise we leave a WHITEHOLE in
        // the heap (#6103)
        SET_HDR(msg, &stg_MSG_THROWTO_info, CCS_SYSTEM);
        return NULL;

    case THROWTO_BLOCKED:
    default:
        // the caller will unlock the message when it is ready.  We
        // cannot unlock it yet, because the calling thread will need
        // to tidy up its state first.
        return msg;
    }
}
    

nat
throwToMsg (Capability *cap, MessageThrowTo *msg)
{
    StgWord status;
    StgTSO *target = msg->target;
    Capability *target_cap;

    goto check_target;

retry:
    write_barrier();
    debugTrace(DEBUG_sched, "throwTo: retrying...");

check_target:
    ASSERT(target != END_TSO_QUEUE);

    // Thread already dead?
    if (target->what_next == ThreadComplete 
	|| target->what_next == ThreadKilled) {
	return THROWTO_SUCCESS;
    }

    debugTraceCap(DEBUG_sched, cap,
                  "throwTo: from thread %lu to thread %lu",
                  (unsigned long)msg->source->id, 
                  (unsigned long)msg->target->id);

#ifdef DEBUG
    traceThreadStatus(DEBUG_sched, target);
#endif

    target_cap = target->cap;
    if (target->cap != cap) {
        throwToSendMsg(cap, target_cap, msg);
        return THROWTO_BLOCKED;
    }

    status = target->why_blocked;
    
    switch (status) {
    case NotBlocked:
    {
        if ((target->flags & TSO_BLOCKEX) == 0) {
            // It's on our run queue and not blocking exceptions
            raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
            return THROWTO_SUCCESS;
        } else {
            blockedThrowTo(cap,target,msg);
            return THROWTO_BLOCKED;
        }
    }

    case BlockedOnMsgThrowTo:
    {
        const StgInfoTable *i;
        MessageThrowTo *m;

        m = target->block_info.throwto;

        // target is local to this cap, but has sent a throwto
        // message to another cap.
        //
        // The source message is locked.  We need to revoke the
        // target's message so that we can raise the exception, so
        // we attempt to lock it.

        // There's a possibility of a deadlock if two threads are both
        // trying to throwTo each other (or more generally, a cycle of
        // threads).  To break the symmetry we compare the addresses
        // of the MessageThrowTo objects, and the one for which m <
        // msg gets to spin, while the other can only try to lock
        // once, but must then back off and unlock both before trying
        // again.
        if (m < msg) {
            i = lockClosure((StgClosure *)m);
        } else {
            i = tryLockClosure((StgClosure *)m);
            if (i == NULL) {
//            debugBelch("collision\n");
                throwToSendMsg(cap, target->cap, msg);
                return THROWTO_BLOCKED;
            }
        }

        if (i == &stg_MSG_NULL_info) {
            // we know there's a MSG_TRY_WAKEUP on the way, so we
            // might as well just do it now.  The message will
            // be a no-op when it arrives.
            unlockClosure((StgClosure*)m, i);
            tryWakeupThread(cap, target);
            goto retry;
        }

        if (i != &stg_MSG_THROWTO_info) {
            // if it's a MSG_NULL, this TSO has been woken up by another Cap
            unlockClosure((StgClosure*)m, i);
            goto retry;
        }

	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
            unlockClosure((StgClosure*)m, i);
            blockedThrowTo(cap,target,msg);
            return THROWTO_BLOCKED;
        }

        // nobody else can wake up this TSO after we claim the message
        doneWithMsgThrowTo(m);

        raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
        return THROWTO_SUCCESS;
    }

    case BlockedOnMVar:
    case BlockedOnMVarRead:
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
        switch (get_itbl((StgClosure *)mvar)->type) {
        case MVAR_CLEAN:
        case MVAR_DIRTY:
            break;
        default:
            goto retry;
        }

	info = lockClosure((StgClosure *)mvar);

        // we have the MVar, let's check whether the thread
	// is still blocked on the same MVar.
	if ((target->why_blocked != BlockedOnMVar && target->why_blocked != BlockedOnMVarRead)
	    || (StgMVar *)target->block_info.closure != mvar) {
	    unlockClosure((StgClosure *)mvar, info);
	    goto retry;
	}

        if (target->_link == END_TSO_QUEUE) {
            // the MVar operation has already completed.  There is a
            // MSG_TRY_WAKEUP on the way, but we can just wake up the
            // thread now anyway and ignore the message when it
            // arrives.
	    unlockClosure((StgClosure *)mvar, info);
            tryWakeupThread(cap, target);
            goto retry;
        }

	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
            blockedThrowTo(cap,target,msg);
	    unlockClosure((StgClosure *)mvar, info);
	    return THROWTO_BLOCKED;
	} else {
            // revoke the MVar operation
            removeFromMVarBlockedQueue(target);
	    raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
	    unlockClosure((StgClosure *)mvar, info);
	    return THROWTO_SUCCESS;
	}
    }

    case BlockedOnBlackHole:
    {
	if (target->flags & TSO_BLOCKEX) {
            // BlockedOnBlackHole is not interruptible.
            blockedThrowTo(cap,target,msg);
	    return THROWTO_BLOCKED;
	} else {
            // Revoke the message by replacing it with IND. We're not
            // locking anything here, so we might still get a TRY_WAKEUP
            // message from the owner of the blackhole some time in the
            // future, but that doesn't matter.
            ASSERT(target->block_info.bh->header.info == &stg_MSG_BLACKHOLE_info);
            OVERWRITE_INFO(target->block_info.bh, &stg_IND_info);
            raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
            return THROWTO_SUCCESS;
        }
    }

    case BlockedOnSTM:
	lockTSO(target);
	// Unblocking BlockedOnSTM threads requires the TSO to be
	// locked; see STM.c:unpark_tso().
	if (target->why_blocked != BlockedOnSTM) {
	    unlockTSO(target);
	    goto retry;
	}
	if ((target->flags & TSO_BLOCKEX) &&
	    ((target->flags & TSO_INTERRUPTIBLE) == 0)) {
            blockedThrowTo(cap,target,msg);
	    unlockTSO(target);
	    return THROWTO_BLOCKED;
	} else {
	    raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
	    unlockTSO(target);
	    return THROWTO_SUCCESS;
	}

    case BlockedOnCCall_Interruptible:
#ifdef THREADED_RTS
    {
        Task *task = NULL;
        // walk suspended_ccalls to find the correct worker thread
        InCall *incall;
        for (incall = cap->suspended_ccalls; incall != NULL; incall = incall->next) {
            if (incall->suspended_tso == target) {
                task = incall->task;
                break;
            }
        }
        if (task != NULL) {
            blockedThrowTo(cap, target, msg);
            if (!((target->flags & TSO_BLOCKEX) &&
                  ((target->flags & TSO_INTERRUPTIBLE) == 0))) {
                interruptWorkerTask(task);
            }
            return THROWTO_BLOCKED;
        } else {
            debugTraceCap(DEBUG_sched, cap, "throwTo: could not find worker thread to kill");
        }
        // fall to next
    }
#endif
    case BlockedOnCCall:
	blockedThrowTo(cap,target,msg);
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
	    blockedThrowTo(cap,target,msg);
	    return THROWTO_BLOCKED;
	} else {
	    removeFromQueues(cap,target);
	    raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
	    return THROWTO_SUCCESS;
	}
#endif

    case ThreadMigrating:
        // if is is ThreadMigrating and tso->cap is ours, then it
        // *must* be migrating *to* this capability.  If it were
        // migrating away from the capability, then tso->cap would
        // point to the destination.
        //
        // There is a MSG_WAKEUP in the message queue for this thread,
        // but we can just do it preemptively:
        tryWakeupThread(cap, target);
        // and now retry, the thread should be runnable.
        goto retry;

    default:
        barf("throwTo: unrecognised why_blocked (%d)", target->why_blocked);
    }
    barf("throwTo");
}

static void
throwToSendMsg (Capability *cap STG_UNUSED,
                Capability *target_cap USED_IF_THREADS, 
                MessageThrowTo *msg USED_IF_THREADS)
            
{
#ifdef THREADED_RTS
    debugTraceCap(DEBUG_sched, cap, "throwTo: sending a throwto message to cap %lu", (unsigned long)target_cap->no);

    sendMessage(cap, target_cap, (Message*)msg);
#endif
}

// Block a throwTo message on the target TSO's blocked_exceptions
// queue.  The current Capability must own the target TSO in order to
// modify the blocked_exceptions queue.
void
blockedThrowTo (Capability *cap, StgTSO *target, MessageThrowTo *msg)
{
    debugTraceCap(DEBUG_sched, cap, "throwTo: blocking on thread %lu",
                  (unsigned long)target->id);

    ASSERT(target->cap == cap);

    msg->link = target->blocked_exceptions;
    target->blocked_exceptions = msg;
    dirty_TSO(cap,target); // we modified the blocked_exceptions queue
}

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
    MessageThrowTo *msg;
    const StgInfoTable *i;
    StgTSO *source;

    if (tso->what_next == ThreadComplete || tso->what_next == ThreadFinished) {
        if (tso->blocked_exceptions != END_BLOCKED_EXCEPTIONS_QUEUE) {
            awakenBlockedExceptionQueue(cap,tso);
            return 1;
        } else {
            return 0;
        }
    }

    if (tso->blocked_exceptions != END_BLOCKED_EXCEPTIONS_QUEUE && 
        (tso->flags & TSO_BLOCKEX) != 0) {
        debugTraceCap(DEBUG_sched, cap, "throwTo: thread %lu has blocked exceptions but is inside block", (unsigned long)tso->id);
    }

    if (tso->blocked_exceptions != END_BLOCKED_EXCEPTIONS_QUEUE
	&& ((tso->flags & TSO_BLOCKEX) == 0
	    || ((tso->flags & TSO_INTERRUPTIBLE) && interruptible(tso)))) {

	// We unblock just the first thread on the queue, and perform
	// its throw immediately.
    loop:
        msg = tso->blocked_exceptions;
        if (msg == END_BLOCKED_EXCEPTIONS_QUEUE) return 0;
        i = lockClosure((StgClosure*)msg);
        tso->blocked_exceptions = (MessageThrowTo*)msg->link;
        if (i == &stg_MSG_NULL_info) {
            unlockClosure((StgClosure*)msg,i);
            goto loop;
        }

        throwToSingleThreaded(cap, msg->target, msg->exception);
        source = msg->source;
        doneWithMsgThrowTo(msg);
        tryWakeupThread(cap, source);
        return 1;
    }
    return 0;
}

// awakenBlockedExceptionQueue(): Just wake up the whole queue of
// blocked exceptions.

void
awakenBlockedExceptionQueue (Capability *cap, StgTSO *tso)
{
    MessageThrowTo *msg;
    const StgInfoTable *i;
    StgTSO *source;

    for (msg = tso->blocked_exceptions; msg != END_BLOCKED_EXCEPTIONS_QUEUE;
         msg = (MessageThrowTo*)msg->link) {
        i = lockClosure((StgClosure *)msg);
        if (i != &stg_MSG_NULL_info) {
            source = msg->source;
            doneWithMsgThrowTo(msg);
            tryWakeupThread(cap, source);
        } else {
            unlockClosure((StgClosure *)msg,i);
        }
    }
    tso->blocked_exceptions = END_BLOCKED_EXCEPTIONS_QUEUE;
}    

/* -----------------------------------------------------------------------------
   Remove a thread from blocking queues.

   This is for use when we raise an exception in another thread, which
   may be blocked.

   Precondition: we have exclusive access to the TSO, via the same set
   of conditions as throwToSingleThreaded() (c.f.).
   -------------------------------------------------------------------------- */

static void
removeFromMVarBlockedQueue (StgTSO *tso)
{
    StgMVar *mvar = (StgMVar*)tso->block_info.closure;
    StgMVarTSOQueue *q = (StgMVarTSOQueue*)tso->_link;

    if (q == (StgMVarTSOQueue*)END_TSO_QUEUE) {
        // already removed from this MVar
        return;
    }

    // Assume the MVar is locked. (not assertable; sometimes it isn't
    // actually WHITEHOLE'd).

    // We want to remove the MVAR_TSO_QUEUE object from the queue.  It
    // isn't doubly-linked so we can't actually remove it; instead we
    // just overwrite it with an IND if possible and let the GC short
    // it out.  However, we have to be careful to maintain the deque
    // structure:

    if (mvar->head == q) {
        mvar->head = q->link;
        OVERWRITE_INFO(q, &stg_IND_info);
        if (mvar->tail == q) {
            mvar->tail = (StgMVarTSOQueue*)END_TSO_QUEUE;
        }
    }
    else if (mvar->tail == q) {
        // we can't replace it with an IND in this case, because then
        // we lose the tail pointer when the GC shorts out the IND.
        // So we use MSG_NULL as a kind of non-dupable indirection;
        // these are ignored by takeMVar/putMVar.
        OVERWRITE_INFO(q, &stg_MSG_NULL_info);
    }
    else {
        OVERWRITE_INFO(q, &stg_IND_info);
    }

    // revoke the MVar operation
    tso->_link = END_TSO_QUEUE;
}

static void
removeFromQueues(Capability *cap, StgTSO *tso)
{
  switch (tso->why_blocked) {

  case NotBlocked:
  case ThreadMigrating:
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
  case BlockedOnMVarRead:
      removeFromMVarBlockedQueue(tso);
      goto done;

  case BlockedOnBlackHole:
      // nothing to do
      goto done;

  case BlockedOnMsgThrowTo:
  {
      MessageThrowTo *m = tso->block_info.throwto;
      // The message is locked by us, unless we got here via
      // deleteAllThreads(), in which case we own all the
      // capabilities.
      // ASSERT(m->header.info == &stg_WHITEHOLE_info);

      // unlock and revoke it at the same time
      doneWithMsgThrowTo(m);
      break;
  }

#if !defined(THREADED_RTS)
  case BlockedOnRead:
  case BlockedOnWrite:
#if defined(mingw32_HOST_OS)
  case BlockedOnDoProc:
#endif
      removeThreadFromDeQueue(cap, &blocked_queue_hd, &blocked_queue_tl, tso);
#if defined(mingw32_HOST_OS)
      /* (Cooperatively) signal that the worker thread should abort
       * the request.
       */
      abandonWorkRequest(tso->block_info.async_result->reqID);
#endif
      goto done;

  case BlockedOnDelay:
        removeThreadFromQueue(cap, &sleeping_queue, tso);
	goto done;
#endif

  default:
      barf("removeFromQueues: %d", tso->why_blocked);
  }

 done:
  tso->why_blocked = NotBlocked;
  appendToRunQueue(cap, tso);
}

/* -----------------------------------------------------------------------------
 * raiseAsync()
 *
 * The following function implements the magic for raising an
 * asynchronous exception in an existing thread.
 *
 * We first remove the thread from any queue on which it might be
 * blocked.  The possible blockages are MVARs, BLOCKING_QUEUESs, and
 * TSO blocked_exception queues.
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

static StgTSO *
raiseAsync(Capability *cap, StgTSO *tso, StgClosure *exception, 
	   rtsBool stop_at_atomically, StgUpdateFrame *stop_here)
{
    StgRetInfoTable *info;
    StgPtr sp, frame;
    StgClosure *updatee;
    nat i;
    StgStack *stack;

    debugTraceCap(DEBUG_sched, cap,
                  "raising exception in thread %ld.", (long)tso->id);
    
#if defined(PROFILING)
    /* 
     * Debugging tool: on raising an  exception, show where we are.
     * See also Exception.cmm:stg_raisezh.
     * This wasn't done for asynchronous exceptions originally; see #1450 
     */
    if (RtsFlags.ProfFlags.showCCSOnException && exception != NULL)
    {
        fprintCCS_stderr(tso->prof.cccs,exception,tso);
    }
#endif
    // ASSUMES: the thread is not already complete or dead
    // Upper layers should deal with that.
    ASSERT(tso->what_next != ThreadComplete && 
           tso->what_next != ThreadKilled);

    // only if we own this TSO (except that deleteThread() calls this 
    ASSERT(tso->cap == cap);

    stack = tso->stackobj;

    // mark it dirty; we're about to change its stack.
    dirty_TSO(cap, tso);
    dirty_STACK(cap, stack);

    sp = stack->sp;
    
    if (stop_here != NULL) {
        updatee = stop_here->updatee;
    } else {
        updatee = NULL;
    }

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
    while (stop_here == NULL || frame < (StgPtr)stop_here) {

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
        // 6. If it's an UNDERFLOW_FRAME, then continue with the next
        //    stack chunk.
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
	    ap = (StgAP_STACK *)allocate(cap,AP_STACK_sizeW(words));
	    
	    ap->size = words;
	    ap->fun  = (StgClosure *)sp[0];
	    sp++;
	    for(i=0; i < (nat)words; ++i) {
		ap->payload[i] = (StgClosure *)*sp++;
	    }
	    
	    SET_HDR(ap,&stg_AP_STACK_info,
		    ((StgClosure *)frame)->header.prof.ccs /* ToDo */); 
	    TICK_ALLOC_UP_THK(WDS(words+1),0);
	    
	    //IF_DEBUG(scheduler,
	    //	     debugBelch("sched: Updating ");
	    //	     printPtr((P_)((StgUpdateFrame *)frame)->updatee); 
	    //	     debugBelch(" with ");
	    //	     printObj((StgClosure *)ap);
	    //	);

            if (((StgUpdateFrame *)frame)->updatee == updatee) {
                // If this update frame points to the same closure as
                // the update frame further down the stack
                // (stop_here), then don't perform the update.  We
                // want to keep the blackhole in this case, so we can
                // detect and report the loop (#2783).
                ap = (StgAP_STACK*)updatee;
            } else {
                // Perform the update
                // TODO: this may waste some work, if the thunk has
                // already been updated by another thread.
                updateThunk(cap, tso, 
                            ((StgUpdateFrame *)frame)->updatee, (StgClosure *)ap);
            }

	    sp += sizeofW(StgUpdateFrame) - 1;
	    sp[0] = (W_)ap; // push onto stack
	    frame = sp + 1;
	    continue; //no need to bump frame
	}

        case UNDERFLOW_FRAME:
        {
	    StgAP_STACK * ap;
	    nat words;
	    
	    // First build an AP_STACK consisting of the stack chunk above the
	    // current update frame, with the top word on the stack as the
	    // fun field.
	    //
	    words = frame - sp - 1;
	    ap = (StgAP_STACK *)allocate(cap,AP_STACK_sizeW(words));
	    
	    ap->size = words;
	    ap->fun  = (StgClosure *)sp[0];
	    sp++;
	    for(i=0; i < (nat)words; ++i) {
		ap->payload[i] = (StgClosure *)*sp++;
	    }
	    
            SET_HDR(ap,&stg_AP_STACK_NOUPD_info,
		    ((StgClosure *)frame)->header.prof.ccs /* ToDo */); 
            TICK_ALLOC_SE_THK(WDS(words+1),0);

            stack->sp = sp;
            threadStackUnderflow(cap,tso);
            stack = tso->stackobj;
            sp = stack->sp;

            sp--;
            sp[0] = (W_)ap;
            frame = sp + 1;
            continue;
        }

        case STOP_FRAME:
	{
	    // We've stripped the entire stack, the thread is now dead.
	    tso->what_next = ThreadKilled;
            stack->sp = frame + sizeofW(StgStopFrame);
            goto done;
	}

	case CATCH_FRAME:
	    // If we find a CATCH_FRAME, and we've got an exception to raise,
	    // then build the THUNK raise(exception), and leave it on
	    // top of the CATCH_FRAME ready to enter.
	    //
	{
	    StgCatchFrame *cf = (StgCatchFrame *)frame;
	    StgThunk *raise;
	    
	    if (exception == NULL) break;

	    // we've got an exception to raise, so let's pass it to the
	    // handler in this frame.
	    //
	    raise = (StgThunk *)allocate(cap,sizeofW(StgThunk)+1);
	    TICK_ALLOC_SE_THK(WDS(1),0);
	    SET_HDR(raise,&stg_raise_info,cf->header.prof.ccs);
	    raise->payload[0] = exception;
	    
	    // throw away the stack from Sp up to the CATCH_FRAME.
	    //
	    sp = frame - 1;
	    
	    /* Ensure that async exceptions are blocked now, so we don't get
	     * a surprise exception before we get around to executing the
	     * handler.
	     */
            tso->flags |= TSO_BLOCKEX;
            if ((cf->exceptions_blocked & TSO_INTERRUPTIBLE) == 0) {
                tso->flags &= ~TSO_INTERRUPTIBLE;
            } else {
                tso->flags |= TSO_INTERRUPTIBLE;
            }

	    /* Put the newly-built THUNK on top of the stack, ready to execute
	     * when the thread restarts.
	     */
	    sp[0] = (W_)raise;
	    sp[-1] = (W_)&stg_enter_info;
            stack->sp = sp-1;
	    tso->what_next = ThreadRunGHC;
            goto done;
	}
	    
	case ATOMICALLY_FRAME:
	    if (stop_at_atomically) {
		ASSERT(tso->trec->enclosing_trec == NO_TREC);
		stmCondemnTransaction(cap, tso -> trec);
                stack->sp = frame - 2;
                // The ATOMICALLY_FRAME expects to be returned a
                // result from the transaction, which it stores in the
                // stack frame.  Hence we arrange to return a dummy
                // result, so that the GC doesn't get upset (#3578).
                // Perhaps a better way would be to have a different
                // ATOMICALLY_FRAME instance for condemned
                // transactions, but I don't fully understand the
                // interaction with STM invariants.
                stack->sp[1] = (W_)&stg_NO_TREC_closure;
                stack->sp[0] = (W_)&stg_ret_p_info;
                tso->what_next = ThreadRunGHC;
                goto done;
	    }
            else
            {
                // Freezing an STM transaction.  Just aborting the
                // transaction would be wrong; this is what we used to
                // do, and it goes wrong if the ATOMICALLY_FRAME ever
                // gets back onto the stack again, which it will do if
                // the transaction is inside unsafePerformIO or
                // unsafeInterleaveIO and hence inside an UPDATE_FRAME.
                //
                // So we want to make it so that if the enclosing
                // computation is resumed, we will re-execute the
                // transaction.  We therefore:
                //
                //   1. abort the current transaction
                //   3. replace the stack up to and including the
                //      atomically frame with a closure representing
                //      a call to "atomically x", where x is the code
                //      of the transaction.
                //   4. continue stripping the stack
                //
                StgTRecHeader *trec = tso->trec;
                StgTRecHeader *outer = trec->enclosing_trec;

                StgThunk *atomically;
                StgAtomicallyFrame *af = (StgAtomicallyFrame*)frame;

                debugTraceCap(DEBUG_stm, cap,
                              "raiseAsync: freezing atomically frame")
                stmAbortTransaction(cap, trec);
                stmFreeAbortedTRec(cap, trec);
                tso->trec = outer;

                atomically = (StgThunk*)allocate(cap,sizeofW(StgThunk)+1);
                TICK_ALLOC_SE_THK(1,0);
                SET_HDR(atomically,&stg_atomically_info,af->header.prof.ccs);
                atomically->payload[0] = af->code;

                // discard stack up to and including the ATOMICALLY_FRAME
                frame += sizeofW(StgAtomicallyFrame);
                sp = frame - 1;

                // replace the ATOMICALLY_FRAME with call to atomically#
                sp[0] = (W_)atomically;
                continue;
            }

        case CATCH_STM_FRAME:
	case CATCH_RETRY_FRAME:
            // CATCH frames within an atomically block: abort the
            // inner transaction and continue.  Eventually we will
            // hit the outer transaction that will get frozen (see
            // above).
            //
            // In this case (unlike ordinary exceptions) we do not care
	    // whether the transaction is valid or not because its
	    // possible validity cannot have caused the exception
	    // and will not be visible after the abort.
        {
            StgTRecHeader *trec = tso -> trec;
            StgTRecHeader *outer = trec -> enclosing_trec;
	    debugTraceCap(DEBUG_stm, cap,
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

done:
    IF_DEBUG(sanity, checkTSO(tso));

    // wake it up
    if (tso->why_blocked != NotBlocked) {
        tso->why_blocked = NotBlocked;
        appendToRunQueue(cap,tso);
    }        

    return tso;
}


