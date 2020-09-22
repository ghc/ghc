/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2001
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "HsFFI.h"

#include "RtsUtils.h"
#include "Prelude.h"
#include "Schedule.h"
#include "Capability.h"
#include "StablePtr.h"
#include "Threads.h"
#include "Weak.h"
#include "StableName.h"

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.

   TODO: Currently this code does not tag created pointers,
         however it is not unsafe (the constructor code will do it)
         just inefficient.
   ------------------------------------------------------------------------- */
HaskellObj
rts_mkChar (Capability *cap, HsChar c)
{
  StgClosure *p = (StgClosure *)allocate(cap, CONSTR_sizeW(0,1));
  SET_HDR(p, Czh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(StgChar)c;
  return p;
}

HaskellObj
rts_mkInt (Capability *cap, HsInt i)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, Izh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt8 (Capability *cap, HsInt8 i)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, I8zh_con_info, CCS_SYSTEM);
  /* Make sure we mask out the bits above the lowest 8 */
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt16 (Capability *cap, HsInt16 i)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, I16zh_con_info, CCS_SYSTEM);
  /* Make sure we mask out the relevant bits */
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt32 (Capability *cap, HsInt32 i)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, I32zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt64 (Capability *cap, HsInt64 i)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,2));
  SET_HDR(p, I64zh_con_info, CCS_SYSTEM);
  ASSIGN_Int64((P_)&(p->payload[0]), i);
  return p;
}

HaskellObj
rts_mkWord (Capability *cap, HsWord i)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, Wzh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)i;
  return p;
}

HaskellObj
rts_mkWord8 (Capability *cap, HsWord8 w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, W8zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xff);
  return p;
}

HaskellObj
rts_mkWord16 (Capability *cap, HsWord16 w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, W16zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xffff);
  return p;
}

HaskellObj
rts_mkWord32 (Capability *cap, HsWord32 w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, W32zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xffffffff);
  return p;
}

HaskellObj
rts_mkWord64 (Capability *cap, HsWord64 w)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,2));
  /* see mk_Int8 comment */
  SET_HDR(p, W64zh_con_info, CCS_SYSTEM);
  ASSIGN_Word64((P_)&(p->payload[0]), w);
  return p;
}


HaskellObj
rts_mkFloat (Capability *cap, HsFloat f)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,1));
  SET_HDR(p, Fzh_con_info, CCS_SYSTEM);
  ASSIGN_FLT((P_)p->payload, (StgFloat)f);
  return p;
}

HaskellObj
rts_mkDouble (Capability *cap, HsDouble d)
{
  StgClosure *p = (StgClosure *)allocate(cap,CONSTR_sizeW(0,sizeofW(StgDouble)));
  SET_HDR(p, Dzh_con_info, CCS_SYSTEM);
  ASSIGN_DBL((P_)p->payload, (StgDouble)d);
  return p;
}

HaskellObj
rts_mkStablePtr (Capability *cap, HsStablePtr s)
{
  StgClosure *p = (StgClosure *)allocate(cap,sizeofW(StgHeader)+1);
  SET_HDR(p, StablePtr_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)s;
  return p;
}

HaskellObj
rts_mkPtr (Capability *cap, HsPtr a)
{
  StgClosure *p = (StgClosure *)allocate(cap,sizeofW(StgHeader)+1);
  SET_HDR(p, Ptr_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)a;
  return p;
}

HaskellObj
rts_mkFunPtr (Capability *cap, HsFunPtr a)
{
  StgClosure *p = (StgClosure *)allocate(cap,sizeofW(StgHeader)+1);
  SET_HDR(p, FunPtr_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)a;
  return p;
}

HaskellObj
rts_mkBool (Capability *cap STG_UNUSED, HsBool b)
{
  if (b) {
    return (StgClosure *)True_closure;
  } else {
    return (StgClosure *)False_closure;
  }
}

HaskellObj
rts_mkString (Capability *cap, char *s)
{
  return rts_apply(cap, (StgClosure *)unpackCString_closure, rts_mkPtr(cap,s));
}

HaskellObj
rts_apply (Capability *cap, HaskellObj f, HaskellObj arg)
{
    StgThunk *ap;

    ap = (StgThunk *)allocate(cap,sizeofW(StgThunk) + 2);
    // Here we don't want to use CCS_SYSTEM, because it's a hidden cost centre,
    // and evaluating Haskell code under a hidden cost centre leads to
    // confusing profiling output. (#7753)
    SET_HDR(ap, (StgInfoTable *)&stg_ap_2_upd_info, CCS_MAIN);
    ap->payload[0] = f;
    ap->payload[1] = arg;
    return (StgClosure *)ap;
}

/* ----------------------------------------------------------------------------
   Deconstructing Haskell objects

   We would like to assert that we have the right kind of object in
   each case, but this is problematic because in GHCi the info table
   for the D# constructor (say) might be dynamically loaded.  Hence we
   omit these assertions for now.
   ------------------------------------------------------------------------- */

HsChar
rts_getChar (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Czh_con_info ||
    //        p->header.info == Czh_static_info);
    return (StgChar)(StgWord)(UNTAG_CLOSURE(p)->payload[0]);
}

HsInt
rts_getInt (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Izh_con_info ||
    //        p->header.info == Izh_static_info);
    return (HsInt)(UNTAG_CLOSURE(p)->payload[0]);
}

HsInt8
rts_getInt8 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I8zh_con_info ||
    //        p->header.info == I8zh_static_info);
    return (HsInt8)(HsInt)(UNTAG_CLOSURE(p)->payload[0]);
}

HsInt16
rts_getInt16 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I16zh_con_info ||
    //        p->header.info == I16zh_static_info);
    return (HsInt16)(HsInt)(UNTAG_CLOSURE(p)->payload[0]);
}

HsInt32
rts_getInt32 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I32zh_con_info ||
    //        p->header.info == I32zh_static_info);
  return (HsInt32)(HsInt)(UNTAG_CLOSURE(p)->payload[0]);
}

HsInt64
rts_getInt64 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I64zh_con_info ||
    //        p->header.info == I64zh_static_info);
    return PK_Int64((P_)&(UNTAG_CLOSURE(p)->payload[0]));
}

HsWord
rts_getWord (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Wzh_con_info ||
    //        p->header.info == Wzh_static_info);
    return (HsWord)(UNTAG_CLOSURE(p)->payload[0]);
}

HsWord8
rts_getWord8 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W8zh_con_info ||
    //        p->header.info == W8zh_static_info);
    return (HsWord8)(HsWord)(UNTAG_CLOSURE(p)->payload[0]);
}

HsWord16
rts_getWord16 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W16zh_con_info ||
    //        p->header.info == W16zh_static_info);
    return (HsWord16)(HsWord)(UNTAG_CLOSURE(p)->payload[0]);
}

HsWord32
rts_getWord32 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W32zh_con_info ||
    //        p->header.info == W32zh_static_info);
    return (HsWord32)(HsWord)(UNTAG_CLOSURE(p)->payload[0]);
}

HsWord64
rts_getWord64 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W64zh_con_info ||
    //        p->header.info == W64zh_static_info);
    return PK_Word64((P_)&(UNTAG_CLOSURE(p)->payload[0]));
}

HsFloat
rts_getFloat (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Fzh_con_info ||
    //        p->header.info == Fzh_static_info);
    return (float)(PK_FLT((P_)UNTAG_CLOSURE(p)->payload));
}

HsDouble
rts_getDouble (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Dzh_con_info ||
    //        p->header.info == Dzh_static_info);
    return (double)(PK_DBL((P_)UNTAG_CLOSURE(p)->payload));
}

HsStablePtr
rts_getStablePtr (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == StablePtr_con_info ||
    //        p->header.info == StablePtr_static_info);
    return (StgStablePtr)(UNTAG_CLOSURE(p)->payload[0]);
}

HsPtr
rts_getPtr (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Ptr_con_info ||
    //        p->header.info == Ptr_static_info);
    return (Capability *)(UNTAG_CLOSURE(p)->payload[0]);
}

HsFunPtr
rts_getFunPtr (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == FunPtr_con_info ||
    //        p->header.info == FunPtr_static_info);
    return (void *)(UNTAG_CLOSURE(p)->payload[0]);
}

HsBool
rts_getBool (HaskellObj p)
{
    const StgInfoTable *info;

    info = get_itbl((const StgClosure *)UNTAG_CONST_CLOSURE(p));
    if (info->srt == 0) { // srt is the constructor tag
        return 0;
    } else {
        return 1;
    }
}

/* -----------------------------------------------------------------------------
   Creating threads
   -------------------------------------------------------------------------- */

INLINE_HEADER void pushClosure   (StgTSO *tso, StgWord c) {
  tso->stackobj->sp--;
  tso->stackobj->sp[0] = (W_) c;
}

StgTSO *
createGenThread (Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread (cap, stack_size);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

StgTSO *
createIOThread (Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread (cap, stack_size);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

/*
 * Same as above, but also evaluate the result of the IO action
 * to whnf while we're at it.
 */

StgTSO *
createStrictIOThread(Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread(cap, stack_size);
  pushClosure(t, (W_)&stg_forceIO_info);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

/* ----------------------------------------------------------------------------
   Evaluating Haskell expressions
   ------------------------------------------------------------------------- */

void rts_eval (/* inout */ Capability **cap,
               /* in    */ HaskellObj p,
               /* out */   HaskellObj *ret)
{
    StgTSO *tso;

    tso = createGenThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

void rts_eval_ (/* inout */ Capability **cap,
                /* in    */ HaskellObj p,
                /* in    */ unsigned int stack_size,
                /* out   */ HaskellObj *ret)
{
    StgTSO *tso;

    tso = createGenThread(*cap, stack_size, p);
    scheduleWaitThread(tso,ret,cap);
}

/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
void rts_evalIO (/* inout */ Capability **cap,
                 /* in    */ HaskellObj p,
                 /* out */   HaskellObj *ret)
{
    StgTSO* tso;

    tso = createStrictIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

/*
 * rts_evalStableIOMain() is suitable for calling main Haskell thread
 * stored in (StablePtr (IO a)) it calls rts_evalStableIO but wraps
 * function in GHC.TopHandler.runMainIO that installs top_handlers.
 * See #12903.
 */
void rts_evalStableIOMain(/* inout */ Capability **cap,
                          /* in    */ HsStablePtr s,
                          /* out   */ HsStablePtr *ret)
{
    StgTSO* tso;
    StgClosure *p, *r, *w;
    SchedulerStatus stat;

    p = (StgClosure *)deRefStablePtr(s);
    w = rts_apply(*cap, &base_GHCziTopHandler_runMainIO_closure, p);
    tso = createStrictIOThread(*cap, RtsFlags.GcFlags.initialStkSize, w);
    // async exceptions are always blocked by default in the created
    // thread.  See #1048.
    tso->flags |= TSO_BLOCKEX | TSO_INTERRUPTIBLE;
    scheduleWaitThread(tso,&r,cap);
    stat = rts_getSchedStatus(*cap);

    if (stat == Success && ret != NULL) {
        ASSERT(r != NULL);
        *ret = getStablePtr((StgPtr)r);
    }
}

/*
 * rts_evalStableIO() is suitable for calling from Haskell.  It
 * evaluates a value of the form (StablePtr (IO a)), forcing the
 * action's result to WHNF before returning.  The result is returned
 * in a StablePtr.
 */
void rts_evalStableIO (/* inout */ Capability **cap,
                       /* in    */ HsStablePtr s,
                       /* out */   HsStablePtr *ret)
{
    StgTSO* tso;
    StgClosure *p, *r;
    SchedulerStatus stat;

    p = (StgClosure *)deRefStablePtr(s);
    tso = createStrictIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    // async exceptions are always blocked by default in the created
    // thread.  See #1048.
    tso->flags |= TSO_BLOCKEX | TSO_INTERRUPTIBLE;
    scheduleWaitThread(tso,&r,cap);
    stat = rts_getSchedStatus(*cap);

    if (stat == Success && ret != NULL) {
        ASSERT(r != NULL);
        *ret = getStablePtr((StgPtr)r);
    }
}

/*
 * Like rts_evalIO(), but doesn't force the action's result.
 */
void rts_evalLazyIO (/* inout */ Capability **cap,
                     /* in    */ HaskellObj p,
                     /* out */   HaskellObj *ret)
{
    StgTSO *tso;

    tso = createIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

void rts_evalLazyIO_ (/* inout */ Capability **cap,
                      /* in    */ HaskellObj p,
                      /* in    */ unsigned int stack_size,
                      /* out   */ HaskellObj *ret)
{
    StgTSO *tso;

    tso = createIOThread(*cap, stack_size, p);
    scheduleWaitThread(tso,ret,cap);
}

/* Convenience function for decoding the returned status. */

void
rts_checkSchedStatus (char* site, Capability *cap)
{
    SchedulerStatus rc = cap->running_task->incall->rstat;
    switch (rc) {
    case Success:
        return;
    case Killed:
        errorBelch("%s: uncaught exception",site);
        stg_exit(EXIT_FAILURE);
    case Interrupted:
        errorBelch("%s: interrupted", site);
#if defined(THREADED_RTS)
        // The RTS is shutting down, and the process will probably
        // soon exit.  We don't want to preempt the shutdown
        // by exiting the whole process here, so we just terminate the
        // current thread.  Don't forget to release the cap first though.
        rts_unlock(cap);
        shutdownThread();
#else
        stg_exit(EXIT_FAILURE);
#endif
    default:
        errorBelch("%s: Return code (%d) not ok",(site),(rc));
        stg_exit(EXIT_FAILURE);
    }
}

SchedulerStatus
rts_getSchedStatus (Capability *cap)
{
    return cap->running_task->incall->rstat;
}

Capability *
rts_lock (void)
{
    Capability *cap;
    Task *task;

    task = newBoundTask();

    if (task->running_finalizers) {
        errorBelch("error: a C finalizer called back into Haskell.\n"
                   "   This was previously allowed, but is disallowed in GHC 6.10.2 and later.\n"
                   "   To create finalizers that may call back into Haskell, use\n"
                   "   Foreign.Concurrent.newForeignPtr instead of Foreign.newForeignPtr.");
        stg_exit(EXIT_FAILURE);
    }

    cap = NULL;
    waitForCapability(&cap, task);

    if (task->incall->prev_stack == NULL) {
      // This is a new outermost call from C into Haskell land.
      // Until the corresponding call to rts_unlock, this task
      // is doing work on behalf of the RTS.
      traceTaskCreate(task, cap);
    }

    return (Capability *)cap;
}

// Exiting the RTS: we hold a Capability that is not necessarily the
// same one that was originally returned by rts_lock(), because
// rts_evalIO() etc. may return a new one.  Now that we have
// investigated the return value, we can release the Capability,
// and free the Task (in that order).

void
rts_unlock (Capability *cap)
{
    Task *task;

    task = cap->running_task;
    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    // Now release the Capability. With the capability released, GC
    // may happen. NB. does not try to put the current Task on the
    // worker queue.
    // NB. keep cap->lock held while we call exitMyTask(). This
    // is necessary during shutdown, where we want the invariant that
    // after shutdownCapability(), all the Tasks associated with the
    // Capability have completed their shutdown too. Otherwise we
    // could have exitMyTask()/workerTaskStop() running at some
    // random point in the future, which causes problems for
    // freeTaskManager().
    ACQUIRE_LOCK(&cap->lock);
    releaseCapability_(cap,false);

    // Finally, we can release the Task to the free list.
    exitMyTask();
    RELEASE_LOCK(&cap->lock);

    if (task->incall == NULL) {
      // This is the end of an outermost call from C into Haskell land.
      // From here on, the task goes back to C land and we should not count
      // it as doing work on behalf of the RTS.
      traceTaskDelete(task);
    }
}

#if defined(THREADED_RTS)
// The task that paused the RTS. The rts_pausing_task variable is owned by the
// task that owns all capabilities (there is at most one such task).
Task * rts_pausing_task = NULL;

// See RtsAPI.h
Capability * rts_pause (void)
{
    // Return immediately if this thread already paused the RTS. If another
    // thread has paused the RTS, then rts_pause will block until rts_resume is
    // called (and compete with other threads calling rts_pause). The blocking
    // behavior is implied by the use of `stopAllCapabilities`.
    Task * task = getMyTask();
    if (rts_pausing_task == task) return task->cap;

    // The current task must not own a capability. This is true when a new
    // thread is stareted, or when making a safe FFI call. If
    // `task->cap->running_task == task` then that is also ok because the
    // capability can be taken by other capabilities. Note that we always check
    // (rather than ASSERT which only happens with `-debug`) because this is a
    // user facing function and we want good error reporting. We also don't
    // expect rts_pause to be performance critical.
    if (task->cap && task->cap->running_task == task)
    {
        // This task owns a capability (at it can't be taken by other capabilities).
        errorBelch(task->cap->in_haskell
            ? ("error: rts_pause: attempting to pause via an unsafe FFI call.\n"
               "   Perhaps a 'foreign import unsafe' should be 'safe'?")
            : ("error: rts_pause: attempting to pause from a Task that owns a capability.\n"
               "   Have you already acquired a capability e.g. with rts_lock?"));
        stg_exit(EXIT_FAILURE);
    }

    // NOTE ghc-debug deadlock:
    //
    // stopAllCapabilities attempts to acquire all capabilities and will only
    // block if an existing thread/task:
    //
    // 1. Owns a capability and
    // 2. Is deadlocked i.e. refuses to yield/release its capability.
    //
    // Let's assume the rest of the RTS is deadlock free (tasks will eventually
    // yield their capability) outside of using the ghc-debug API:
    //
    // * rts_pause
    // * rts_resume
    // * rts_isPaused
    // * rts_listThreads
    // * rts_listMiscRoots
    //
    // Except rts_pause, none of these functions acquire a lock and so cannot
    // block. rts_pause may block on stopAllCapabilities, but we ensure that the
    // current task does not own a capability before calling
    // stopAllCapabilities. Hence, (1) does not hold given an isolated call to
    // rts_pause. The only lose end is that after rts_pause, we now have a task
    // that (by design) owns all capabilities (point (1) above) and is refusing
    // to yield them (point (2) above). Indeed, if 2 threads concurrently call
    // rts_pause, one will block until the other calls rts_resume. As "correct
    // usage" of this API requires calling rts_resume, this case is a non-issue,
    // but does imply the awkward quirk that if you call rts_pause on many
    // threads, they will all "take turns" pausing the rts, blocking until it is
    // their turn. In adition, any API function that attempts to acquire a
    // capability (e.g. rts_lock), will block until rts_resume is called. Of
    // course, all ghc-debug API functions  besides rts_pause do not attempt to
    // acquire a capability.
    //
    // The moral to this story is that you will not dealock as long as you, on
    // the same thread:
    //
    // * First call rts_pause
    // * Then avoid rts functions other than:
    //      * rts_isPaused
    //      * rts_listThreads
    //      * rts_listMiscRoots
    //      * AND dereferencing/inspect the heap directly e.g. using
    //        rts_listThreads/rts_listMiscRoots and the ghc-heap library.
    // * Finally call rts_resume
    //
    // TODO
    //
    // I think we should return Capability*. We should be able to use the rest
    // of the rts API with that token. There are a few functions that take
    // `Capability **` implying that it may change capabilities. I need to
    // confirm, but I think that in our case, we'll just end up with the same
    // capability since all others are acquired already. These other API
    // functions may change the heap, but it is up to the caller to account for
    // that. Is it possible that the API can be used to start executing a
    // haskell thread?!?!?! That's perhaps ok as long as we reacquire the
    // capability at the end so we're paused.
    task = newBoundTask(); // TODO I'm not sure why we need this. rts_lock does this.
    stopAllCapabilities(NULL, task);

    // Now we own all capabilities so we own rts_pausing_task.
    rts_pausing_task = task;

    return task->cap;
}

// See RtsAPI.h The cap argument is here just for symmetry with rts_pause and to
// match the pattern of rts_lock/rts_unlock.
void rts_resume (Capability * cap STG_UNUSED)
{
    Task * task = getMyTask(); // This thread has ownership over its Task.

    if (task != rts_pausing_task)
    {
        // We don't have a lock on rts_pausing_task but we are garanteed that
        // rts_pausing_task won't be set the current task (because the current
        // task is here now!), so the error messages are still correct.
        errorBelch (rts_isPaused()
            ? "error: rts_resume: called from a different OS thread than rts_pause."
            : "error: rts_resume: the rts is not paused. Did you forget to call rts_pause?");

        stg_exit(EXIT_FAILURE);
    }

    // Check that we own all capabilities.
    for (uint i = 0; i < n_capabilities; i++)
    {
        Capability *cap = capabilities[i];
        if (cap->running_task != task)
        {
            errorBelch ("error: rts_resume: the pausing thread does not own all capabilities."
                        "   Have you manually released a capability after calling rts_pause?");
            stg_exit(EXIT_FAILURE);
        }
    }

    // Now we own all capabilities so we own rts_pausing_task.
    rts_pausing_task = NULL;

    // releaseAllCapabilities will not block because the current task owns all
    // capabilities.
    releaseAllCapabilities(n_capabilities, NULL, task);
    exitMyTask();
}

// See RtsAPI.h
bool rts_isPaused(void)
{
    return rts_pausing_task != NULL;
}

// Check that the rts_pause was called on this thread/task. If not, outputs an
// error and exits with EXIT_FAILURE.
void assert_isPausedOnMyTask(void)
{
    if (rts_pausing_task == NULL)
    {
        errorBelch ("error: rts_listThreads: the rts is not paused. Did you forget to call rts_pause?");
        stg_exit(EXIT_FAILURE);
    }
    else if (rts_pausing_task != myTask())
    {
        errorBelch ("error: rts_listThreads: must be called from the same thread as rts_pause.");
        stg_exit(EXIT_FAILURE);
    }
}

// See RtsAPI.h
void rts_listThreads(ListThreadsCb cb, void *user)
{
    assert_isPausedOnMyTask();

    // rts_pausing_task == myTask(). This implies that the rts is paused and can
    // only be resumed by the current thread. Hence it is safe to read global
    // thread data.

    for (uint32_t g=0; g < RtsFlags.GcFlags.generations; g++) {
        StgTSO *tso = generations[g].threads;
        while (tso != END_TSO_QUEUE) {
            cb(user, tso);
            tso = tso->global_link;
        }
    }
}

struct list_roots_ctx {
    ListRootsCb cb;
    void *user;
};

// This is an evac_fn.
static void list_roots_helper(void *user, StgClosure **p) {
    struct list_roots_ctx *ctx = (struct list_roots_ctx *) user;
    ctx->cb(ctx->user, *p);
}

// See RtsAPI.h
void rts_listMiscRoots (ListRootsCb cb, void *user)
{
    assert_isPausedOnMyTask();

    struct list_roots_ctx ctx;
    ctx.cb = cb;
    ctx.user = user;

    threadStableNameTable(&list_roots_helper, (void *)&ctx);
    threadStablePtrTable(&list_roots_helper, (void *)&ctx);
}

#else
Capability * rts_pause (void)
{
    errorBelch("Warning: Pausing the RTS is only possible for "
               "multithreaded RTS.");
    stg_exit(EXIT_FAILURE);
}

void rts_resume (Capability * cap)
{
    errorBelch("Warning: Unpausing the RTS is only possible for "
               "multithreaded RTS.");
    stg_exit(EXIT_FAILURE);
}

bool rts_isPaused()
{
    errorBelch("Warning: (Un-) Pausing the RTS is only possible for "
               "multithreaded RTS.");
    return false;
}


void rts_listThreads(ListThreadsCb cb STG_UNUSED, void *user STG_UNUSED)
{
    errorBelch("Warning: Listing RTS-threads is only possible for "
               "multithreaded RTS.");
}

void rts_listMiscRoots (ListRootsCb cb STG_UNUSED, void *user STG_UNUSED)
{
    errorBelch("Warning: Listing MiscRoots is only possible for "
               "multithreaded RTS.");
}
#endif

void rts_done (void)
{
    freeMyTask();
}

/* -----------------------------------------------------------------------------
   tryPutMVar from outside Haskell

   The C call

      hs_try_putmvar(cap, mvar)

   is equivalent to the Haskell call

      tryPutMVar mvar ()

   but it is

     * non-blocking: takes a bounded, short, amount of time
     * asynchronous: the actual putMVar may be performed after the
       call returns.  That's why hs_try_putmvar() doesn't return a
       result to say whether the put succeeded.

   NOTE: this call transfers ownership of the StablePtr to the RTS, which will
   free it after the tryPutMVar has taken place.  The reason is that otherwise,
   it would be very difficult for the caller to arrange to free the StablePtr
   in all circumstances.

   For more details, see the section "Waking up Haskell threads from C" in the
   User's Guide.
   -------------------------------------------------------------------------- */

void hs_try_putmvar (/* in */ int capability,
                     /* in */ HsStablePtr mvar)
{
    Task *task = getMyTask();
    Capability *cap;
    Capability *task_old_cap USED_IF_THREADS;

    if (capability < 0) {
        capability = task->preferred_capability;
        if (capability < 0) {
            capability = 0;
        }
    }
    cap = capabilities[capability % enabled_capabilities];

#if !defined(THREADED_RTS)

    performTryPutMVar(cap, (StgMVar*)deRefStablePtr(mvar), Unit_closure);
    freeStablePtr(mvar);

#else

    ACQUIRE_LOCK(&cap->lock);
    // If the capability is free, we can perform the tryPutMVar immediately
    if (cap->running_task == NULL) {
        cap->running_task = task;
        task_old_cap = task->cap;
        task->cap = cap;
        RELEASE_LOCK(&cap->lock);

        performTryPutMVar(cap, (StgMVar*)deRefStablePtr(mvar), Unit_closure);

        freeStablePtr(mvar);

        // Wake up the capability, which will start running the thread that we
        // just awoke (if there was one).
        releaseCapability(cap);
        task->cap = task_old_cap;
    } else {
        PutMVar *p = stgMallocBytes(sizeof(PutMVar),"hs_try_putmvar");
        // We cannot deref the StablePtr if we don't have a capability,
        // so we have to store it and deref it later.
        p->mvar = mvar;
        p->link = cap->putMVars;
        cap->putMVars = p;
        RELEASE_LOCK(&cap->lock);
    }

#endif
}
