/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2009
 *
 * Upcall support
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Schedule.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "Prelude.h"
#include "Threads.h"
#include "Upcalls.h"

// Initialization
UpcallQueue*
allocUpcallQueue (void)
{
  return newWSDeque (4096); //TODO KC -- Add this to RtsFlags.UpcallFlags
}

void
pushUpcallReturning (Capability* cap, Upcall uc)
{
  if (!pushWSDeque (cap->upcall_queue_returning, uc))
    barf ("pushUpcall overflow!!");
  debugTrace (DEBUG_sched, "Adding new returning upcall %p (queue size = %d)",
              (void*)uc, upcallQueueSize (cap->upcall_queue_returning));
}

void
pushUpcallNonReturning (Capability* cap, Upcall uc)
{
  if ((StgClosure*)uc == (StgClosure*)defaultUpcall_closure)
    return; //See getSwitchToNextThreadUpcall

  if (!pushWSDeque (cap->upcall_queue_non_returning, uc))
    barf ("pushUpcall overflow!!");
  debugTrace (DEBUG_sched, "Adding new non returning upcall %p (queue size = %d)",
              (void*)uc, upcallQueueSize (cap->upcall_queue_non_returning));
}

//See libraries/base/LwConc/Substrate.hs:scheduleSContActionRts
Upcall
getResumeThreadUpcall (Capability* cap, StgTSO* t)
{
  Upcall p;

  ASSERT (!t->is_upcall_thread);
  ASSERT (t->schedule_scont_action != (StgClosure*)defaultUpcall_closure);

  p = rts_apply (cap, (StgClosure*)scheduleSContActionRts_closure,
                 rts_mkSCont (cap, t));

  debugTrace (DEBUG_sched, "cap %d: getResumeThreadUpcall(%p) for thread %d",
              cap->no, (void*)p, t->id);
  return p;
}

//See libraries/base/LwConc/Substrate.hs:yieldControlAction
Upcall
getSwitchToNextThreadUpcall (Capability* cap, StgTSO* t)
{
  Upcall p;

  ASSERT (!t->is_upcall_thread);
  ASSERT (t->yield_control_action != (StgClosure*)defaultUpcall_closure);

  if (t->release_ULS) {
    p = (StgClosure*)defaultUpcall_closure;
    t->release_ULS = rtsFalse;
  }
  else {
   p = rts_apply (cap, (StgClosure*)yieldControlActionRts_closure,
                  rts_mkSCont (cap, t));
  }

  debugTrace (DEBUG_sched, "cap %d: getSwitchToNextThreadupcall(%p) for thread %d%s",
              cap->no, (void*)p, t->id, (p == (Upcall)defaultUpcall_closure)?" (DUMMY)":"");
  return p;
}

Upcall
getFinalizerUpcall (Capability* cap STG_UNUSED, StgTSO* t)
{
  ASSERT (!t->is_upcall_thread);
  StgClosure* p = t->finalizer;
  return p;
}


StgTSO*
prepareUpcallThread (Capability* cap, StgTSO* current_thread)
{
  StgTSO *upcall_thread;


  //If current thread is not an upcall thread, get the upcall thread.
  if (current_thread == (StgTSO*)END_TSO_QUEUE ||
      !isUpcallThread(current_thread)) {

    //if Upcall thread is running, create a new upcall thread
    if (cap->upcall_thread->what_next != ThreadComplete)
      initUpcallThreadOnCapability (cap);

    upcall_thread = cap->upcall_thread;
    debugTrace (DEBUG_sched, "cap %d: switching to upcall_thread %d. Saving current "
                "thread %d.", (int)cap->no, cap->upcall_thread->id,
                (current_thread == (StgTSO*)END_TSO_QUEUE)?-1:(int)current_thread->id);

    //Save current thread
    if (current_thread != (StgTSO*)END_TSO_QUEUE)
      pushOnRunQueue (cap, current_thread);
  }
  else {
    upcall_thread = current_thread;
  }

  ASSERT (isUpcallThread (upcall_thread));

  //Upcall thread is currently running
  if (upcall_thread->what_next != ThreadComplete)
    return upcall_thread;

  ASSERT (upcall_thread->what_next != ThreadKilled);
  upcall_thread->_link = (StgTSO*)END_TSO_QUEUE;
  upcall_thread->what_next = ThreadRunGHC;
  upcall_thread->why_blocked = NotBlocked;

  StgStack* stack = upcall_thread->stackobj;
  dirty_STACK (cap, stack);
  //Pop everything
  stack->sp = stack->stack + stack->stack_size;
  //Push stop frame
  stack->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure*)stack->sp,
          (StgInfoTable *)&stg_stop_thread_info,CCS_SYSTEM);

  //Finally, insert an upcall picked up from the upcall queue
  StgClosure* upcall = popUpcallQueue (cap);

  //Save the upcall in the finalzer slot of the upcall thread so that it can be
  //retrieved quickly if the upcall happens to block on a black hole.
  upcall_thread->finalizer = upcall;

  pushCallToClosure (cap, upcall_thread, upcall);

  return upcall_thread;
}

// restoreCurrentThreadIfNecessary can return END_TSO_QUEUE if there was no
// current thread when we first switched to the upcall_thread. Care must be
// taken by the caller to handle the case when returned thread is END_TSO_QUEUE.

StgTSO*
restoreCurrentThreadIfNecessary (Capability* cap, StgTSO* current_thread) {

  StgTSO* return_thread;

  //Given Thread is the upcall thread, which has finished
  if (isUpcallThread (current_thread) &&
      current_thread->what_next == ThreadComplete) {

    if (emptyRunQueue (cap))
      return_thread = (StgTSO*)END_TSO_QUEUE;
    else
      return_thread = popRunQueue (cap);

    //Save the upcall thread
    cap->upcall_thread = current_thread;

    debugTrace (DEBUG_sched, "cap %d: saving upcall thread %d and restoring original"
                " thread %d", (int)cap->no, current_thread->id,
                (return_thread == (StgTSO*)END_TSO_QUEUE)?-1:(int)return_thread->id);
  }
  else {
    return_thread = current_thread;
  }

  return return_thread;
}

/* GC for the upcall queue, called inside Capability.c for all capabilities in
 * turn. */
void
traverseUpcallQueue (evac_fn evac, void* user, Capability *cap)
{
  StgClosure **upcallp;
  UpcallQueue* queues[2];
  UpcallQueue *queue;
  StgWord top,bottom, modMask;
  nat i;

  queues[0] = cap->upcall_queue_returning;
  queues[1] = cap->upcall_queue_non_returning;

  for (i=0; i < 2; i++) {
    queue = queues[i];

    ASSERT_WSDEQUE_INVARIANTS(queue);

    top = queue->top;
    bottom = queue->bottom;
    upcallp = (StgClosurePtr*)queue->elements;
    modMask = queue->moduloSize;

    while (top < bottom) {
      /* call evac for all closures in range (wrap-around via modulo)
      * In GHC-6.10, evac takes an additional 1st argument to hold a
      * GC-specific register, see rts/sm/GC.c::mark_root()
      */
      evac( user , upcallp + (top & modMask) );
      top++;
    }

    debugTrace(DEBUG_gc,
              "traversed upcall queue, len=%ld; (hd=%ld; tl=%ld)",
              upcallQueueSize(queue), queue->bottom, queue->top);
  }
}

//upcallQueueSize == 0
rtsBool emptyUpcallQueue (Capability* cap)
{
  UpcallQueue* r = cap->upcall_queue_returning;
  UpcallQueue* nr = cap->upcall_queue_non_returning;
  return (upcallQueueSize (r) == 0 &&
          upcallQueueSize (nr) == 0);
}

StgClosure* popUpcallQueue (Capability* cap)
{
  UpcallQueue *r,*nr;
  ASSERT (!emptyUpcallQueue (cap));

  r = cap->upcall_queue_returning;
  nr = cap->upcall_queue_non_returning;

  if (upcallQueueSize (r) > 0) {
    //Assume no stealing
    return popWSDeque (r);
  }
  return popWSDeque (nr);
}
