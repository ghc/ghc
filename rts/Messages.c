/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * Inter-Capability message passing
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "Messages.h"
#include "Trace.h"
#include "Capability.h"
#include "Schedule.h"
#include "Threads.h"
#include "RaiseAsync.h"
#include "sm/Storage.h"
#include "CloneStack.h"

/* ----------------------------------------------------------------------------
   Send a message to another Capability
   ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

void sendMessage(Capability *from_cap, Capability *to_cap, Message *msg)
{
    ACQUIRE_LOCK(&to_cap->lock);

#if defined(DEBUG)
    {
        const StgInfoTable *i = msg->header.info;
        if (i != &stg_MSG_THROWTO_info &&
            i != &stg_MSG_BLACKHOLE_info &&
            i != &stg_MSG_TRY_WAKEUP_info &&
            i != &stg_IND_info && // can happen if a MSG_BLACKHOLE is revoked
            i != &stg_WHITEHOLE_info &&
            i != &stg_MSG_CLONE_STACK_info) {
            barf("sendMessage: %p", i);
        }
    }
#endif

    msg->link = to_cap->inbox;
    RELAXED_STORE(&to_cap->inbox, msg);

    recordClosureMutated(from_cap,(StgClosure*)msg);

    if (to_cap->running_task == NULL) {
        to_cap->running_task = myTask();
            // precond for releaseCapability_()
        releaseCapability_(to_cap,false);
    } else {
        interruptCapability(to_cap);
    }

    RELEASE_LOCK(&to_cap->lock);
}

#endif /* THREADED_RTS */

/* ----------------------------------------------------------------------------
   Handle a message
   ------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

void
executeMessage (Capability *cap, Message *m)
{
    const StgInfoTable *i;

loop:
    i = ACQUIRE_LOAD(&m->header.info);
    if (i == &stg_MSG_TRY_WAKEUP_info)
    {
        StgTSO *tso = ((MessageWakeup *)m)->tso;
        debugTraceCap(DEBUG_sched, cap, "message: try wakeup thread %"
                      FMT_StgThreadID, tso->id);
        tryWakeupThread(cap, tso);
    }
    else if (i == &stg_MSG_THROWTO_info)
    {
        MessageThrowTo *t = (MessageThrowTo *)m;
        uint32_t r;
        const StgInfoTable *i;

        i = lockClosure((StgClosure*)m);
        if (i != &stg_MSG_THROWTO_info) {
            unlockClosure((StgClosure*)m, i);
            goto loop;
        }

        debugTraceCap(DEBUG_sched, cap, "message: throwTo %ld -> %ld",
                      (W_)t->source->id, (W_)t->target->id);

        r = throwToMsg(cap, t);

        switch (r) {
        case THROWTO_SUCCESS: {
            // this message is done
            StgTSO *source = t->source;
            doneWithMsgThrowTo(cap, t);
            tryWakeupThread(cap, source);
            break;
        }
        case THROWTO_BLOCKED:
            // unlock the message
            unlockClosure((StgClosure*)m, &stg_MSG_THROWTO_info);
            break;
        }
    }
    else if (i == &stg_MSG_BLACKHOLE_info)
    {
        uint32_t r;
        MessageBlackHole *b = (MessageBlackHole*)m;

        r = messageBlackHole(cap, b);
        if (r == 0) {
            tryWakeupThread(cap, b->tso);
        }
        return;
    }
    else if (i == &stg_IND_info || i == &stg_MSG_NULL_info)
    {
        // message was revoked
        return;
    }
    else if (i == &stg_WHITEHOLE_info)
    {
#if defined(PROF_SPIN)
        NONATOMIC_ADD(&whitehole_executeMessage_spin, 1);
#endif
        goto loop;
    }
    else if(i == &stg_MSG_CLONE_STACK_info){
        MessageCloneStack *cloneStackMessage = (MessageCloneStack*) m;
        handleCloneStackMessage(cloneStackMessage);
    }
    else
    {
        barf("executeMessage: %p", i);
    }
}

#endif

/* ----------------------------------------------------------------------------
   Handle a MSG_BLACKHOLE message

   This is called from two places: either we just entered a BLACKHOLE
   (stg_BLACKHOLE_info), or we received a MSG_BLACKHOLE in our
   cap->inbox.

   We need to establish whether the BLACKHOLE belongs to
   this Capability, and
     - if so, arrange to block the current thread on it
     - otherwise, forward the message to the right place

   Returns:
     - 0 if the blocked thread can be woken up by the caller
     - 1 if the thread is still blocked, and we promise to send a MSG_TRY_WAKEUP
       at some point in the future.

   ------------------------------------------------------------------------- */

uint32_t messageBlackHole(Capability *cap, MessageBlackHole *msg)
{
    debugTraceCap(DEBUG_sched, cap, "message: thread %" FMT_StgThreadID
                  " blocking on blackhole %p", msg->tso->id, msg->bh);

    StgClosure *bh = UNTAG_CLOSURE(msg->bh);
    const StgInfoTable *const bh_info = ACQUIRE_LOAD(&bh->header.info);

    // If we got this message in our inbox, it might be that the
    // BLACKHOLE has already been updated, and GC has shorted out the
    // indirection, so the pointer no longer points to a BLACKHOLE at
    // all.
    if (bh_info != &stg_BLACKHOLE_info &&
        bh_info != &stg_CAF_BLACKHOLE_info &&
        bh_info != &__stg_EAGER_BLACKHOLE_info &&
        bh_info != &stg_WHITEHOLE_info) {
        // if it is a WHITEHOLE, then a thread is in the process of
        // trying to BLACKHOLE it.  But we know that it was once a
        // BLACKHOLE, so there is at least a valid pointer in the
        // payload, so we can carry on.
        return 0;
    }

    // The blackhole must indirect to a TSO, a BLOCKING_QUEUE, an IND,
    // or a value.
    StgClosure *p;
    const StgInfoTable *info;
    do {
        p = UNTAG_CLOSURE(ACQUIRE_LOAD(&((StgInd*)bh)->indirectee));
        info = RELAXED_LOAD(&p->header.info);
    } while (info == &stg_IND_info);
        // We could encounter an IND, if e.g. we got a BLOCKING_QUEUE that has
        // just been replaced with an IND by another thread in
        // updateThunk().  In which case, if we read the indirectee
        // again we should get the value.
        // See Note [BLACKHOLE pointing to IND] in sm/Evac.c

    if (info == &stg_TSO_info)
    {
        StgTSO *owner = (StgTSO*)p;

#if defined(THREADED_RTS)
        if (owner->cap != cap) {
            sendMessage(cap, owner->cap, (Message*)msg);
            debugTraceCap(DEBUG_sched, cap, "forwarding message to cap %d",
                          owner->cap->no);
            return 1;
        }
#endif
        // owner is the owner of the BLACKHOLE, and resides on this
        // Capability.  msg->tso is the first thread to block on this
        // BLACKHOLE, so we first create a BLOCKING_QUEUE object.

        StgBlockingQueue *bq = (StgBlockingQueue*)allocate(cap, sizeofW(StgBlockingQueue));

        // initialise the BLOCKING_QUEUE object
        bq->bh = bh;
        bq->queue = msg;
        bq->owner = owner;

        msg->link = (MessageBlackHole*)END_TSO_QUEUE;

        // All BLOCKING_QUEUES are linked in a list on owner->bq, so
        // that we can search through them in the event that there is
        // a collision to update a BLACKHOLE and a BLOCKING_QUEUE
        // becomes orphaned (see updateThunk()).
        bq->link = owner->bq;
        SET_HDR(bq, &stg_BLOCKING_QUEUE_DIRTY_info, CCS_SYSTEM);
        // We are about to make the newly-constructed message visible to other cores;
        // a barrier is necessary to ensure that all writes are visible.
        // See Note [Heap memory barriers] in SMP.h.
        dirty_TSO(cap, owner); // we will modify owner->bq
        RELEASE_STORE(&owner->bq, bq);

        // If the owner of the blackhole is currently runnable, then
        // bump it to the front of the run queue.  This gives the
        // blocked-on thread a little boost which should help unblock
        // this thread, and may avoid a pile-up of other threads
        // becoming blocked on the same BLACKHOLE (#3838).
        //
        // NB. we check to make sure that the owner is not the same as
        // the current thread, since in that case it will not be on
        // the run queue.
        if (owner->why_blocked == NotBlocked && owner->id != msg->tso->id) {
            promoteInRunQueue(cap, owner);
        }

        // point to the BLOCKING_QUEUE from the BLACKHOLE
        // RELEASE to make the BQ visible, see Note [Heap memory barriers].
        RELEASE_STORE(&((StgInd*)bh)->indirectee, (StgClosure *)bq);
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            updateRemembSetPushClosure(cap, (StgClosure*)p);
        }
        recordClosureMutated(cap,bh); // bh was mutated

        debugTraceCap(DEBUG_sched, cap, "thread %" FMT_StgThreadID " blocked on"
                      " thread %" FMT_StgThreadID, msg->tso->id, owner->id);

        return 1; // blocked
    }
    else if (info == &stg_BLOCKING_QUEUE_CLEAN_info ||
             info == &stg_BLOCKING_QUEUE_DIRTY_info)
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        ASSERT(bq->bh == bh);

        StgTSO *owner = bq->owner;

        ASSERT(owner != END_TSO_QUEUE);

#if defined(THREADED_RTS)
        if (owner->cap != cap) {
            sendMessage(cap, owner->cap, (Message*)msg);
            debugTraceCap(DEBUG_sched, cap, "forwarding message to cap %d",
                          owner->cap->no);
            return 1;
        }
#endif

        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            // We are about to overwrite bq->queue; make sure its current value
            // makes it into the update remembered set
            updateRemembSetPushClosure(cap, (StgClosure*)bq->queue);
        }
        RELAXED_STORE(&msg->link, bq->queue);
        bq->queue = msg;
        // No barrier is necessary here: we are only exposing the
        // closure to the GC. See Note [Heap memory barriers] in SMP.h.
        recordClosureMutated(cap,(StgClosure*)msg);

        if (info == &stg_BLOCKING_QUEUE_CLEAN_info) {
            RELAXED_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_DIRTY_info);
            // No barrier is necessary here: we are only exposing the
            // closure to the GC. See Note [Heap memory barriers] in SMP.h.
            recordClosureMutated(cap,(StgClosure*)bq);
        }

        debugTraceCap(DEBUG_sched, cap,
                      "thread %" FMT_StgThreadID " blocked on existing "
                      "BLOCKING_QUEUE owned by thread %" FMT_StgThreadID,
                      msg->tso->id, owner->id);

        // See above, #3838
        if (owner->why_blocked == NotBlocked && owner->id != msg->tso->id) {
            promoteInRunQueue(cap, owner);
        }

        return 1; // blocked
    }

    return 0; // not blocked
}

// A shorter version of messageBlackHole(), that just returns the
// owner (or NULL if the owner cannot be found, because the blackhole
// has been updated in the meantime).

StgTSO * blackHoleOwner (StgClosure *bh)
{
    const StgInfoTable *info;
    StgClosure *p;

    info = RELAXED_LOAD(&bh->header.info);

    if (info != &stg_BLACKHOLE_info &&
        info != &stg_CAF_BLACKHOLE_info &&
        info != &__stg_EAGER_BLACKHOLE_info &&
        info != &stg_WHITEHOLE_info) {
        return NULL;
    }

    // The blackhole must indirect to a TSO, a BLOCKING_QUEUE, an IND,
    // or a value.
loop:
    p = UNTAG_CLOSURE(ACQUIRE_LOAD(&((StgInd*)bh)->indirectee));
    info = RELAXED_LOAD(&p->header.info);

    if (info == &stg_IND_info) goto loop;

    else if (info == &stg_TSO_info)
    {
        return (StgTSO*)p;
    }
    else if (info == &stg_BLOCKING_QUEUE_CLEAN_info ||
             info == &stg_BLOCKING_QUEUE_DIRTY_info)
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;
        return RELAXED_LOAD(&bq->owner);
    }

    return NULL; // not blocked
}
