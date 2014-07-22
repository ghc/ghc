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

/* ----------------------------------------------------------------------------
   Send a message to another Capability
   ------------------------------------------------------------------------- */

#ifdef THREADED_RTS

void sendMessage(Capability *from_cap, Capability *to_cap, Message *msg)
{
    ACQUIRE_LOCK(&to_cap->lock);

#ifdef DEBUG
    {
        const StgInfoTable *i = msg->header.info;
        if (i != &stg_MSG_THROWTO_info &&
            i != &stg_MSG_BLACKHOLE_info &&
            i != &stg_MSG_TRY_WAKEUP_info &&
            i != &stg_IND_info && // can happen if a MSG_BLACKHOLE is revoked
            i != &stg_WHITEHOLE_info) {
            barf("sendMessage: %p", i);
        }
    }
#endif

    msg->link = to_cap->inbox;
    to_cap->inbox = msg;

    recordClosureMutated(from_cap,(StgClosure*)msg);

    if (to_cap->running_task == NULL) {
        to_cap->running_task = myTask();
            // precond for releaseCapability_()
        releaseCapability_(to_cap,rtsFalse);
    } else {
        interruptCapability(to_cap);
    }

    RELEASE_LOCK(&to_cap->lock);
}

#endif /* THREADED_RTS */

/* ----------------------------------------------------------------------------
   Handle a message
   ------------------------------------------------------------------------- */

#ifdef THREADED_RTS

void
executeMessage (Capability *cap, Message *m)
{
    const StgInfoTable *i;

loop:
    write_barrier(); // allow m->header to be modified by another thread
    i = m->header.info;
    if (i == &stg_MSG_TRY_WAKEUP_info)
    {
        StgTSO *tso = ((MessageWakeup *)m)->tso;
        debugTraceCap(DEBUG_sched, cap, "message: try wakeup thread %ld",
                      (W_)tso->id);
        tryWakeupThread(cap, tso);
    }
    else if (i == &stg_MSG_THROWTO_info)
    {
        MessageThrowTo *t = (MessageThrowTo *)m;
        nat r;
        const StgInfoTable *i;

        i = lockClosure((StgClosure*)m);
        if (i != &stg_MSG_THROWTO_info) {
            unlockClosure((StgClosure*)m, i);
            goto loop;
        }

        debugTraceCap(DEBUG_sched, cap, "message: throwTo %ld -> %ld",
                      (W_)t->source->id, (W_)t->target->id);

        ASSERT(t->source->why_blocked == BlockedOnMsgThrowTo);
        ASSERT(t->source->block_info.closure == (StgClosure *)m);

        r = throwToMsg(cap, t);

        switch (r) {
        case THROWTO_SUCCESS: {
            // this message is done
            StgTSO *source = t->source;
            doneWithMsgThrowTo(t);
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
        nat r;
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
        goto loop;
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

nat messageBlackHole(Capability *cap, MessageBlackHole *msg)
{
    const StgInfoTable *info;
    StgClosure *p;
    StgBlockingQueue *bq;
    StgClosure *bh = UNTAG_CLOSURE(msg->bh);
    StgTSO *owner;

    debugTraceCap(DEBUG_sched, cap, "message: thread %d blocking on blackhole %p",
                  (W_)msg->tso->id, msg->bh);

    info = bh->header.info;

    // If we got this message in our inbox, it might be that the
    // BLACKHOLE has already been updated, and GC has shorted out the
    // indirection, so the pointer no longer points to a BLACKHOLE at
    // all.
    if (info != &stg_BLACKHOLE_info &&
        info != &stg_CAF_BLACKHOLE_info &&
        info != &__stg_EAGER_BLACKHOLE_info &&
        info != &stg_WHITEHOLE_info) {
        // if it is a WHITEHOLE, then a thread is in the process of
        // trying to BLACKHOLE it.  But we know that it was once a
        // BLACKHOLE, so there is at least a valid pointer in the
        // payload, so we can carry on.
        return 0;
    }

    // The blackhole must indirect to a TSO, a BLOCKING_QUEUE, an IND,
    // or a value.
loop:
    // NB. VOLATILE_LOAD(), because otherwise gcc hoists the load
    // and turns this into an infinite loop.
    p = UNTAG_CLOSURE((StgClosure*)VOLATILE_LOAD(&((StgInd*)bh)->indirectee));
    info = p->header.info;

    if (info == &stg_IND_info)
    {
        // This could happen, if e.g. we got a BLOCKING_QUEUE that has
        // just been replaced with an IND by another thread in
        // updateThunk().  In which case, if we read the indirectee
        // again we should get the value.
        goto loop;
    }

    else if (info == &stg_TSO_info)
    {
        owner = (StgTSO*)p;

#ifdef THREADED_RTS
        if (owner->cap != cap) {
            sendMessage(cap, owner->cap, (Message*)msg);
            debugTraceCap(DEBUG_sched, cap, "forwarding message to cap %d", owner->cap->no);
            return 1;
        }
#endif
        // owner is the owner of the BLACKHOLE, and resides on this
        // Capability.  msg->tso is the first thread to block on this
        // BLACKHOLE, so we first create a BLOCKING_QUEUE object.

        bq = (StgBlockingQueue*)allocate(cap, sizeofW(StgBlockingQueue));

        // initialise the BLOCKING_QUEUE object
        SET_HDR(bq, &stg_BLOCKING_QUEUE_DIRTY_info, CCS_SYSTEM);
        bq->bh = bh;
        bq->queue = msg;
        bq->owner = owner;

        msg->link = (MessageBlackHole*)END_TSO_QUEUE;

        // All BLOCKING_QUEUES are linked in a list on owner->bq, so
        // that we can search through them in the event that there is
        // a collision to update a BLACKHOLE and a BLOCKING_QUEUE
        // becomes orphaned (see updateThunk()).
        bq->link = owner->bq;
        owner->bq = bq;
        dirty_TSO(cap, owner); // we modified owner->bq

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
        write_barrier(); // make the BQ visible
        ((StgInd*)bh)->indirectee = (StgClosure *)bq;
        recordClosureMutated(cap,bh); // bh was mutated

        debugTraceCap(DEBUG_sched, cap, "thread %d blocked on thread %d",
                      (W_)msg->tso->id, (W_)owner->id);

        return 1; // blocked
    }
    else if (info == &stg_BLOCKING_QUEUE_CLEAN_info ||
             info == &stg_BLOCKING_QUEUE_DIRTY_info)
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        ASSERT(bq->bh == bh);

        owner = bq->owner;

        ASSERT(owner != END_TSO_QUEUE);

#ifdef THREADED_RTS
        if (owner->cap != cap) {
            sendMessage(cap, owner->cap, (Message*)msg);
            debugTraceCap(DEBUG_sched, cap, "forwarding message to cap %d", owner->cap->no);
            return 1;
        }
#endif

        msg->link = bq->queue;
        bq->queue = msg;
        recordClosureMutated(cap,(StgClosure*)msg);

        if (info == &stg_BLOCKING_QUEUE_CLEAN_info) {
            bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
            recordClosureMutated(cap,(StgClosure*)bq);
        }

        debugTraceCap(DEBUG_sched, cap, "thread %d blocked on thread %d",
                      (W_)msg->tso->id, (W_)owner->id);

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

    info = bh->header.info;

    if (info != &stg_BLACKHOLE_info &&
        info != &stg_CAF_BLACKHOLE_info &&
        info != &__stg_EAGER_BLACKHOLE_info &&
        info != &stg_WHITEHOLE_info) {
        return NULL;
    }

    // The blackhole must indirect to a TSO, a BLOCKING_QUEUE, an IND,
    // or a value.
loop:
    // NB. VOLATILE_LOAD(), because otherwise gcc hoists the load
    // and turns this into an infinite loop.
    p = UNTAG_CLOSURE((StgClosure*)VOLATILE_LOAD(&((StgInd*)bh)->indirectee));
    info = p->header.info;

    if (info == &stg_IND_info) goto loop;

    else if (info == &stg_TSO_info)
    {
        return (StgTSO*)p;
    }
    else if (info == &stg_BLOCKING_QUEUE_CLEAN_info ||
             info == &stg_BLOCKING_QUEUE_DIRTY_info)
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;
        return bq->owner;
    }

    return NULL; // not blocked
}
