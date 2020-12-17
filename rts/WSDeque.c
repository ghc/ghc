/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * Work-stealing Deque data structure
 *
 * The implementation uses Double-Ended Queues with lock-free access
 * (thereby often called "deque") as described in
 *
 * D.Chase and Y.Lev, Dynamic Circular Work-Stealing Deque.
 * SPAA'05, July 2005, Las Vegas, USA.
 * ACM 1-58113-986-1/05/0007
 *
 * This implementation closely follows the C11 implementation presented in
 *
 * N.M. Lê, A. Pop, A.Cohen, and F.Z. Nardelli. "Correct and Efficient
 * Work-Stealing for Weak Memory Models". PPoPP'13, February 2013,
 * ACM 978-1-4503-1922/13/02.
 *
 * Author: Jost Berthold MSRC 07-09/2008
 * Rewritten by: Ben Gamari (Well-Typed)
 *
 *
 * The DeQue is held as a circular array with known length. Positions
 * of top (read-end) and bottom (write-end) always increase, and the
 * array is accessed with indices modulo array-size. While this bears
 * the risk of overflow, we assume that (with 64 bit indices), a
 * program must run very long to reach that point.
 *
 * The write end of the queue (position bottom) can only be used with
 * mutual exclusion, i.e. by exactly one caller at a time.  At this
 * end, new items can be enqueued using pushBottom()/newSpark(), and
 * removed using popBottom()/reclaimSpark() (the latter implying a cas
 * synchronisation with potential concurrent readers for the case of
 * just one element).
 *
 * Multiple readers can steal from the read end (position top), and
 * are synchronised without a lock, based on a cas of the top
 * position. One reader wins, the others return NULL for a failure.
 *
 * Both popWSDeque and stealWSDeque also return NULL when the queue is empty.
 *
 * Testing: see testsuite/tests/rts/testwsdeque.c.  If
 * there's anything wrong with the deque implementation, this test
 * will probably catch it.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "WSDeque.h"

// Returns true on success.
static inline bool
cas_top(WSDeque *q, StgInt old, StgInt new)
{
    return (StgWord) old == SEQ_CST_RELAXED_CAS((StgPtr) &q->top,
                                                (StgWord) old, (StgWord) new);
}


/* -----------------------------------------------------------------------------
 * newWSDeque
 * -------------------------------------------------------------------------- */

/* internal helpers ... */

static StgWord
roundUp2(StgWord val)
{
    StgWord rounded = 1;

    /* StgWord is unsigned anyway, only catch 0 */
    if (val == 0) {
        barf("DeQue,roundUp2: invalid size 0 requested");
    }
    /* at least 1 bit set, shift up to its place */
    do {
        rounded = rounded << 1;
    } while (0 != (val = val>>1));
    return rounded;
}

WSDeque *
newWSDeque (uint32_t size)
{
    StgWord realsize;
    WSDeque *q;

    realsize = roundUp2(size); /* to compute modulo as a bitwise & */

    q = (WSDeque*) stgMallocBytes(sizeof(WSDeque),   /* admin fields */
                                  "newWSDeque");
    q->elements = stgMallocBytes(realsize * sizeof(StgClosurePtr), /* dataspace */
                                 "newWSDeque:data space");
    q->size = realsize;  /* power of 2 */
    q->moduloSize = realsize - 1; /* n % size == n & moduloSize  */

    q->top=0;
    RELEASE_STORE(&q->bottom, 0); /* read by writer, updated each time top is read */

    ASSERT_WSDEQUE_INVARIANTS(q);
    return q;
}

/* -----------------------------------------------------------------------------
 * freeWSDeque
 * -------------------------------------------------------------------------- */

void
freeWSDeque (WSDeque *q)
{
    stgFree(q->elements);
    stgFree(q);
}

/* -----------------------------------------------------------------------------
 *
 * popWSDeque: remove an element from the write end of the queue.
 * Returns the removed spark, and NULL if a race is lost or the pool
 * empty.
 *
 * If only one spark is left in the pool, we synchronise with
 * concurrently stealing threads by using cas to modify the top field.
 * This routine should NEVER be called by a task which does not own
 * this deque.
 *
 * -------------------------------------------------------------------------- */

void *
popWSDeque (WSDeque *q)
{
    StgInt b = RELAXED_LOAD(&q->bottom) - 1;
    RELAXED_STORE(&q->bottom, b);
    SEQ_CST_FENCE();
    StgInt t = RELAXED_LOAD(&q->top);

    void *result;
    if (t <= b) {
        /* Non-empty */
        result = RELAXED_LOAD(&q->elements[b & q->moduloSize]);
        if (t == b) {
            /* Single last element in queue */
            if (!cas_top(q, t, t+1)) {
                /* Failed race */
                result = NULL;
            }

            RELAXED_STORE(&q->bottom, b+1);
        }
    } else {
        /* Empty queue */
        result = NULL;
        RELAXED_STORE(&q->bottom, b+1);
    }

    return result;
}

/* -----------------------------------------------------------------------------
 * stealWSDeque
 * -------------------------------------------------------------------------- */

void *
stealWSDeque_ (WSDeque *q)
{
    StgInt t = ACQUIRE_LOAD(&q->top);
    SEQ_CST_FENCE();
    StgInt b = ACQUIRE_LOAD(&q->bottom);

    void *result = NULL;
    if (t < b) {
        /* Non-empty queue */
        result = RELAXED_LOAD(&q->elements[t % q->size]);
        if (!cas_top(q, t, t+1)) {
            return NULL;
        }
    }
    return result;
}

void *
stealWSDeque (WSDeque *q)
{
    void *stolen;

    do {
        stolen = stealWSDeque_(q);
    } while (stolen == NULL && !looksEmptyWSDeque(q));

    return stolen;
}

/* -----------------------------------------------------------------------------
 * pushWSQueue
 * -------------------------------------------------------------------------- */

/* Enqueue an element. Must only be called by owner. Returns true if element was
 * pushed, false if queue is full
 */
bool
pushWSDeque (WSDeque* q, void * elem)
{
    StgInt b = ACQUIRE_LOAD(&q->bottom);
    StgInt t = ACQUIRE_LOAD(&q->top);

    if ( b - t > q->size - 1 ) {
        /* Full queue */
        /* We don't implement resizing, just say we didn't push anything. */
        return false;
    }

    RELAXED_STORE(&q->elements[b & q->moduloSize], elem);
#if defined(TSAN_ENABLED)
    // ThreadSanizer doesn't know about release fences, so we need to
    // strengthen this to a release store lest we get spurious data race
    // reports.
    RELEASE_STORE(&q->bottom, b+1);
#else
    RELEASE_FENCE();
    RELAXED_STORE(&q->bottom, b+1);
#endif
    return true;
}
