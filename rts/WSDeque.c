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
 * Author: Jost Berthold MSRC 07-09/2008
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

#define CASTOP(addr,old,new) ((old) == cas(((StgPtr)addr),(old),(new)))

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
    q->top=0;
    q->bottom=0;
    q->topBound=0; /* read by writer, updated each time top is read */

    q->size = realsize;  /* power of 2 */
    q->moduloSize = realsize - 1; /* n % size == n & moduloSize  */

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
    /* also a bit tricky, has to avoid concurrent steal() calls by
       accessing top with cas, when there is only one element left */
    StgWord t, b;
    long  currSize;
    void * removed;

    ASSERT_WSDEQUE_INVARIANTS(q);

    b = q->bottom;

    // "decrement b as a test, see what happens"
    b--;
    q->bottom = b;

    // very important that the following read of q->top does not occur
    // before the earlier write to q->bottom.
    store_load_barrier();

    t = q->top; /* using topBound would give an *upper* bound, we
                   need a lower bound. We use the real top here, but
                   can update the topBound value */
    q->topBound = t;
    currSize = (long)b - (long)t;
    if (currSize < 0) { /* was empty before decrementing b, set b
                           consistently and abort */
        q->bottom = t;
        return NULL;
    }

    // read the element at b
    removed = q->elements[b & q->moduloSize];

    if (currSize > 0) { /* no danger, still elements in buffer after b-- */
        // debugBelch("popWSDeque: t=%ld b=%ld = %ld\n", t, b, removed);
        return removed;
    }
    /* otherwise, has someone meanwhile stolen the same (last) element?
       Check and increment top value to know  */
    if ( !(CASTOP(&(q->top),t,t+1)) ) {
        removed = NULL; /* no success, but continue adjusting bottom */
    }
    q->bottom = t+1; /* anyway, empty now. Adjust bottom consistently. */
    q->topBound = t+1; /* ...and cached top value as well */

    ASSERT_WSDEQUE_INVARIANTS(q);
    ASSERT(q->bottom >= q->top);

    // debugBelch("popWSDeque: t=%ld b=%ld = %ld\n", t, b, removed);

    return removed;
}

/* -----------------------------------------------------------------------------
 * stealWSDeque
 * -------------------------------------------------------------------------- */

void *
stealWSDeque_ (WSDeque *q)
{
    void * stolen;
    StgWord b,t;

// Can't do this on someone else's spark pool:
// ASSERT_WSDEQUE_INVARIANTS(q);

    // NB. these loads must be ordered, otherwise there is a race
    // between steal and pop.
    t = q->top;
    load_load_barrier();
    b = q->bottom;

    // NB. b and t are unsigned; we need a signed value for the test
    // below, because it is possible that t > b during a
    // concurrent popWSQueue() operation.
    if ((long)b - (long)t <= 0 ) {
        return NULL; /* already looks empty, abort */
    }
    // NB. the load of q->bottom must be ordered before the load of
    // q->elements[t & q-> moduloSize]. See comment "KG:..." below
    // and Ticket #13633.
    load_load_barrier();
    /* now access array, see pushBottom() */
    stolen = q->elements[t & q->moduloSize];

    /* now decide whether we have won */
    if ( !(CASTOP(&(q->top),t,t+1)) ) {
        /* lost the race, someone else has changed top in the meantime */
        return NULL;
    }  /* else: OK, top has been incremented by the cas call */

    // debugBelch("stealWSDeque_: t=%d b=%d\n", t, b);

// Can't do this on someone else's spark pool:
// ASSERT_WSDEQUE_INVARIANTS(q);

    return stolen;
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

#define DISCARD_NEW

/* enqueue an element. Should always succeed by resizing the array
   (not implemented yet, silently fails in that case). */
bool
pushWSDeque (WSDeque* q, void * elem)
{
    StgWord t;
    StgWord b;
    StgWord sz = q->moduloSize;

    ASSERT_WSDEQUE_INVARIANTS(q);

    /* we try to avoid reading q->top (accessed by all) and use
       q->topBound (accessed only by writer) instead.
       This is why we do not just call empty(q) here.
    */
    b = q->bottom;
    t = q->topBound;
    if ( (StgInt)b - (StgInt)t >= (StgInt)sz ) {
        /* NB. 1. sz == q->size - 1, thus ">="
           2. signed comparison, it is possible that t > b
        */
        /* could be full, check the real top value in this case */
        t = q->top;
        q->topBound = t;
        if (b - t >= sz) { /* really no space left :-( */
            /* reallocate the array, copying the values. Concurrent steal()s
               will in the meantime use the old one and modify only top.
               This means: we cannot safely free the old space! Can keep it
               on a free list internally here...

               Potential bug in combination with steal(): if array is
               replaced, it is unclear which one concurrent steal operations
               use. Must read the array base address in advance in steal().
            */
#if defined(DISCARD_NEW)
            ASSERT_WSDEQUE_INVARIANTS(q);
            return false; // we didn't push anything
#else
            /* could make room by incrementing the top position here.  In
             * this case, should use CASTOP. If this fails, someone else has
             * removed something, and new room will be available.
             */
            ASSERT_WSDEQUE_INVARIANTS(q);
#endif
        }
    }

    q->elements[b & sz] = elem;
    /*
       KG: we need to put write barrier here since otherwise we might
       end with elem not added to q->elements, but q->bottom already
       modified (write reordering) and with stealWSDeque_ failing
       later when invoked from another thread since it thinks elem is
       there (in case there is just added element in the queue). This
       issue concretely hit me on ARMv7 multi-core CPUs
     */
    write_barrier();
    q->bottom = b + 1;

    ASSERT_WSDEQUE_INVARIANTS(q);
    return true;
}
