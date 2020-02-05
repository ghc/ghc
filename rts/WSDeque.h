/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * Work-stealing Deque data structure
 *
 * ---------------------------------------------------------------------------*/

#pragma once

typedef struct WSDeque_ {
    // Size of elements array. Used for modulo calculation: we round up
    // to powers of 2 and use the dyadic log (modulo == bitwise &)
    StgInt size;
    StgWord moduloSize; /* bitmask for modulo */

    // top, index where multiple readers steal() (protected by a cas)
    StgInt top;

    // bottom, index of next free place where one writer can push
    // elements. This happens unsynchronised.
    StgInt bottom;

    // both top and bottom are continuously incremented, and used as
    // an index modulo the current array size.

    // The elements array
    void ** elements;

    //  Please note: the dataspace cannot follow the admin fields
    //  immediately, as it should be possible to enlarge it without
    //  disposing the old one automatically (as realloc would)!

} WSDeque;

/* INVARIANTS, in this order: reasonable size,
   space pointer, space accessible to us.

   NB. This is safe to use only (a) on a spark pool owned by the
   current thread, or (b) when there's only one thread running, or no
   stealing going on (e.g. during GC).
*/
#define ASSERT_WSDEQUE_INVARIANTS(p)                                  \
    ASSERT((p)->size > 0);                                            \
    ASSERT(RELAXED_LOAD(&(p)->elements) != NULL);                     \
    ASSERT(RELAXED_LOAD(&(p)->elements[0]) || 1);                     \
    ASSERT(RELAXED_LOAD(&(p)->elements[(p)->size - 1]) || 1);

// No: it is possible that top > bottom when using pop()
//  ASSERT((p)->bottom >= (p)->top);
//  ASSERT((p)->size > (p)->bottom - (p)->top);

/* -----------------------------------------------------------------------------
 * Operations
 *
 * A WSDeque has an *owner* thread.  The owner can perform any operation;
 * other threads are only allowed to call stealWSDeque_(),
 * stealWSDeque(), looksEmptyWSDeque(), and dequeElements().
 *
 * -------------------------------------------------------------------------- */

// Allocation, deallocation
WSDeque * newWSDeque  (uint32_t size);
void      freeWSDeque (WSDeque *q);

// (owner-only) Take an element from the "write" end of the pool.  Can be called
// by the pool owner only.
void* popWSDeque (WSDeque *q);

// (owner-only) Push onto the "write" end of the pool.  Return true if the push
// succeeded, or false if the deque is full.
bool pushWSDeque (WSDeque *q, void *elem);

// (owner-only) Removes all elements from the deque.
EXTERN_INLINE void discardElements (WSDeque *q);

// Removes an element of the deque from the "read" end, or returns
// NULL if the pool is empty, or if there was a collision with another
// thief.
void * stealWSDeque_ (WSDeque *q);

// Removes an element of the deque from the "read" end, or returns
// NULL if the pool is empty.
void * stealWSDeque (WSDeque *q);

// "guesses" whether a deque is empty. Can return false negatives in
// presence of concurrent steal() calls, and false positives in
// presence of a concurrent pushBottom().
EXTERN_INLINE bool looksEmptyWSDeque (WSDeque* q);

// "guesses" how many elements are present on the deque. Like
// looksEmptyWSDeque, this may suggest that the deque is empty when it's not
// and vice-versa.
EXTERN_INLINE StgInt dequeElements   (WSDeque *q);

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

EXTERN_INLINE StgInt
dequeElements (WSDeque *q)
{
    StgWord t = ACQUIRE_LOAD(&q->top);
    StgWord b = ACQUIRE_LOAD(&q->bottom);
    // try to prefer false negatives by reading top first
    StgInt n = (StgInt)b - (StgInt)t;
    return n > 0 ? n : 0;
}

EXTERN_INLINE bool
looksEmptyWSDeque (WSDeque *q)
{
    return (dequeElements(q) <= 0);
}

EXTERN_INLINE void
discardElements (WSDeque *q)
{
    RELAXED_STORE(&q->top, RELAXED_LOAD(&q->bottom));
}
