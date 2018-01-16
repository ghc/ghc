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
    StgWord size;
    StgWord moduloSize; /* bitmask for modulo */

    // top, index where multiple readers steal() (protected by a cas)
    volatile StgWord top;

    // bottom, index of next free place where one writer can push
    // elements. This happens unsynchronised.
    volatile StgWord bottom;

    // both top and bottom are continuously incremented, and used as
    // an index modulo the current array size.

    // lower bound on the current top value. This is an internal
    // optimisation to avoid unnecessarily accessing the top field
    // inside pushBottom
    volatile StgWord topBound;

    // The elements array
    void ** elements;

    //  Please note: the dataspace cannot follow the admin fields
    //  immediately, as it should be possible to enlarge it without
    //  disposing the old one automatically (as realloc would)!

} WSDeque;

/* INVARIANTS, in this order: reasonable size,
   topBound consistent, space pointer, space accessible to us.

   NB. This is safe to use only (a) on a spark pool owned by the
   current thread, or (b) when there's only one thread running, or no
   stealing going on (e.g. during GC).
*/
#define ASSERT_WSDEQUE_INVARIANTS(p)         \
  ASSERT((p)->size > 0);                        \
  ASSERT((p)->topBound <= (p)->top);            \
  ASSERT((p)->elements != NULL);                \
  ASSERT(*((p)->elements) || 1);                \
  ASSERT(*((p)->elements - 1  + ((p)->size)) || 1);

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

// Take an element from the "write" end of the pool.  Can be called
// by the pool owner only.
void* popWSDeque (WSDeque *q);

// Push onto the "write" end of the pool.  Return true if the push
// succeeded, or false if the deque is full.
bool pushWSDeque (WSDeque *q, void *elem);

// Removes all elements from the deque
EXTERN_INLINE void discardElements (WSDeque *q);

// Removes an element of the deque from the "read" end, or returns
// NULL if the pool is empty, or if there was a collision with another
// thief.
void * stealWSDeque_ (WSDeque *q);

// Removes an element of the deque from the "read" end, or returns
// NULL if the pool is empty.
void * stealWSDeque (WSDeque *q);

// "guesses" whether a deque is empty. Can return false negatives in
//  presence of concurrent steal() calls, and false positives in
//  presence of a concurrent pushBottom().
EXTERN_INLINE bool looksEmptyWSDeque (WSDeque* q);

EXTERN_INLINE long dequeElements   (WSDeque *q);

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

EXTERN_INLINE long
dequeElements (WSDeque *q)
{
    StgWord t = q->top;
    StgWord b = q->bottom;
    // try to prefer false negatives by reading top first
    return ((long)b - (long)t);
}

EXTERN_INLINE bool
looksEmptyWSDeque (WSDeque *q)
{
    return (dequeElements(q) <= 0);
}

EXTERN_INLINE void
discardElements (WSDeque *q)
{
    q->top = q->bottom;
//    pool->topBound = pool->top;
}
