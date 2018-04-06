/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2009
 *
 * Sparking support for PAR and THREADED_RTS versions of the RTS.
 * 
 * ---------------------------------------------------------------------------*/

#pragma once

#include "WSDeque.h"

#include "BeginPrivate.h"

/* typedef for SparkPool in RtsTypes.h */

/* Stats on spark creation/conversion */
typedef struct {
    StgWord created;
    StgWord dud;
    StgWord overflowed;
    StgWord converted;
    StgWord gcd;
    StgWord fizzled;
} SparkCounters;

#if defined(THREADED_RTS)

typedef WSDeque SparkPool;

// Initialisation
SparkPool *allocSparkPool (void);

// Take a spark from the "write" end of the pool.  Can be called
// by the pool owner only.
INLINE_HEADER StgClosure* reclaimSpark(SparkPool *pool);

// Returns True if the spark pool is empty (can give a false positive
// if the pool is almost empty).
INLINE_HEADER bool looksEmpty(SparkPool* deque);

INLINE_HEADER StgClosure * tryStealSpark (SparkPool *pool);
INLINE_HEADER bool         fizzledSpark  (StgClosure *);

void         freeSparkPool     (SparkPool *pool);
void         createSparkThread (Capability *cap);
void         traverseSparkQueue(evac_fn evac, void *user, Capability *cap);
void         pruneSparkQueue   (Capability *cap);

INLINE_HEADER void discardSparks  (SparkPool *pool);
INLINE_HEADER long sparkPoolSize  (SparkPool *pool);

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

INLINE_HEADER StgClosure* reclaimSpark(SparkPool *pool)
{
    return popWSDeque(pool);
}

INLINE_HEADER bool looksEmpty(SparkPool* deque)
{
    return looksEmptyWSDeque(deque);
}

INLINE_HEADER long sparkPoolSize (SparkPool *pool) 
{ 
    return dequeElements(pool);
}

INLINE_HEADER void discardSparks (SparkPool *pool)
{
    discardElements(pool);
}

/* ----------------------------------------------------------------------------
 * 
 * tryStealSpark: try to steal a spark from a Capability.
 *
 * Returns either:
 *  (a) a useful spark;
 *  (b) a fizzled spark (use fizzledSpark to check);
 *  (c) or NULL if the pool was empty, and can occasionally return NULL
 *      if there was a race with another thread stealing from the same
 *      pool.  In this case, try again later.
 *
 -------------------------------------------------------------------------- */

INLINE_HEADER StgClosure * tryStealSpark (SparkPool *pool)
{
    return stealWSDeque_(pool);
    // use the no-loopy version, stealWSDeque_(), since if we get a
    // spurious NULL here the caller may want to try stealing from
    // other pools before trying again.
}

INLINE_HEADER bool fizzledSpark (StgClosure *spark)
{
    return (GET_CLOSURE_TAG(spark) != 0 || !closure_SHOULD_SPARK(spark));
}

#endif // THREADED_RTS

#include "EndPrivate.h"
