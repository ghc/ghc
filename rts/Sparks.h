/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2009
 *
 * Sparking support for GRAN, PAR and THREADED_RTS versions of the RTS.
 * 
 * ---------------------------------------------------------------------------*/

#ifndef SPARKS_H
#define SPARKS_H

#include "WSDeque.h"

#include "BeginPrivate.h"

/* typedef for SparkPool in RtsTypes.h */

#if defined(THREADED_RTS)

typedef WSDeque SparkPool;

// Initialisation
void initSparkPools (void);

// Take a spark from the "write" end of the pool.  Can be called
// by the pool owner only.
INLINE_HEADER StgClosure* reclaimSpark(SparkPool *pool);

// Returns True if the spark pool is empty (can give a false positive
// if the pool is almost empty).
INLINE_HEADER rtsBool looksEmpty(SparkPool* deque);

StgClosure * tryStealSpark     (Capability *cap);
INLINE_HEADER rtsBool      fizzledSpark  (StgClosure *);

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

INLINE_HEADER rtsBool looksEmpty(SparkPool* deque)
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

INLINE_HEADER rtsBool fizzledSpark (StgClosure *spark)
{
    return (GET_CLOSURE_TAG(spark) != 0 || !closure_SHOULD_SPARK(spark));
}

#endif // THREADED_RTS

#include "EndPrivate.h"

#endif /* SPARKS_H */
