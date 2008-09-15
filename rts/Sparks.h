/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2006
 *
 * Sparking support for GRAN, PAR and THREADED_RTS versions of the RTS.
 * 
 * ---------------------------------------------------------------------------*/

#ifndef SPARKS_H
#define SPARKS_H

#if defined(PARALLEL_HASKELL)
#error Sparks.c using new internal structure, needs major overhaul!
#endif

/* typedef for SparkPool in RtsTypes.h */

#if defined(THREADED_RTS)

/* INVARIANTS, in this order: bottom/top consistent, reasonable size,
   topBound consistent, space pointer, space accessible to us */
#define ASSERT_SPARK_POOL_INVARIANTS(p)         \
  ASSERT((p)->bottom >= (p)->top);              \
  ASSERT((p)->size > 0);                        \
  ASSERT((p)->size > (p)->bottom - (p)->top);	\
  ASSERT((p)->topBound <= (p)->top);            \
  ASSERT((p)->elements != NULL);                \
  ASSERT(*((p)->elements) || 1);                \
  ASSERT(*((p)->elements - 1  + ((p)->size)) || 1);

// missing in old interface. Currently called by initSparkPools
// internally.
SparkPool* initPool(StgWord size);

// special case: accessing our own pool, at the write end
// otherwise, we can always steal from our pool as the others do...
StgClosure* reclaimSpark(Capability *cap);

rtsBool looksEmpty(SparkPool* deque);

// rest: same as old interface
StgClosure * findSpark         (Capability *cap);
void         initSparkPools    (void);
void         freeSparkPool     (SparkPool *pool);
void         createSparkThread (Capability *cap, StgClosure *p);
void         pruneSparkQueues  (void);
void         traverseSparkQueue(evac_fn evac, void *user, Capability *cap);

INLINE_HEADER void     discardSparks  (SparkPool *pool);
INLINE_HEADER nat      sparkPoolSize  (SparkPool *pool);

INLINE_HEADER void     discardSparksCap  (Capability *cap);
INLINE_HEADER nat      sparkPoolSizeCap  (Capability *cap);
INLINE_HEADER rtsBool  emptySparkPoolCap (Capability *cap);
#endif

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL) || defined(THREADED_RTS)

INLINE_HEADER rtsBool  
emptySparkPool (SparkPool *pool) 
{ return looksEmpty(pool); }

INLINE_HEADER rtsBool
emptySparkPoolCap (Capability *cap) 
{ return looksEmpty(cap->sparks); }

INLINE_HEADER nat
sparkPoolSize (SparkPool *pool) 
{
  return (pool->bottom - pool->top);
}

INLINE_HEADER nat
sparkPoolSizeCap (Capability *cap) 
{ return sparkPoolSize(cap->sparks); }

INLINE_HEADER void
discardSparks (SparkPool *pool)
{
    pool->top = pool->bottom = 0;
}

INLINE_HEADER void
discardSparksCap (Capability *cap) 
{ return discardSparks(cap->sparks); }

#endif

#endif /* SPARKS_H */
