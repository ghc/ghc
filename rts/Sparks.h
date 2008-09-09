/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2006
 *
 * Sparking support for GRAN, PAR and THREADED_RTS versions of the RTS.
 * 
 * ---------------------------------------------------------------------------*/

#ifndef SPARKS_H
#define SPARKS_H

#if defined(THREADED_RTS)
StgClosure * findSpark         (Capability *cap);
void         initSparkPools    (void);
void         freeSparkPool     (StgSparkPool *pool);
void         createSparkThread (Capability *cap, StgClosure *p);
void         pruneSparkQueues  (void);
void         traverseSparkQueue(evac_fn evac, void *user, Capability *cap);

INLINE_HEADER void     discardSparks  (StgSparkPool *pool);
INLINE_HEADER nat      sparkPoolSize  (StgSparkPool *pool);
INLINE_HEADER rtsBool  emptySparkPool (StgSparkPool *pool);

INLINE_HEADER void     discardSparksCap  (Capability *cap);
INLINE_HEADER nat      sparkPoolSizeCap  (Capability *cap);
INLINE_HEADER rtsBool  emptySparkPoolCap (Capability *cap);
#endif

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL) || defined(THREADED_RTS)

INLINE_HEADER rtsBool
emptySparkPool (StgSparkPool *pool)
{
    return (pool->hd == pool->tl);
}

INLINE_HEADER rtsBool
emptySparkPoolCap (Capability *cap) 
{ return emptySparkPool(&cap->r.rSparks); }

INLINE_HEADER nat
sparkPoolSize (StgSparkPool *pool) 
{
    if (pool->hd <= pool->tl) {
	return (pool->tl - pool->hd);
    } else {
	return (pool->lim - pool->hd + pool->tl - pool->base);
    }
}

INLINE_HEADER nat
sparkPoolSizeCap (Capability *cap) 
{ return sparkPoolSize(&cap->r.rSparks); }

INLINE_HEADER void
discardSparks (StgSparkPool *pool)
{
    pool->hd = pool->tl;
}

INLINE_HEADER void
discardSparksCap (Capability *cap) 
{ return discardSparks(&cap->r.rSparks); }


#elif defined(THREADED_RTS) 

INLINE_HEADER rtsBool
emptySparkPoolCap (Capability *cap STG_UNUSED)
{ return rtsTrue; }

#endif

#endif /* SPARKS_H */
