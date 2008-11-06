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

/* Spark pools: used to store pending sparks 
 *  (THREADED_RTS & PARALLEL_HASKELL only)
 * Implementation uses a DeQue to enable concurrent read accesses at
 * the top end.
 */
typedef struct  SparkPool_ {
  /* Size of elements array. Used for modulo calculation: we round up
     to powers of 2 and use the dyadic log (modulo == bitwise &) */
  StgWord size; 
  StgWord moduloSize; /* bitmask for modulo */

  /* top, index where multiple readers steal() (protected by a cas) */
  volatile StgWord top;

  /* bottom, index of next free place where one writer can push
     elements. This happens unsynchronised. */
  volatile StgWord bottom;
  /* both position indices are continuously incremented, and used as
     an index modulo the current array size. */
  
  /* lower bound on the current top value. This is an internal
     optimisation to avoid unnecessarily accessing the top field
     inside pushBottom */
  volatile StgWord topBound;

  /* The elements array */
  StgClosurePtr* elements;
  /*  Please note: the dataspace cannot follow the admin fields
      immediately, as it should be possible to enlarge it without
      disposing the old one automatically (as realloc would)! */

} SparkPool;


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

// Initialisation
void initSparkPools (void);

// Take a spark from the "write" end of the pool.  Can be called
// by the pool owner only.
StgClosure* reclaimSpark(SparkPool *pool);

// Returns True if the spark pool is empty (can give a false positive
// if the pool is almost empty).
rtsBool looksEmpty(SparkPool* deque);

StgClosure * tryStealSpark     (Capability *cap);
void         freeSparkPool     (SparkPool *pool);
void         createSparkThread (Capability *cap);
void         traverseSparkQueue(evac_fn evac, void *user, Capability *cap);
void         pruneSparkQueue   (evac_fn evac, void *user, Capability *cap);

INLINE_HEADER void discardSparks  (SparkPool *pool);
INLINE_HEADER nat  sparkPoolSize  (SparkPool *pool);
#endif

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL) || defined(THREADED_RTS)

INLINE_HEADER rtsBool  
emptySparkPool (SparkPool *pool) 
{ return looksEmpty(pool); }

INLINE_HEADER nat
sparkPoolSize (SparkPool *pool) 
{ return (pool->bottom - pool->top); }

INLINE_HEADER void
discardSparks (SparkPool *pool)
{
    pool->top = pool->topBound = pool->bottom = 0;
}

#endif

#endif /* SPARKS_H */
