/* -----------------------------------------------------------------------------
 * $Id: Sparks.c,v 1.1 2000/01/12 15:15:18 simonmar Exp $
 *
 * (c) The GHC Team, 2000
 *
 * Sparking support for PAR and SMP versions of the RTS.
 *
 * ---------------------------------------------------------------------------*/

#if defined(SMP) || defined(PAR)

#include "Rts.h"
#include "Schedule.h"
#include "SchedAPI.h"
#include "Sparks.h"
#include "Storage.h"
#include "RtsFlags.h"
#include "RtsUtils.h"

static void slide_spark_pool( StgSparkPool *pool );

void
initSparkPools( void )
{
  Capability *cap;
  StgSparkPool *pool;

#ifdef SMP
  /* walk over the capabilities, allocating a spark pool for each one */
  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
#else
  /* allocate a single spark pool */
  cap = &MainRegTable;
  {
#endif
    pool = &(cap->rSparks);
    
    pool->base = stgMallocBytes(RtsFlags.ParFlags.maxLocalSparks
				     * sizeof(StgClosure *),
				     "initSparkPools");
    pool->lim = pool->base + RtsFlags.ParFlags.maxLocalSparks;
    pool->hd  = pool->base;
    pool->tl  = pool->base;
  }
}

StgClosure *
findSpark( void )
{
  Capability *cap;
  StgSparkPool *pool;
  StgClosure *spark;

#ifdef SMP
  /* walk over the capabilities, allocating a spark pool for each one */
  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
#else
  /* allocate a single spark pool */
  cap = &MainRegTable;
  {
#endif
    pool = &(cap->rSparks);
    while (pool->hd < pool->tl) {
      spark = *pool->hd++;
      if (closure_SHOULD_SPARK(spark))
	return spark;
    }
    slide_spark_pool(pool);
  }
  return NULL;
}
  
rtsBool
add_to_spark_queue( StgClosure *closure, StgSparkPool *pool )
{
  if (pool->tl == pool->lim)
    slide_spark_pool(pool);

  if (closure_SHOULD_SPARK(closure) && 
      pool->tl < pool->lim) {
    *(pool->tl++) = closure;
    return rtsTrue;
  } else {
    return rtsFalse;
  }
}

static void
slide_spark_pool( StgSparkPool *pool )
{
  StgClosure **sparkp, **to_sparkp;

  sparkp = pool->hd;
  to_sparkp = pool->base;
  while (sparkp < pool->tl) {
    ASSERT(to_sparkp<=sparkp);
    ASSERT(*sparkp!=NULL);
    ASSERT(LOOKS_LIKE_GHC_INFO((*sparkp)->header.info));

    if (closure_SHOULD_SPARK(*sparkp)) {
      *to_sparkp++ = *sparkp++;
    } else {
      sparkp++;
    }
  }
  pool->hd = pool->base;
  pool->tl = to_sparkp;
}

static inline nat
spark_queue_len( StgSparkPool *pool ) 
{
  return (nat) (pool->tl - pool->hd);
}

/* Mark all nodes pointed to by sparks in the spark queues (for GC) Does an
   implicit slide i.e. after marking all sparks are at the beginning of the
   spark pool and the spark pool only contains sparkable closures 
*/
void
markSparkQueue( void )
{ 
  StgClosure **sparkp, **to_sparkp;
#ifdef DEBUG
  nat n, pruned_sparks;
#endif
  StgSparkPool *pool;
  Capability *cap;

#ifdef SMP
  /* walk over the capabilities, allocating a spark pool for each one */
  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
#else
  /* allocate a single spark pool */
  cap = &MainRegTable;
  {
#endif
    pool = &(cap->rSparks);
    
#ifdef DEBUG
    n = 0;
    pruned_sparks = 0;
#endif

    sparkp = pool->hd;
    to_sparkp = pool->base;
    while (sparkp < pool->tl) {
      ASSERT(to_sparkp<=sparkp);
      ASSERT(*sparkp!=NULL);
      ASSERT(LOOKS_LIKE_GHC_INFO(((StgClosure *)*sparkp)->header.info));
      // ToDo?: statistics gathering here (also for GUM!)
      if (closure_SHOULD_SPARK(*sparkp)) {
	*to_sparkp = MarkRoot(*sparkp);
	to_sparkp++;
#ifdef DEBUG
	n++;
#endif
      } else {
#ifdef DEBUG
	pruned_sparks++;
#endif
      }
      sparkp++;
    }
    pool->hd = pool->base;
    pool->tl = to_sparkp;

#if defined(SMP)
    IF_DEBUG(scheduler,
	     belch("markSparkQueue: marked %d sparks and pruned %d sparks on [%x]",
		   n, pruned_sparks, pthread_self()));
#elif defined(PAR)
    IF_DEBUG(scheduler,
	     belch("markSparkQueue: marked %d sparks and pruned %d sparks on [%x]",
		   n, pruned_sparks, mytid));
#else
    IF_DEBUG(scheduler,
	     belch("markSparkQueue: marked %d sparks and pruned %d sparks",
		   n, pruned_sparks));
#endif

    IF_DEBUG(scheduler,
	     belch("markSparkQueue:   new spark queue len=%d; (hd=%p; tl=%p)",
		   spark_queue_len(pool), pool->hd, pool->tl));

  }
}

#endif /* SMP || PAR */

#if defined(GRAN)

... ToDo ...

#endif
