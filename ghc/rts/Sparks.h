/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * Sparking support for GRAN, PAR and SMP versions of the RTS.
 * 
 * ---------------------------------------------------------------------------*/

#ifndef SPARKS_H
#define SPARKS_H

#if defined(PARALLEL_HASKELL) || defined(SMP)
StgClosure * findSpark         (Capability *cap);
void         initSparkPools    (void);
void         markSparkQueue    (evac_fn evac);
void         createSparkThread (Capability *cap, StgClosure *p);
StgInt       newSpark          (StgRegTable *reg, StgClosure *p);

INLINE_HEADER void     discardSparks  (StgSparkPool *pool);
INLINE_HEADER nat      sparkPoolSize  (StgSparkPool *pool);
INLINE_HEADER rtsBool  emptySparkPool (StgSparkPool *pool);

INLINE_HEADER void     discardSparksCap  (Capability *cap);
INLINE_HEADER nat      sparkPoolSizeCap  (Capability *cap);
INLINE_HEADER rtsBool  emptySparkPoolCap (Capability *cap);
#endif

#if defined(PARALLEL_HASKELL)
StgTSO      *activateSpark (rtsSpark spark) ;
rtsBool      add_to_spark_queue( StgClosure *closure, StgSparkPool *pool );
void         markSparkQueue( void );
nat          spark_queue_len( StgSparkPool *pool );
void         disposeSpark( StgClosure *spark );
#endif

#if defined(GRAN)
void      findLocalSpark (rtsEvent *event, rtsBool *found_res, rtsSparkQ *spark_res);
rtsBool   activateSpark (rtsEvent *event, rtsSparkQ spark);
rtsSpark *newSpark(StgClosure *node, nat name, nat gran_info, 
		   nat size_info, nat par_info, nat local);
void      add_to_spark_queue(rtsSpark *spark);
rtsSpark *delete_from_sparkq (rtsSpark *spark, PEs p, rtsBool dispose_too);
void 	  disposeSpark(rtsSpark *spark);
void 	  disposeSparkQ(rtsSparkQ spark);
void 	  print_spark(rtsSpark *spark);
void      print_sparkq(PEs proc);
void 	  print_sparkq_stats(void);
nat  	  spark_queue_len(PEs proc);
void      markSparkQueue(void);
#endif

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL) || defined(SMP)

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
	return (pool->hd - pool->tl);
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
