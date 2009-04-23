/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2008
 *
 * Sparking support for PARALLEL_HASKELL and THREADED_RTS versions of the RTS.
 *
 -------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Storage.h"
#include "Schedule.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "ParTicky.h"
#include "Trace.h"
#include "Prelude.h"

#include "SMP.h" // for cas

#include "Sparks.h"

#if defined(THREADED_RTS) || defined(PARALLEL_HASKELL)

void
initSparkPools( void )
{
#ifdef THREADED_RTS
    /* walk over the capabilities, allocating a spark pool for each one */
    nat i;
    for (i = 0; i < n_capabilities; i++) {
      capabilities[i].sparks = newWSDeque(RtsFlags.ParFlags.maxLocalSparks);
    }
#else
    /* allocate a single spark pool */
    MainCapability->sparks = newWSDeque(RtsFlags.ParFlags.maxLocalSparks);
#endif
}

void
freeSparkPool (SparkPool *pool)
{
    freeWSDeque(pool);
}

/* -----------------------------------------------------------------------------
 * 
 * Turn a spark into a real thread
 *
 * -------------------------------------------------------------------------- */

void
createSparkThread (Capability *cap)
{
    StgTSO *tso;

    tso = createIOThread (cap, RtsFlags.GcFlags.initialStkSize, 
                          &base_GHCziConc_runSparks_closure);

    postEvent(cap, EVENT_CREATE_SPARK_THREAD, 0, tso->id);

    appendToRunQueue(cap,tso);
}

/* --------------------------------------------------------------------------
 * newSpark: create a new spark, as a result of calling "par"
 * Called directly from STG.
 * -------------------------------------------------------------------------- */

StgInt
newSpark (StgRegTable *reg, StgClosure *p)
{
    Capability *cap = regTableToCapability(reg);
    SparkPool *pool = cap->sparks;

    /* I am not sure whether this is the right thing to do.
     * Maybe it is better to exploit the tag information
     * instead of throwing it away?
     */
    p = UNTAG_CLOSURE(p);

    if (closure_SHOULD_SPARK(p)) {
        pushWSDeque(pool,p);
    }	

    cap->sparks_created++;

    postEvent(cap, EVENT_CREATE_SPARK, cap->r.rCurrentTSO->id, 0);

    return 1;
}

/* -----------------------------------------------------------------------------
 * 
 * tryStealSpark: try to steal a spark from a Capability.
 *
 * Returns a valid spark, or NULL if the pool was empty, and can
 * occasionally return NULL if there was a race with another thread
 * stealing from the same pool.  In this case, try again later.
 *
 -------------------------------------------------------------------------- */

StgClosure *
tryStealSpark (Capability *cap)
{
  SparkPool *pool = cap->sparks;
  StgClosure *stolen;

  do { 
      stolen = stealWSDeque_(pool); 
      // use the no-loopy version, stealWSDeque_(), since if we get a
      // spurious NULL here the caller may want to try stealing from
      // other pools before trying again.
  } while (stolen != NULL && !closure_SHOULD_SPARK(stolen));

  return stolen;
}

/* --------------------------------------------------------------------------
 * Remove all sparks from the spark queues which should not spark any
 * more.  Called after GC. We assume exclusive access to the structure
 * and replace  all sparks in the queue, see explanation below. At exit,
 * the spark pool only contains sparkable closures.
 * -------------------------------------------------------------------------- */

void
pruneSparkQueue (evac_fn evac, void *user, Capability *cap)
{ 
    SparkPool *pool;
    StgClosurePtr spark, tmp, *elements;
    nat n, pruned_sparks; // stats only
    StgWord botInd,oldBotInd,currInd; // indices in array (always < size)
    const StgInfoTable *info;
    
    PAR_TICKY_MARK_SPARK_QUEUE_START();
    
    n = 0;
    pruned_sparks = 0;
    
    pool = cap->sparks;
    
    // it is possible that top > bottom, indicating an empty pool.  We
    // fix that here; this is only necessary because the loop below
    // assumes it.
    if (pool->top > pool->bottom)
        pool->top = pool->bottom;

    // Take this opportunity to reset top/bottom modulo the size of
    // the array, to avoid overflow.  This is only possible because no
    // stealing is happening during GC.
    pool->bottom  -= pool->top & ~pool->moduloSize;
    pool->top     &= pool->moduloSize;
    pool->topBound = pool->top;

    debugTrace(DEBUG_sched,
               "markSparkQueue: current spark queue len=%ld; (hd=%ld; tl=%ld)",
               sparkPoolSize(pool), pool->bottom, pool->top);

    ASSERT_WSDEQUE_INVARIANTS(pool);

    elements = (StgClosurePtr *)pool->elements;

    /* We have exclusive access to the structure here, so we can reset
       bottom and top counters, and prune invalid sparks. Contents are
       copied in-place if they are valuable, otherwise discarded. The
       routine uses "real" indices t and b, starts by computing them
       as the modulus size of top and bottom,

       Copying:

       At the beginning, the pool structure can look like this:
       ( bottom % size >= top % size , no wrap-around)
                  t          b
       ___________***********_________________

       or like this ( bottom % size < top % size, wrap-around )
                  b         t
       ***********__________******************
       As we need to remove useless sparks anyway, we make one pass
       between t and b, moving valuable content to b and subsequent
       cells (wrapping around when the size is reached).

                     b      t
       ***********OOO_______XX_X__X?**********
                     ^____move?____/

       After this movement, botInd becomes the new bottom, and old
       bottom becomes the new top index, both as indices in the array
       size range.
    */
    // starting here
    currInd = (pool->top) & (pool->moduloSize); // mod

    // copies of evacuated closures go to space from botInd on
    // we keep oldBotInd to know when to stop
    oldBotInd = botInd = (pool->bottom) & (pool->moduloSize); // mod

    // on entry to loop, we are within the bounds
    ASSERT( currInd < pool->size && botInd  < pool->size );

    while (currInd != oldBotInd ) {
      /* must use != here, wrap-around at size
	 subtle: loop not entered if queue empty
       */

      /* check element at currInd. if valuable, evacuate and move to
	 botInd, otherwise move on */
      spark = elements[currInd];

      // We have to be careful here: in the parallel GC, another
      // thread might evacuate this closure while we're looking at it,
      // so grab the info pointer just once.
      info = spark->header.info;
      if (IS_FORWARDING_PTR(info)) {
          tmp = (StgClosure*)UN_FORWARDING_PTR(info);
          /* if valuable work: shift inside the pool */
          if (closure_SHOULD_SPARK(tmp)) {
              elements[botInd] = tmp; // keep entry (new address)
              botInd++;
              n++;
          } else {
              pruned_sparks++; // discard spark
              cap->sparks_pruned++;
          }
      } else {
          if (!(closure_flags[INFO_PTR_TO_STRUCT(info)->type] & _NS)) {
              elements[botInd] = spark; // keep entry (new address)
              evac (user, &elements[botInd]);
              botInd++;
              n++;
          } else {
              pruned_sparks++; // discard spark
              cap->sparks_pruned++;
          }
      }
      currInd++;

      // in the loop, we may reach the bounds, and instantly wrap around
      ASSERT( currInd <= pool->size && botInd <= pool->size );
      if ( currInd == pool->size ) { currInd = 0; }
      if ( botInd == pool->size )  { botInd = 0;  }

    } // while-loop over spark pool elements

    ASSERT(currInd == oldBotInd);

    pool->top = oldBotInd; // where we started writing
    pool->topBound = pool->top;

    pool->bottom = (oldBotInd <= botInd) ? botInd : (botInd + pool->size); 
    // first free place we did not use (corrected by wraparound)

    PAR_TICKY_MARK_SPARK_QUEUE_END(n);

    debugTrace(DEBUG_sched, "pruned %d sparks", pruned_sparks);
    
    debugTrace(DEBUG_sched,
               "new spark queue len=%ld; (hd=%ld; tl=%ld)",
               sparkPoolSize(pool), pool->bottom, pool->top);

    ASSERT_WSDEQUE_INVARIANTS(pool);
}

/* GC for the spark pool, called inside Capability.c for all
   capabilities in turn. Blindly "evac"s complete spark pool. */
void
traverseSparkQueue (evac_fn evac, void *user, Capability *cap)
{
    StgClosure **sparkp;
    SparkPool *pool;
    StgWord top,bottom, modMask;
    
    pool = cap->sparks;

    ASSERT_WSDEQUE_INVARIANTS(pool);

    top = pool->top;
    bottom = pool->bottom;
    sparkp = (StgClosurePtr*)pool->elements;
    modMask = pool->moduloSize;

    while (top < bottom) {
    /* call evac for all closures in range (wrap-around via modulo)
     * In GHC-6.10, evac takes an additional 1st argument to hold a
     * GC-specific register, see rts/sm/GC.c::mark_root()
     */
      evac( user , sparkp + (top & modMask) ); 
      top++;
    }

    debugTrace(DEBUG_sched,
               "traversed spark queue, len=%ld; (hd=%ld; tl=%ld)",
               sparkPoolSize(pool), pool->bottom, pool->top);
}

/* ----------------------------------------------------------------------------
 * balanceSparkPoolsCaps: takes an array of capabilities (usually: all
 * capabilities) and its size. Accesses all spark pools and equally
 * distributes the sparks among them.
 *
 * Could be called after GC, before Cap. release, from scheduler. 
 * -------------------------------------------------------------------------- */
void balanceSparkPoolsCaps(nat n_caps, Capability caps[]);

void balanceSparkPoolsCaps(nat n_caps STG_UNUSED, 
                           Capability caps[] STG_UNUSED) {
  barf("not implemented");
}

#else

StgInt
newSpark (StgRegTable *reg STG_UNUSED, StgClosure *p STG_UNUSED)
{
    /* nothing */
    return 1;
}


#endif /* PARALLEL_HASKELL || THREADED_RTS */


/* -----------------------------------------------------------------------------
 * 
 * GRAN & PARALLEL_HASKELL stuff beyond here.
 *
 *  TODO "nuke" this!
 *
 * -------------------------------------------------------------------------- */

#if defined(PARALLEL_HASKELL) || defined(GRAN)

static void slide_spark_pool( StgSparkPool *pool );

rtsBool
add_to_spark_queue( StgClosure *closure, StgSparkPool *pool )
{
  if (pool->tl == pool->lim)
    slide_spark_pool(pool);

  if (closure_SHOULD_SPARK(closure) && 
      pool->tl < pool->lim) {
    *(pool->tl++) = closure;

#if defined(PARALLEL_HASKELL)
    // collect parallel global statistics (currently done together with GC stats)
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      // debugBelch("Creating spark for %x @ %11.2f\n", closure, usertime()); 
      globalParStats.tot_sparks_created++;
    }
#endif
    return rtsTrue;
  } else {
#if defined(PARALLEL_HASKELL)
    // collect parallel global statistics (currently done together with GC stats)
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      //debugBelch("Ignoring spark for %x @ %11.2f\n", closure, usertime()); 
      globalParStats.tot_sparks_ignored++;
    }
#endif
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

void
disposeSpark(spark)
StgClosure *spark;
{
#if !defined(THREADED_RTS)
  Capability *cap;
  StgSparkPool *pool;

  cap = &MainRegTable;
  pool = &(cap->rSparks);
  ASSERT(pool->hd <= pool->tl && pool->tl <= pool->lim);
#endif
  ASSERT(spark != (StgClosure *)NULL);
  /* Do nothing */
}


#elif defined(GRAN)

/* 
   Search the spark queue of the proc in event for a spark that's worth
   turning into a thread 
   (was gimme_spark in the old RTS)
*/
void
findLocalSpark (rtsEvent *event, rtsBool *found_res, rtsSparkQ *spark_res)
{
   PEs proc = event->proc,       /* proc to search for work */
       creator = event->creator; /* proc that requested work */
   StgClosure* node;
   rtsBool found;
   rtsSparkQ spark_of_non_local_node = NULL, 
             spark_of_non_local_node_prev = NULL, 
             low_priority_spark = NULL, 
             low_priority_spark_prev = NULL,
             spark = NULL, prev = NULL;
  
   /* Choose a spark from the local spark queue */
   prev = (rtsSpark*)NULL;
   spark = pending_sparks_hds[proc];
   found = rtsFalse;

   // ToDo: check this code & implement local sparking !! -- HWL  
   while (!found && spark != (rtsSpark*)NULL)
     {
       ASSERT((prev!=(rtsSpark*)NULL || spark==pending_sparks_hds[proc]) &&
	      (prev==(rtsSpark*)NULL || prev->next==spark) &&
	      (spark->prev==prev));
       node = spark->node;
       if (!closure_SHOULD_SPARK(node)) 
         {
	   IF_GRAN_DEBUG(checkSparkQ,
			 debugBelch("^^ pruning spark %p (node %p) in gimme_spark",
			       spark, node));

           if (RtsFlags.GranFlags.GranSimStats.Sparks)
             DumpRawGranEvent(proc, (PEs)0, SP_PRUNED,(StgTSO*)NULL,
			      spark->node, spark->name, spark_queue_len(proc));
  
	   ASSERT(spark != (rtsSpark*)NULL);
	   ASSERT(SparksAvail>0);
	   --SparksAvail;

	   ASSERT(prev==(rtsSpark*)NULL || prev->next==spark);
	   spark = delete_from_sparkq (spark, proc, rtsTrue);
	   if (spark != (rtsSpark*)NULL)
	     prev = spark->prev;
	   continue;
         }
       /* -- node should eventually be sparked */
       else if (RtsFlags.GranFlags.PreferSparksOfLocalNodes && 
               !IS_LOCAL_TO(PROCS(node),CurrentProc)) 
         {
	   barf("Local sparking not yet implemented");

           /* Remember first low priority spark */
           if (spark_of_non_local_node==(rtsSpark*)NULL) {
	     spark_of_non_local_node_prev = prev;
             spark_of_non_local_node = spark;
  	      }
  
           if (spark->next == (rtsSpark*)NULL) { 
  	     /* ASSERT(spark==SparkQueueTl);  just for testing */
  	     prev = spark_of_non_local_node_prev;
  	     spark = spark_of_non_local_node;
             found = rtsTrue;
             break;
           }
  
# if defined(GRAN) && defined(GRAN_CHECK)
           /* Should never happen; just for testing 
           if (spark==pending_sparks_tl) {
             debugBelch("ReSchedule: Last spark != SparkQueueTl\n");
	   	stg_exit(EXIT_FAILURE);
		} */
# endif
  	   prev = spark; 
  	   spark = spark->next;
	   ASSERT(SparksAvail>0);
           --SparksAvail;
	   continue;
         }
       else if ( RtsFlags.GranFlags.DoPrioritySparking || 
  		 (spark->gran_info >= RtsFlags.GranFlags.SparkPriority2) )
         {
	   if (RtsFlags.GranFlags.DoPrioritySparking)
	     barf("Priority sparking not yet implemented");

           found = rtsTrue;
         }
#if 0	   
       else /* only used if SparkPriority2 is defined */
         {
	   /* ToDo: fix the code below and re-integrate it */
           /* Remember first low priority spark */
           if (low_priority_spark==(rtsSpark*)NULL) { 
	     low_priority_spark_prev = prev;
             low_priority_spark = spark;
	   }
  
           if (spark->next == (rtsSpark*)NULL) { 
	        /* ASSERT(spark==spark_queue_tl);  just for testing */
	     prev = low_priority_spark_prev;
	     spark = low_priority_spark;
             found = rtsTrue;       /* take low pri spark => rc is 2  */
             break;
           }
  
           /* Should never happen; just for testing 
           if (spark==pending_sparks_tl) {
             debugBelch("ReSchedule: Last spark != SparkQueueTl\n");
  		stg_exit(EXIT_FAILURE);
             break;
	   } */                
	   prev = spark; 
	   spark = spark->next;

	   IF_GRAN_DEBUG(pri,
			 debugBelch("++ Ignoring spark of priority %u (SparkPriority=%u); node=%p; name=%u\n", 
			       spark->gran_info, RtsFlags.GranFlags.SparkPriority, 
			       spark->node, spark->name);)
           }
#endif
   }  /* while (spark!=NULL && !found) */

   *spark_res = spark;
   *found_res = found;
}

/*
  Turn the spark into a thread.
  In GranSim this basically means scheduling a StartThread event for the
  node pointed to by the spark at some point in the future.
  (was munch_spark in the old RTS)
*/
rtsBool
activateSpark (rtsEvent *event, rtsSparkQ spark) 
{
  PEs proc = event->proc,       /* proc to search for work */
      creator = event->creator; /* proc that requested work */
  StgTSO* tso;
  StgClosure* node;
  rtsTime spark_arrival_time;

  /* 
     We've found a node on PE proc requested by PE creator.
     If proc==creator we can turn the spark into a thread immediately;
     otherwise we schedule a MoveSpark event on the requesting PE
  */
     
  /* DaH Qu' yIchen */
  if (proc!=creator) { 

    /* only possible if we simulate GUM style fishing */
    ASSERT(RtsFlags.GranFlags.Fishing);

    /* Message packing costs for sending a Fish; qeq jabbI'ID */
    CurrentTime[proc] += RtsFlags.GranFlags.Costs.mpacktime;
  
    if (RtsFlags.GranFlags.GranSimStats.Sparks)
      DumpRawGranEvent(proc, (PEs)0, SP_EXPORTED,
		       (StgTSO*)NULL, spark->node,
		       spark->name, spark_queue_len(proc));

    /* time of the spark arrival on the remote PE */
    spark_arrival_time = CurrentTime[proc] + RtsFlags.GranFlags.Costs.latency;

    new_event(creator, proc, spark_arrival_time,
	      MoveSpark,
	      (StgTSO*)NULL, spark->node, spark);

    CurrentTime[proc] += RtsFlags.GranFlags.Costs.mtidytime;
	    
  } else { /* proc==creator i.e. turn the spark into a thread */

    if ( RtsFlags.GranFlags.GranSimStats.Global && 
	 spark->gran_info < RtsFlags.GranFlags.SparkPriority2 ) {

      globalGranStats.tot_low_pri_sparks++;
      IF_GRAN_DEBUG(pri,
		    debugBelch("++ No high priority spark available; low priority (%u) spark chosen: node=%p; name=%u\n",
			  spark->gran_info, 
			  spark->node, spark->name));
    } 
    
    CurrentTime[proc] += RtsFlags.GranFlags.Costs.threadcreatetime;
    
    node = spark->node;
    
# if 0
    /* ToDo: fix the GC interface and move to StartThread handling-- HWL */
    if (GARBAGE COLLECTION IS NECESSARY) {
      /* Some kind of backoff needed here in case there's too little heap */
#  if defined(GRAN_CHECK) && defined(GRAN)
      if (RtsFlags.GcFlags.giveStats)
	fprintf(RtsFlags.GcFlags.statsFile,"***** vIS Qu' chen veQ boSwI'; spark=%p, node=%p;  name=%u\n", 
		/* (found==2 ? "no hi pri spark" : "hi pri spark"), */
		spark, node, spark->name);
#  endif
      new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc]+1,
    		  FindWork,
    		  (StgTSO*)NULL, (StgClosure*)NULL, (rtsSpark*)NULL);
      barf("//// activateSpark: out of heap ; ToDo: call GarbageCollect()");
      GarbageCollect(GetRoots, rtsFalse);
      // HWL old: ReallyPerformThreadGC(TSO_HS+TSO_CTS_SIZE,rtsFalse);
      // HWL old: SAVE_Hp -= TSO_HS+TSO_CTS_SIZE;
      spark = NULL;
      return; /* was: continue; */ /* to the next event, eventually */
    }
# endif
    
    if (RtsFlags.GranFlags.GranSimStats.Sparks)
      DumpRawGranEvent(CurrentProc,(PEs)0,SP_USED,(StgTSO*)NULL,
		       spark->node, spark->name,
		       spark_queue_len(CurrentProc));
    
    new_event(proc, proc, CurrentTime[proc],
	      StartThread, 
	      END_TSO_QUEUE, node, spark); // (rtsSpark*)NULL);
    
    procStatus[proc] = Starting;
  }
}

/* -------------------------------------------------------------------------
   This is the main point where handling granularity information comes into
   play. 
   ------------------------------------------------------------------------- */

#define MAX_RAND_PRI    100

/* 
   Granularity info transformers. 
   Applied to the GRAN_INFO field of a spark.
*/
STATIC_INLINE nat  ID(nat x) { return(x); };
STATIC_INLINE nat  INV(nat x) { return(-x); };
STATIC_INLINE nat  IGNORE(nat x) { return (0); };
STATIC_INLINE nat  RAND(nat x) { return ((random() % MAX_RAND_PRI) + 1); }

/* NB: size_info and par_info are currently unused (what a shame!) -- HWL */
rtsSpark *
newSpark(node,name,gran_info,size_info,par_info,local)
StgClosure *node;
nat name, gran_info, size_info, par_info, local;
{
  nat pri;
  rtsSpark *newspark;

  pri = RtsFlags.GranFlags.RandomPriorities ? RAND(gran_info) :
        RtsFlags.GranFlags.InversePriorities ? INV(gran_info) :
	RtsFlags.GranFlags.IgnorePriorities ? IGNORE(gran_info) :
                           ID(gran_info);

  if ( RtsFlags.GranFlags.SparkPriority!=0 && 
       pri<RtsFlags.GranFlags.SparkPriority ) {
    IF_GRAN_DEBUG(pri,
      debugBelch(",, NewSpark: Ignoring spark of priority %u (SparkPriority=%u); node=%#x; name=%u\n", 
	      pri, RtsFlags.GranFlags.SparkPriority, node, name));
    return ((rtsSpark*)NULL);
  }

  newspark = (rtsSpark*) stgMallocBytes(sizeof(rtsSpark), "NewSpark");
  newspark->prev = newspark->next = (rtsSpark*)NULL;
  newspark->node = node;
  newspark->name = (name==1) ? CurrentTSO->gran.sparkname : name;
  newspark->gran_info = pri;
  newspark->global = !local;      /* Check that with parAt, parAtAbs !!*/

  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_sparks_created++;
    globalGranStats.sparks_created_on_PE[CurrentProc]++;
  }

  return(newspark);
}

void
disposeSpark(spark)
rtsSpark *spark;
{
  ASSERT(spark!=NULL);
  stgFree(spark);
}

void 
disposeSparkQ(spark)
rtsSparkQ spark;
{
  if (spark==NULL) 
    return;

  disposeSparkQ(spark->next);

# ifdef GRAN_CHECK
  if (SparksAvail < 0) {
    debugBelch("disposeSparkQ: SparksAvail<0 after disposing sparkq @ %p\n", &spark);
    print_spark(spark);
  }
# endif

  stgFree(spark);
}

/*
   With PrioritySparking add_to_spark_queue performs an insert sort to keep
   the spark queue sorted. Otherwise the spark is just added to the end of
   the queue. 
*/

void
add_to_spark_queue(spark)
rtsSpark *spark;
{
  rtsSpark *prev = NULL, *next = NULL;
  nat count = 0;
  rtsBool found = rtsFalse;

  if ( spark == (rtsSpark *)NULL ) {
    return;
  }

  if (RtsFlags.GranFlags.DoPrioritySparking && (spark->gran_info != 0) ) {
    /* Priority sparking is enabled i.e. spark queues must be sorted */

    for (prev = NULL, next = pending_sparks_hd, count=0;
	 (next != NULL) && 
	 !(found = (spark->gran_info >= next->gran_info));
	 prev = next, next = next->next, count++) 
     {}

  } else {   /* 'utQo' */
    /* Priority sparking is disabled */
    
    found = rtsFalse;   /* to add it at the end */

  }

  if (found) {
    /* next points to the first spark with a gran_info smaller than that
       of spark; therefore, add spark before next into the spark queue */
    spark->next = next;
    if ( next == NULL ) {
      pending_sparks_tl = spark;
    } else {
      next->prev = spark;
    }
    spark->prev = prev;
    if ( prev == NULL ) {
      pending_sparks_hd = spark;
    } else {
      prev->next = spark;
    }
  } else {  /* (RtsFlags.GranFlags.DoPrioritySparking && !found) || !DoPrioritySparking */
    /* add the spark at the end of the spark queue */
    spark->next = NULL;			       
    spark->prev = pending_sparks_tl;
    if (pending_sparks_hd == NULL)
      pending_sparks_hd = spark;
    else
      pending_sparks_tl->next = spark;
    pending_sparks_tl = spark;	  
  } 
  ++SparksAvail;

  /* add costs for search in priority sparking */
  if (RtsFlags.GranFlags.DoPrioritySparking) {
    CurrentTime[CurrentProc] += count * RtsFlags.GranFlags.Costs.pri_spark_overhead;
  }

  IF_GRAN_DEBUG(checkSparkQ,
		debugBelch("++ Spark stats after adding spark %p (node %p) to queue on PE %d",
		      spark, spark->node, CurrentProc);
		print_sparkq_stats());

#  if defined(GRAN_CHECK)
  if (RtsFlags.GranFlags.Debug.checkSparkQ) {
    for (prev = NULL, next =  pending_sparks_hd;
	 (next != NULL);
	 prev = next, next = next->next) 
      {}
    if ( (prev!=NULL) && (prev!=pending_sparks_tl) )
      debugBelch("SparkQ inconsistency after adding spark %p: (PE %u) pending_sparks_tl (%p) not end of queue (%p)\n",
	      spark,CurrentProc, 
	      pending_sparks_tl, prev);
  }
#  endif

#  if defined(GRAN_CHECK)
  /* Check if the sparkq is still sorted. Just for testing, really!  */
  if ( RtsFlags.GranFlags.Debug.checkSparkQ &&
       RtsFlags.GranFlags.Debug.pri ) {
    rtsBool sorted = rtsTrue;
    rtsSpark *prev, *next;

    if (pending_sparks_hd == NULL ||
	pending_sparks_hd->next == NULL ) {
      /* just 1 elem => ok */
    } else {
      for (prev = pending_sparks_hd,
	   next = pending_sparks_hd->next;
	   (next != NULL) ;
	   prev = next, next = next->next) {
	sorted = sorted && 
	         (prev->gran_info >= next->gran_info);
      }
    }
    if (!sorted) {
      debugBelch("ghuH: SPARKQ on PE %d is not sorted:\n",
	      CurrentProc);
      print_sparkq(CurrentProc);
    }
  }
#  endif
}

nat
spark_queue_len(proc) 
PEs proc;
{
 rtsSpark *prev, *spark;                     /* prev only for testing !! */
 nat len;

 for (len = 0, prev = NULL, spark = pending_sparks_hds[proc]; 
      spark != NULL; 
      len++, prev = spark, spark = spark->next)
   {}

#  if defined(GRAN_CHECK)
  if ( RtsFlags.GranFlags.Debug.checkSparkQ ) 
    if ( (prev!=NULL) && (prev!=pending_sparks_tls[proc]) )
      debugBelch("ERROR in spark_queue_len: (PE %u) pending_sparks_tl (%p) not end of queue (%p)\n",
	      proc, pending_sparks_tls[proc], prev);
#  endif

 return (len);
}

/* 
   Take spark out of the spark queue on PE p and nuke the spark. Adjusts
   hd and tl pointers of the spark queue. Returns a pointer to the next
   spark in the queue.
*/
rtsSpark *
delete_from_sparkq (spark, p, dispose_too)     /* unlink and dispose spark */
rtsSpark *spark;
PEs p;
rtsBool dispose_too;
{
  rtsSpark *new_spark;

  if (spark==NULL) 
    barf("delete_from_sparkq: trying to delete NULL spark\n");

#  if defined(GRAN_CHECK)
  if ( RtsFlags.GranFlags.Debug.checkSparkQ ) {
    debugBelch("## |%p:%p| (%p)<-spark=%p->(%p) <-(%p)\n",
	    pending_sparks_hd, pending_sparks_tl,
	    spark->prev, spark, spark->next, 
	    (spark->next==NULL ? 0 : spark->next->prev));
  }
#  endif

  if (spark->prev==NULL) {
    /* spark is first spark of queue => adjust hd pointer */
    ASSERT(pending_sparks_hds[p]==spark);
    pending_sparks_hds[p] = spark->next;
  } else {
    spark->prev->next = spark->next;
  }
  if (spark->next==NULL) {
    ASSERT(pending_sparks_tls[p]==spark);
    /* spark is first spark of queue => adjust tl pointer */
    pending_sparks_tls[p] = spark->prev;
  } else {
    spark->next->prev = spark->prev;
  }
  new_spark = spark->next;
  
#  if defined(GRAN_CHECK)
  if ( RtsFlags.GranFlags.Debug.checkSparkQ ) {
    debugBelch("## |%p:%p| (%p)<-spark=%p->(%p) <-(%p); spark=%p will be deleted NOW \n",
	    pending_sparks_hd, pending_sparks_tl,
	    spark->prev, spark, spark->next, 
	    (spark->next==NULL ? 0 : spark->next->prev), spark);
  }
#  endif

  if (dispose_too)
    disposeSpark(spark);
                  
  return new_spark;
}

/* Mark all nodes pointed to by sparks in the spark queues (for GC) */
void
markSparkQueue(void)
{ 
  StgClosure *MarkRoot(StgClosure *root); // prototype
  PEs p;
  rtsSpark *sp;

  for (p=0; p<RtsFlags.GranFlags.proc; p++)
    for (sp=pending_sparks_hds[p]; sp!=NULL; sp=sp->next) {
      ASSERT(sp->node!=NULL);
      ASSERT(LOOKS_LIKE_GHC_INFO(sp->node->header.info));
      // ToDo?: statistics gathering here (also for GUM!)
      sp->node = (StgClosure *)MarkRoot(sp->node);
    }

  IF_DEBUG(gc,
	   debugBelch("markSparkQueue: spark statistics at start of GC:");
	   print_sparkq_stats());
}

void
print_spark(spark)
rtsSpark *spark;
{ 
  char str[16];

  if (spark==NULL) {
    debugBelch("Spark: NIL\n");
    return;
  } else {
    sprintf(str,
	    ((spark->node==NULL) ? "______" : "%#6lx"), 
	    stgCast(StgPtr,spark->node));

    debugBelch("Spark: Node %8s, Name %#6x, Global %5s, Creator %5x, Prev %6p, Next %6p\n",
	    str, spark->name, 
            ((spark->global)==rtsTrue?"True":"False"), spark->creator, 
            spark->prev, spark->next);
  }
}

void
print_sparkq(proc)
PEs proc;
// rtsSpark *hd;
{
  rtsSpark *x = pending_sparks_hds[proc];

  debugBelch("Spark Queue of PE %d with root at %p:\n", proc, x);
  for (; x!=(rtsSpark*)NULL; x=x->next) {
    print_spark(x);
  }
}

/* 
   Print a statistics of all spark queues.
*/
void
print_sparkq_stats(void)
{
  PEs p;

  debugBelch("SparkQs: [");
  for (p=0; p<RtsFlags.GranFlags.proc; p++)
    debugBelch(", PE %d: %d", p, spark_queue_len(p));
  debugBelch("\n");
}

#endif
