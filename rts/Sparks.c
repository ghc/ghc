/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2008
 *
 * Sparking support for PARALLEL_HASKELL and THREADED_RTS versions of the RTS.
 *
 * The implementation uses Double-Ended Queues with lock-free access
 * (thereby often called "deque") as described in
 *
 * D.Chase and Y.Lev, Dynamic Circular Work-Stealing Deque.
 * SPAA'05, July 2005, Las Vegas, USA.
 * ACM 1-58113-986-1/05/0007
 *
 * Author: Jost Berthold MSRC 07-09/2008
 *
 * The DeQue is held as a circular array with known length. Positions
 * of top (read-end) and bottom (write-end) always increase, and the
 * array is accessed with indices modulo array-size. While this bears
 * the risk of overflow, we assume that (with 64 bit indices), a
 * program must run very long to reach that point.
 * 
 * The write end of the queue (position bottom) can only be used with
 * mutual exclusion, i.e. by exactly one caller at a time.  At this
 * end, new items can be enqueued using pushBottom()/newSpark(), and
 * removed using popBottom()/reclaimSpark() (the latter implying a cas
 * synchronisation with potential concurrent readers for the case of
 * just one element).
 * 
 * Multiple readers can steal()/findSpark() from the read end
 * (position top), and are synchronised without a lock, based on a cas
 * of the top position. One reader wins, the others return NULL for a
 * failure.
 * 
 * Both popBottom and steal also return NULL when the queue is empty.
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

#include "SMP.h" // for cas

#include "Sparks.h"

#if defined(THREADED_RTS) || defined(PARALLEL_HASKELL)

/* internal helpers ... */

static StgWord
roundUp2(StgWord val)
{
  StgWord rounded = 1;

  /* StgWord is unsigned anyway, only catch 0 */
  if (val == 0) {
    barf("DeQue,roundUp2: invalid size 0 requested");
  }
  /* at least 1 bit set, shift up to its place */
  do {
    rounded = rounded << 1;
  } while (0 != (val = val>>1));
  return rounded;
}

#define CASTOP(addr,old,new) ((old) == cas(((StgPtr)addr),(old),(new)))

/* -----------------------------------------------------------------------------
 * 
 * Initialising spark pools.
 *
 * -------------------------------------------------------------------------- */

/* constructor */
static SparkPool*
initPool(StgWord size)
{
  StgWord realsize; 
  SparkPool *q;

  realsize = roundUp2(size); /* to compute modulo as a bitwise & */

  q = (SparkPool*) stgMallocBytes(sizeof(SparkPool),   /* admin fields */
			      "newSparkPool");
  q->elements = (StgClosurePtr*) 
                stgMallocBytes(realsize * sizeof(StgClosurePtr), /* dataspace */
			       "newSparkPool:data space");
  q->top=0;
  q->bottom=0;
  q->topBound=0; /* read by writer, updated each time top is read */

  q->size = realsize;  /* power of 2 */
  q->moduloSize = realsize - 1; /* n % size == n & moduloSize  */

  ASSERT_SPARK_POOL_INVARIANTS(q); 
  return q;
}

void
initSparkPools( void )
{
#ifdef THREADED_RTS
    /* walk over the capabilities, allocating a spark pool for each one */
    nat i;
    for (i = 0; i < n_capabilities; i++) {
      capabilities[i].sparks = initPool(RtsFlags.ParFlags.maxLocalSparks);
    }
#else
    /* allocate a single spark pool */
    MainCapability->sparks = initPool(RtsFlags.ParFlags.maxLocalSparks);
#endif
}

void
freeSparkPool (SparkPool *pool)
{
  /* should not interfere with concurrent findSpark() calls! And
     nobody should use the pointer any more. We cross our fingers...*/
  stgFree(pool->elements);
  stgFree(pool);
}

/* -----------------------------------------------------------------------------
 * 
 * reclaimSpark: remove a spark from the write end of the queue.
 * Returns the removed spark, and NULL if a race is lost or the pool
 * empty.
 *
 * If only one spark is left in the pool, we synchronise with
 * concurrently stealing threads by using cas to modify the top field.
 * This routine should NEVER be called by a task which does not own
 * the capability. Can this be checked here?
 *
 * -------------------------------------------------------------------------- */

StgClosure *
reclaimSpark (SparkPool *deque)
{
  /* also a bit tricky, has to avoid concurrent steal() calls by
     accessing top with cas, when there is only one element left */
  StgWord t, b;
  StgClosurePtr* pos;
  long  currSize;
  StgClosurePtr removed;

  ASSERT_SPARK_POOL_INVARIANTS(deque); 

  b = deque->bottom;
  /* "decrement b as a test, see what happens" */
  deque->bottom = --b; 
  pos = (deque->elements) + (b & (deque->moduloSize));
  t = deque->top; /* using topBound would give an *upper* bound, we
		     need a lower bound. We use the real top here, but
		     can update the topBound value */
  deque->topBound = t;
  currSize = b - t;
  if (currSize < 0) { /* was empty before decrementing b, set b
			 consistently and abort */
    deque->bottom = t;
    return NULL;
  }
  removed = *pos;
  if (currSize > 0) { /* no danger, still elements in buffer after b-- */
    return removed;
  } 
  /* otherwise, has someone meanwhile stolen the same (last) element?
     Check and increment top value to know  */
  if ( !(CASTOP(&(deque->top),t,t+1)) ) {
    removed = NULL; /* no success, but continue adjusting bottom */
  }
  deque->bottom = t+1; /* anyway, empty now. Adjust bottom consistently. */
  deque->topBound = t+1; /* ...and cached top value as well */

  ASSERT_SPARK_POOL_INVARIANTS(deque); 

  return removed;
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

static StgClosurePtr
steal(SparkPool *deque)
{
  StgClosurePtr* pos;
  StgClosurePtr* arraybase;
  StgWord sz;
  StgClosurePtr stolen;
  StgWord b,t; 

  ASSERT_SPARK_POOL_INVARIANTS(deque); 

  b = deque->bottom;
  t = deque->top;
  if (b - t <= 0 ) { 
    return NULL; /* already looks empty, abort */
  }

  /* now access array, see pushBottom() */
  arraybase = deque->elements;
  sz = deque->moduloSize;
  pos = arraybase + (t & sz);  
  stolen = *pos;

  /* now decide whether we have won */
  if ( !(CASTOP(&(deque->top),t,t+1)) ) {
      /* lost the race, someon else has changed top in the meantime */
      return NULL;
  }  /* else: OK, top has been incremented by the cas call */

  ASSERT_SPARK_POOL_INVARIANTS(deque); 
  /* return stolen element */
  return stolen;
}

StgClosure *
tryStealSpark (SparkPool *pool)
{
  StgClosure *stolen;

  do { 
      stolen = steal(pool);
  } while (stolen != NULL && !closure_SHOULD_SPARK(stolen));

  return stolen;
}


/* -----------------------------------------------------------------------------
 * 
 * "guesses" whether a deque is empty. Can return false negatives in
 *  presence of concurrent steal() calls, and false positives in
 *  presence of a concurrent pushBottom().
 *
 * -------------------------------------------------------------------------- */

rtsBool
looksEmpty(SparkPool* deque)
{
  StgWord t = deque->top;
  StgWord b = deque->bottom;
  /* try to prefer false negatives by reading top first */
  return (b - t <= 0);
  /* => array is *never* completely filled, always 1 place free! */
}

/* -----------------------------------------------------------------------------
 * 
 * Turn a spark into a real thread
 *
 * -------------------------------------------------------------------------- */

void
createSparkThread (Capability *cap, StgClosure *p)
{
    StgTSO *tso;

    tso = createGenThread (cap, RtsFlags.GcFlags.initialStkSize, p);
    appendToRunQueue(cap,tso);
    cap->sparks_converted++;
}

/* -----------------------------------------------------------------------------
 * 
 * Create a new spark
 *
 * -------------------------------------------------------------------------- */

#define DISCARD_NEW

/* enqueue an element. Should always succeed by resizing the array
   (not implemented yet, silently fails in that case). */
static void
pushBottom (SparkPool* deque, StgClosurePtr elem)
{
  StgWord t;
  StgClosurePtr* pos;
  StgWord sz = deque->moduloSize; 
  StgWord b = deque->bottom;

  ASSERT_SPARK_POOL_INVARIANTS(deque); 

  /* we try to avoid reading deque->top (accessed by all) and use
     deque->topBound (accessed only by writer) instead. 
     This is why we do not just call empty(deque) here.
  */
  t = deque->topBound;
  if ( b - t >= sz ) { /* nota bene: sz == deque->size - 1, thus ">=" */
    /* could be full, check the real top value in this case */
    t = deque->top;
    deque->topBound = t;
    if (b - t >= sz) { /* really no space left :-( */
      /* reallocate the array, copying the values. Concurrent steal()s
	 will in the meantime use the old one and modify only top.
	 This means: we cannot safely free the old space! Can keep it
	 on a free list internally here...

	 Potential bug in combination with steal(): if array is
	 replaced, it is unclear which one concurrent steal operations
	 use. Must read the array base address in advance in steal().
      */
#if defined(DISCARD_NEW)
      ASSERT_SPARK_POOL_INVARIANTS(deque); 
      return; /* for now, silently fail */
#else
      /* could make room by incrementing the top position here.  In
       * this case, should use CASTOP. If this fails, someone else has
       * removed something, and new room will be available.
       */
      ASSERT_SPARK_POOL_INVARIANTS(deque); 
#endif
    }
  }
  pos = (deque->elements) + (b & sz);
  *pos = elem;
  (deque->bottom)++;

  ASSERT_SPARK_POOL_INVARIANTS(deque); 
  return;
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

    ASSERT_SPARK_POOL_INVARIANTS(pool);

    if (closure_SHOULD_SPARK(p)) {
      pushBottom(pool,p);
    }	

    cap->sparks_created++;

    ASSERT_SPARK_POOL_INVARIANTS(pool);
    return 1;
}



/* --------------------------------------------------------------------------
 * Remove all sparks from the spark queues which should not spark any
 * more.  Called after GC. We assume exclusive access to the structure
 * and replace  all sparks in the queue, see explanation below. At exit,
 * the spark pool only contains sparkable closures.
 * -------------------------------------------------------------------------- */

static void
pruneSparkQueue (Capability *cap)
{ 
    SparkPool *pool;
    StgClosurePtr spark, *elements;
    nat n, pruned_sparks; // stats only
    StgWord botInd,oldBotInd,currInd; // indices in array (always < size)
    
    PAR_TICKY_MARK_SPARK_QUEUE_START();
    
    n = 0;
    pruned_sparks = 0;
    
    pool = cap->sparks;
    
    debugTrace(DEBUG_sched,
               "markSparkQueue: current spark queue len=%d; (hd=%ld; tl=%ld)",
               sparkPoolSize(pool), pool->bottom, pool->top);
    ASSERT_SPARK_POOL_INVARIANTS(pool);

    elements = pool->elements;

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

      /* if valuable work: shift inside the pool */
      if ( closure_SHOULD_SPARK(spark) ) {
	elements[botInd] = spark; // keep entry (new address)
	botInd++;
	n++;
      } else { 
	pruned_sparks++; // discard spark
        cap->sparks_pruned++;
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
               "new spark queue len=%d; (hd=%ld; tl=%ld)",
               sparkPoolSize(pool), pool->bottom, pool->top);

    ASSERT_SPARK_POOL_INVARIANTS(pool);
}

void
pruneSparkQueues (void)
{
    nat i;
    for (i = 0; i < n_capabilities; i++) {
        pruneSparkQueue(&capabilities[i]);
    }
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

    ASSERT_SPARK_POOL_INVARIANTS(pool);

    top = pool->top;
    bottom = pool->bottom;
    sparkp = pool->elements;
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
               "traversed spark queue, len=%d; (hd=%ld; tl=%ld)",
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
