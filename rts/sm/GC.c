/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Apply.h"
#include "OSThreads.h"
#include "LdvProfile.h"
#include "Updates.h"
#include "Stats.h"
#include "Schedule.h"
#include "Sanity.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "ProfHeap.h"
#include "SchedAPI.h"
#include "Weak.h"
#include "Prelude.h"
#include "ParTicky.h"		// ToDo: move into Rts.h
#include "RtsSignals.h"
#include "STM.h"
#include "HsFFI.h"
#include "Linker.h"
#if defined(RTS_GTK_FRONTPANEL)
#include "FrontPanel.h"
#endif
#include "Trace.h"
#include "RetainerProfile.h"
#include "RaiseAsync.h"
#include "Sparks.h"

#include "GC.h"
#include "Compact.h"
#include "Evac.h"
#include "Scav.h"
#include "GCUtils.h"
#include "MarkWeak.h"

#include <string.h> // for memset()

/* -----------------------------------------------------------------------------
   Global variables
   -------------------------------------------------------------------------- */

/* STATIC OBJECT LIST.
 *
 * During GC:
 * We maintain a linked list of static objects that are still live.
 * The requirements for this list are:
 *
 *  - we need to scan the list while adding to it, in order to
 *    scavenge all the static objects (in the same way that
 *    breadth-first scavenging works for dynamic objects).
 *
 *  - we need to be able to tell whether an object is already on
 *    the list, to break loops.
 *
 * Each static object has a "static link field", which we use for
 * linking objects on to the list.  We use a stack-type list, consing
 * objects on the front as they are added (this means that the
 * scavenge phase is depth-first, not breadth-first, but that
 * shouldn't matter).  
 *
 * A separate list is kept for objects that have been scavenged
 * already - this is so that we can zero all the marks afterwards.
 *
 * An object is on the list if its static link field is non-zero; this
 * means that we have to mark the end of the list with '1', not NULL.  
 *
 * Extra notes for generational GC:
 *
 * Each generation has a static object list associated with it.  When
 * collecting generations up to N, we treat the static object lists
 * from generations > N as roots.
 *
 * We build up a static object list while collecting generations 0..N,
 * which is then appended to the static object list of generation N+1.
 */
StgClosure* static_objects;      // live static objects
StgClosure* scavenged_static_objects;   // static objects scavenged so far
#ifdef THREADED_RTS
SpinLock static_objects_sync;
#endif

/* N is the oldest generation being collected, where the generations
 * are numbered starting at 0.  A major GC (indicated by the major_gc
 * flag) is when we're collecting all generations.  We only attempt to
 * deal with static objects and GC CAFs when doing a major GC.
 */
nat N;
rtsBool major_gc;

/* Data used for allocation area sizing.
 */
static lnat g0s0_pcnt_kept = 30; // percentage of g0s0 live at last minor GC 

/* Mut-list stats */
#ifdef DEBUG
nat mutlist_MUTVARS,
    mutlist_MUTARRS,
    mutlist_MVARS,
    mutlist_OTHERS;
#endif

/* Thread-local data for each GC thread
 */
gc_thread *gc_threads = NULL;
gc_thread *gct = NULL;  // this thread's gct TODO: make thread-local

// For stats:
long copied;        // *words* copied & scavenged during this GC
long scavd_copied;  // *words* copied only during this GC

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static void mark_root               (StgClosure **root);
static void zero_static_object_list (StgClosure* first_static);
static void initialise_N            (rtsBool force_major_gc);
static void alloc_gc_threads        (void);
static void init_collected_gen      (nat g, nat threads);
static void init_uncollected_gen    (nat g, nat threads);
static void init_gc_thread          (gc_thread *t);
static void update_task_list        (void);
static void resize_generations      (void);
static void resize_nursery          (void);

#if 0 && defined(DEBUG)
static void gcCAFs                  (void);
#endif

/* -----------------------------------------------------------------------------
   The mark bitmap & stack.
   -------------------------------------------------------------------------- */

#define MARK_STACK_BLOCKS 4

bdescr *mark_stack_bdescr;
StgPtr *mark_stack;
StgPtr *mark_sp;
StgPtr *mark_splim;

// Flag and pointers used for falling back to a linear scan when the
// mark stack overflows.
rtsBool mark_stack_overflowed;
bdescr *oldgen_scan_bd;
StgPtr  oldgen_scan;

/* -----------------------------------------------------------------------------
   GarbageCollect: the main entry point to the garbage collector.

   Locks held: all capabilities are held throughout GarbageCollect().
   -------------------------------------------------------------------------- */

void
GarbageCollect ( rtsBool force_major_gc )
{
  bdescr *bd;
  step *stp;
  lnat live, allocated;
  lnat oldgen_saved_blocks = 0;
  nat n_threads; // number of threads participating in GC

  nat g, s, t;

#ifdef PROFILING
  CostCentreStack *prev_CCS;
#endif

  ACQUIRE_SM_LOCK;

  debugTrace(DEBUG_gc, "starting GC");

#if defined(RTS_USER_SIGNALS)
  if (RtsFlags.MiscFlags.install_signal_handlers) {
    // block signals
    blockUserSignals();
  }
#endif

  // tell the STM to discard any cached closures it's hoping to re-use
  stmPreGCHook();

  // tell the stats department that we've started a GC 
  stat_startGC();

#ifdef DEBUG
  // check for memory leaks if DEBUG is on 
  memInventory();
#endif

#ifdef DEBUG
  mutlist_MUTVARS = 0;
  mutlist_MUTARRS = 0;
  mutlist_OTHERS = 0;
#endif

  // attribute any costs to CCS_GC 
#ifdef PROFILING
  prev_CCS = CCCS;
  CCCS = CCS_GC;
#endif

  /* Approximate how much we allocated.  
   * Todo: only when generating stats? 
   */
  allocated = calcAllocated();

  /* Figure out which generation to collect
   */
  initialise_N(force_major_gc);

  /* Allocate + initialise the gc_thread structures.
   */
  alloc_gc_threads();

  /* How many threads will be participating in this GC?
   * We don't try to parallelise minor GC.
   */
#if defined(THREADED_RTS)
  if (N == 0) {
      n_threads = 1;
  } else {
      n_threads = RtsFlags.ParFlags.gcThreads;
  }
#else
  n_threads = 1;
#endif

#ifdef RTS_GTK_FRONTPANEL
  if (RtsFlags.GcFlags.frontpanel) {
      updateFrontPanelBeforeGC(N);
  }
#endif

  // check stack sanity *before* GC (ToDo: check all threads) 
  IF_DEBUG(sanity, checkFreeListSanity());

  /* Initialise the static object lists
   */
  static_objects = END_OF_STATIC_LIST;
  scavenged_static_objects = END_OF_STATIC_LIST;
#ifdef THREADED_RTS
  initSpinLock(&static_objects_sync);
#endif

  // Initialise all the generations/steps that we're collecting.
  for (g = 0; g <= N; g++) {
      init_collected_gen(g,n_threads);
  }
  
  // Initialise all the generations/steps that we're *not* collecting.
  for (g = N+1; g < RtsFlags.GcFlags.generations; g++) {
      init_uncollected_gen(g,n_threads);
  }

  /* Allocate a mark stack if we're doing a major collection.
   */
  if (major_gc) {
      mark_stack_bdescr = allocGroup(MARK_STACK_BLOCKS);
      mark_stack = (StgPtr *)mark_stack_bdescr->start;
      mark_sp    = mark_stack;
      mark_splim = mark_stack + (MARK_STACK_BLOCKS * BLOCK_SIZE_W);
  } else {
      mark_stack_bdescr = NULL;
  }

  // Initialise all our gc_thread structures
  for (t = 0; t < n_threads; t++) {
      init_gc_thread(&gc_threads[t]);
  }

  // Initialise stats
  copied = 0;
  scavd_copied = 0;

  // start threads etc.
  // For now, we just have one thread, and set gct to gc_threads[0]
  gct = &gc_threads[0];

  /* -----------------------------------------------------------------------
   * follow all the roots that we know about:
   *   - mutable lists from each generation > N
   * we want to *scavenge* these roots, not evacuate them: they're not
   * going to move in this GC.
   * Also do them in reverse generation order, for the usual reason:
   * namely to reduce the likelihood of spurious old->new pointers.
   */
  { 
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      generations[g].saved_mut_list = generations[g].mut_list;
      generations[g].mut_list = allocBlock(); 
        // mut_list always has at least one block.
    }
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      scavenge_mutable_list(&generations[g]);
    }
  }

  // follow roots from the CAF list (used by GHCi)
  gct->evac_gen = 0;
  markCAFs(mark_root);

  // follow all the roots that the application knows about.
  gct->evac_gen = 0;
  GetRoots(mark_root);

  // Mark the weak pointer list, and prepare to detect dead weak pointers.
  markWeakPtrList();
  initWeakForGC();

  // Mark the stable pointer table.
  markStablePtrTable(mark_root);

  /* -------------------------------------------------------------------------
   * Repeatedly scavenge all the areas we know about until there's no
   * more scavenging to be done.
   */
  { 
    rtsBool flag;
  loop:
    flag = rtsFalse;

    scavenge_loop();

    // if any blackholes are alive, make the threads that wait on
    // them alive too.
    if (traverseBlackholeQueue())
	flag = rtsTrue;

    if (flag) { goto loop; }

    // must be last...  invariant is that everything is fully
    // scavenged at this point.
    if (traverseWeakPtrList()) { // returns rtsTrue if evaced something 
      goto loop;
    }
  }

  // Update pointers from the Task list
  update_task_list();

  // Now see which stable names are still alive.
  gcStablePtrTable();

#ifdef PROFILING
  // We call processHeapClosureForDead() on every closure destroyed during
  // the current garbage collection, so we invoke LdvCensusForDead().
  if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV
      || RtsFlags.ProfFlags.bioSelector != NULL)
    LdvCensusForDead(N);
#endif

  // NO MORE EVACUATION AFTER THIS POINT!
  // Finally: compaction of the oldest generation.
  if (major_gc && oldest_gen->steps[0].is_compacted) {
      // save number of blocks for stats
      oldgen_saved_blocks = oldest_gen->steps[0].n_old_blocks;
      compact();
  }

  IF_DEBUG(sanity, checkGlobalTSOList(rtsFalse));

  // Two-space collector: free the old to-space.
  // g0s0->old_blocks is the old nursery
  // g0s0->blocks is to-space from the previous GC
  if (RtsFlags.GcFlags.generations == 1) {
      if (g0s0->blocks != NULL) {
	  freeChain(g0s0->blocks);
	  g0s0->blocks = NULL;
      }
  }

  // For each workspace, in each thread:
  //    * clear the BF_EVACUATED flag from each copied block
  //    * move the copied blocks to the step
  {
      gc_thread *thr;
      step_workspace *ws;
      bdescr *prev;

      for (t = 0; t < n_threads; t++) {
	  thr = &gc_threads[t];

	  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	      for (s = 0; s < generations[g].n_steps; s++) {
		  ws = &thr->steps[g][s];
		  if (g==0 && s==0) continue;

		  ASSERT( ws->scan_bd == ws->todo_bd );
		  ASSERT( ws->scan_bd ? ws->scan == ws->scan_bd->free : 1 );

		  // Push the final block
		  if (ws->scan_bd) { push_scan_block(ws->scan_bd, ws); }

		  // update stats: we haven't counted the block at the
		  // front of the scavd_list yet.
		  scavd_copied += ws->scavd_list->free - ws->scavd_list->start;

		  ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks);

		  prev = ws->scavd_list;
		  for (bd = ws->scavd_list; bd != NULL; bd = bd->link) {
		      bd->flags &= ~BF_EVACUATED;	 // now from-space 
		      prev = bd;
		  }
		  prev->link = ws->stp->blocks;
		  ws->stp->blocks = ws->scavd_list;
		  ws->stp->n_blocks += ws->n_scavd_blocks;
		  ASSERT(countBlocks(ws->stp->blocks) == ws->stp->n_blocks);
	      }
	  }
      }
  }

  // Two-space collector: swap the semi-spaces around.
  // Currently: g0s0->old_blocks is the old nursery
  //            g0s0->blocks is to-space from this GC
  // We want these the other way around.
  if (RtsFlags.GcFlags.generations == 1) {
      bdescr *nursery_blocks = g0s0->old_blocks;
      nat n_nursery_blocks = g0s0->n_old_blocks;
      g0s0->old_blocks = g0s0->blocks;
      g0s0->n_old_blocks = g0s0->n_blocks;
      g0s0->blocks = nursery_blocks;
      g0s0->n_blocks = n_nursery_blocks;
  }

  /* run through all the generations/steps and tidy up 
   */
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {

    if (g <= N) {
      generations[g].collections++; // for stats 
    }

    // Count the mutable list as bytes "copied" for the purposes of
    // stats.  Every mutable list is copied during every GC.
    if (g > 0) {
	nat mut_list_size = 0;
	for (bd = generations[g].mut_list; bd != NULL; bd = bd->link) {
	    mut_list_size += bd->free - bd->start;
	}
	copied +=  mut_list_size;

	debugTrace(DEBUG_gc,
		   "mut_list_size: %lu (%d vars, %d arrays, %d MVARs, %d others)",
		   (unsigned long)(mut_list_size * sizeof(W_)),
		   mutlist_MUTVARS, mutlist_MUTARRS, mutlist_MVARS, mutlist_OTHERS);
    }

    for (s = 0; s < generations[g].n_steps; s++) {
      bdescr *next;
      stp = &generations[g].steps[s];

      // for generations we collected... 
      if (g <= N) {

	/* free old memory and shift to-space into from-space for all
	 * the collected steps (except the allocation area).  These
	 * freed blocks will probaby be quickly recycled.
	 */
	if (!(g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1)) {
	    if (stp->is_compacted)
            {
		// for a compacted step, just shift the new to-space
		// onto the front of the now-compacted existing blocks.
		for (bd = stp->blocks; bd != NULL; bd = bd->link) {
		    bd->flags &= ~BF_EVACUATED;  // now from-space 
		}
		// tack the new blocks on the end of the existing blocks
		if (stp->old_blocks != NULL) {
		    for (bd = stp->old_blocks; bd != NULL; bd = next) {
			// NB. this step might not be compacted next
			// time, so reset the BF_COMPACTED flags.
			// They are set before GC if we're going to
			// compact.  (search for BF_COMPACTED above).
			bd->flags &= ~BF_COMPACTED;
			next = bd->link;
			if (next == NULL) {
			    bd->link = stp->blocks;
			}
		    }
		    stp->blocks = stp->old_blocks;
		}
		// add the new blocks to the block tally
		stp->n_blocks += stp->n_old_blocks;
		ASSERT(countBlocks(stp->blocks) == stp->n_blocks);
	    }
	    else // not copacted
	    {
		freeChain(stp->old_blocks);
	    }
	    stp->old_blocks = NULL;
	    stp->n_old_blocks = 0;
	}

	/* LARGE OBJECTS.  The current live large objects are chained on
	 * scavenged_large, having been moved during garbage
	 * collection from large_objects.  Any objects left on
	 * large_objects list are therefore dead, so we free them here.
	 */
	for (bd = stp->large_objects; bd != NULL; bd = next) {
	  next = bd->link;
	  freeGroup(bd);
	  bd = next;
	}

	// update the count of blocks used by large objects
	for (bd = stp->scavenged_large_objects; bd != NULL; bd = bd->link) {
	  bd->flags &= ~BF_EVACUATED;
	}
	stp->large_objects  = stp->scavenged_large_objects;
	stp->n_large_blocks = stp->n_scavenged_large_blocks;

      }
      else // for older generations... 
      {
	/* For older generations, we need to append the
	 * scavenged_large_object list (i.e. large objects that have been
	 * promoted during this GC) to the large_object list for that step.
	 */
	for (bd = stp->scavenged_large_objects; bd; bd = next) {
	  next = bd->link;
	  bd->flags &= ~BF_EVACUATED;
	  dbl_link_onto(bd, &stp->large_objects);
	}

	// add the new blocks we promoted during this GC 
	stp->n_large_blocks += stp->n_scavenged_large_blocks;
      }
    }
  }

  // update the max size of older generations after a major GC
  resize_generations();
  
  // Guess the amount of live data for stats.
  live = calcLive();

  // Free the small objects allocated via allocate(), since this will
  // all have been copied into G0S1 now.  
  if (RtsFlags.GcFlags.generations > 1) {
      if (g0s0->blocks != NULL) {
          freeChain(g0s0->blocks);
          g0s0->blocks = NULL;
      }
      g0s0->n_blocks = 0;
  }
  alloc_blocks = 0;
  alloc_blocks_lim = RtsFlags.GcFlags.minAllocAreaSize;

  // Start a new pinned_object_block
  pinned_object_block = NULL;

  // Free the mark stack.
  if (mark_stack_bdescr != NULL) {
      freeGroup(mark_stack_bdescr);
  }

  // Free any bitmaps.
  for (g = 0; g <= N; g++) {
      for (s = 0; s < generations[g].n_steps; s++) {
	  stp = &generations[g].steps[s];
	  if (stp->bitmap != NULL) {
	      freeGroup(stp->bitmap);
	      stp->bitmap = NULL;
	  }
      }
  }

  resize_nursery();

 // mark the garbage collected CAFs as dead 
#if 0 && defined(DEBUG) // doesn't work at the moment 
  if (major_gc) { gcCAFs(); }
#endif
  
#ifdef PROFILING
  // resetStaticObjectForRetainerProfiling() must be called before
  // zeroing below.
  resetStaticObjectForRetainerProfiling();
#endif

  // zero the scavenged static object list 
  if (major_gc) {
    zero_static_object_list(scavenged_static_objects);
  }

  // Reset the nursery
  resetNurseries();

  // start any pending finalizers 
  RELEASE_SM_LOCK;
  scheduleFinalizers(last_free_capability, old_weak_ptr_list);
  ACQUIRE_SM_LOCK;
  
  // send exceptions to any threads which were about to die 
  RELEASE_SM_LOCK;
  resurrectThreads(resurrected_threads);
  ACQUIRE_SM_LOCK;

  // Update the stable pointer hash table.
  updateStablePtrTable(major_gc);

  // check sanity after GC 
  IF_DEBUG(sanity, checkSanity());

  // extra GC trace info 
  IF_DEBUG(gc, statDescribeGens());

#ifdef DEBUG
  // symbol-table based profiling 
  /*  heapCensus(to_blocks); */ /* ToDo */
#endif

  // restore enclosing cost centre 
#ifdef PROFILING
  CCCS = prev_CCS;
#endif

#ifdef DEBUG
  // check for memory leaks if DEBUG is on 
  memInventory();
#endif

#ifdef RTS_GTK_FRONTPANEL
  if (RtsFlags.GcFlags.frontpanel) {
      updateFrontPanelAfterGC( N, live );
  }
#endif

  // ok, GC over: tell the stats department what happened. 
  stat_endGC(allocated, live, copied, scavd_copied, N);

#if defined(RTS_USER_SIGNALS)
  if (RtsFlags.MiscFlags.install_signal_handlers) {
    // unblock signals again
    unblockUserSignals();
  }
#endif

  RELEASE_SM_LOCK;
}

/* ---------------------------------------------------------------------------
   Where are the roots that we know about?

        - all the threads on the runnable queue
        - all the threads on the blocked queue
        - all the threads on the sleeping queue
	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   ------------------------------------------------------------------------ */

/* This has to be protected either by the scheduler monitor, or by the
	garbage collection monitor (probably the latter).
	KH @ 25/10/99
*/

void
GetRoots( evac_fn evac )
{
    nat i;
    Capability *cap;
    Task *task;

#if defined(GRAN)
    for (i=0; i<=RtsFlags.GranFlags.proc; i++) {
	if ((run_queue_hds[i] != END_TSO_QUEUE) && ((run_queue_hds[i] != NULL)))
	    evac((StgClosure **)&run_queue_hds[i]);
	if ((run_queue_tls[i] != END_TSO_QUEUE) && ((run_queue_tls[i] != NULL)))
	    evac((StgClosure **)&run_queue_tls[i]);
	
	if ((blocked_queue_hds[i] != END_TSO_QUEUE) && ((blocked_queue_hds[i] != NULL)))
	    evac((StgClosure **)&blocked_queue_hds[i]);
	if ((blocked_queue_tls[i] != END_TSO_QUEUE) && ((blocked_queue_tls[i] != NULL)))
	    evac((StgClosure **)&blocked_queue_tls[i]);
	if ((ccalling_threadss[i] != END_TSO_QUEUE) && ((ccalling_threadss[i] != NULL)))
	    evac((StgClosure **)&ccalling_threads[i]);
    }

    markEventQueue();

#else /* !GRAN */

    for (i = 0; i < n_capabilities; i++) {
	cap = &capabilities[i];
	evac((StgClosure **)(void *)&cap->run_queue_hd);
	evac((StgClosure **)(void *)&cap->run_queue_tl);
#if defined(THREADED_RTS)
	evac((StgClosure **)(void *)&cap->wakeup_queue_hd);
	evac((StgClosure **)(void *)&cap->wakeup_queue_tl);
#endif
	for (task = cap->suspended_ccalling_tasks; task != NULL; 
	     task=task->next) {
	    debugTrace(DEBUG_sched,
		       "evac'ing suspended TSO %lu", (unsigned long)task->suspended_tso->id);
	    evac((StgClosure **)(void *)&task->suspended_tso);
	}

    }
    

#if !defined(THREADED_RTS)
    evac((StgClosure **)(void *)&blocked_queue_hd);
    evac((StgClosure **)(void *)&blocked_queue_tl);
    evac((StgClosure **)(void *)&sleeping_queue);
#endif 
#endif

    // evac((StgClosure **)&blackhole_queue);

#if defined(THREADED_RTS) || defined(PARALLEL_HASKELL) || defined(GRAN)
    markSparkQueue(evac);
#endif
    
#if defined(RTS_USER_SIGNALS)
    // mark the signal handlers (signals should be already blocked)
    markSignalHandlers(evac);
#endif
}

/* -----------------------------------------------------------------------------
   isAlive determines whether the given closure is still alive (after
   a garbage collection) or not.  It returns the new address of the
   closure if it is alive, or NULL otherwise.

   NOTE: Use it before compaction only!
         It untags and (if needed) retags pointers to closures.
   -------------------------------------------------------------------------- */


StgClosure *
isAlive(StgClosure *p)
{
  const StgInfoTable *info;
  bdescr *bd;
  StgWord tag;
  StgClosure *q;

  while (1) {
    /* The tag and the pointer are split, to be merged later when needed. */
    tag = GET_CLOSURE_TAG(p);
    q = UNTAG_CLOSURE(p);

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));
    info = get_itbl(q);

    // ignore static closures 
    //
    // ToDo: for static closures, check the static link field.
    // Problem here is that we sometimes don't set the link field, eg.
    // for static closures with an empty SRT or CONSTR_STATIC_NOCAFs.
    //
    if (!HEAP_ALLOCED(q)) {
	return p;
    }

    // ignore closures in generations that we're not collecting. 
    bd = Bdescr((P_)q);
    if (bd->gen_no > N) {
	return p;
    }

    // if it's a pointer into to-space, then we're done
    if (bd->flags & BF_EVACUATED) {
	return p;
    }

    // large objects use the evacuated flag
    if (bd->flags & BF_LARGE) {
	return NULL;
    }

    // check the mark bit for compacted steps
    if ((bd->flags & BF_COMPACTED) && is_marked((P_)q,bd)) {
	return p;
    }

    switch (info->type) {

    case IND:
    case IND_STATIC:
    case IND_PERM:
    case IND_OLDGEN:		// rely on compatible layout with StgInd 
    case IND_OLDGEN_PERM:
      // follow indirections 
      p = ((StgInd *)q)->indirectee;
      continue;

    case EVACUATED:
      // alive! 
      return ((StgEvacuated *)q)->evacuee;

    case TSO:
      if (((StgTSO *)q)->what_next == ThreadRelocated) {
	p = (StgClosure *)((StgTSO *)q)->link;
	continue;
      } 
      return NULL;

    default:
      // dead. 
      return NULL;
    }
  }
}

/* -----------------------------------------------------------------------------
   Figure out which generation to collect, initialise N and major_gc.
   -------------------------------------------------------------------------- */

static void
initialise_N (rtsBool force_major_gc)
{
    nat g;

    if (force_major_gc) {
	N = RtsFlags.GcFlags.generations - 1;
	major_gc = rtsTrue;
    } else {
	N = 0;
	for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    if (generations[g].steps[0].n_blocks +
		generations[g].steps[0].n_large_blocks
		>= generations[g].max_blocks) {
		N = g;
	    }
	}
	major_gc = (N == RtsFlags.GcFlags.generations-1);
    }
}

/* -----------------------------------------------------------------------------
   Initialise the gc_thread structures.
   -------------------------------------------------------------------------- */

static void
alloc_gc_thread (gc_thread *t, int n)
{
    nat g, s;
    step_workspace *ws;

    t->thread_index = n;
    t->free_blocks = NULL;
    t->gc_count = 0;

    init_gc_thread(t);
    
    t->steps = stgMallocBytes(RtsFlags.GcFlags.generations * 
				sizeof(step_workspace *), 
				"initialise_gc_thread");

    for (g = 0; g < RtsFlags.GcFlags.generations; g++)
    {
        t->steps[g] = stgMallocBytes(generations[g].n_steps * 
				       sizeof(step_workspace),
				       "initialise_gc_thread/2");

        for (s = 0; s < generations[g].n_steps; s++)
        {
            ws = &t->steps[g][s];
            ws->stp = &generations[g].steps[s];
            ws->gct = t;

            ws->scan_bd = NULL;
            ws->scan = NULL;

	    ws->todo_bd = NULL;
            ws->buffer_todo_bd = NULL;

	    ws->scavd_list = NULL;
	    ws->n_scavd_blocks = 0;
        }
    }
}


static void
alloc_gc_threads (void)
{
    if (gc_threads == NULL) {
#if defined(THREADED_RTS)
        nat i;

	gc_threads = stgMallocBytes (RtsFlags.ParFlags.gcThreads * 
				     sizeof(gc_thread), 
				     "alloc_gc_threads");

	for (i = 0; i < RtsFlags.ParFlags.gcThreads; i++) {
	    alloc_gc_thread(&gc_threads[i], i);
	}
#else
	gc_threads = stgMallocBytes (sizeof(gc_thread), 
				     "alloc_gc_threads");

	alloc_gc_thread(gc_threads, 0);
#endif
    }
}

/* ----------------------------------------------------------------------------
   Initialise a generation that is to be collected 
   ------------------------------------------------------------------------- */

static void
init_collected_gen (nat g, nat n_threads)
{
    nat s, t, i;
    step_workspace *ws;
    step *stp;
    bdescr *bd;

    // Throw away the current mutable list.  Invariant: the mutable
    // list always has at least one block; this means we can avoid a
    // check for NULL in recordMutable().
    if (g != 0) {
	freeChain(generations[g].mut_list);
	generations[g].mut_list = allocBlock();
	for (i = 0; i < n_capabilities; i++) {
	    freeChain(capabilities[i].mut_lists[g]);
	    capabilities[i].mut_lists[g] = allocBlock();
	}
    }

    for (s = 0; s < generations[g].n_steps; s++) {

	// generation 0, step 0 doesn't need to-space 
	if (g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1) { 
	    continue; 
	}
	
	stp = &generations[g].steps[s];
	ASSERT(stp->gen_no == g);

	// deprecate the existing blocks
	stp->old_blocks   = stp->blocks;
	stp->n_old_blocks = stp->n_blocks;
	stp->blocks       = NULL;
	stp->n_blocks     = 0;

	// we don't have any to-be-scavenged blocks yet
	stp->todos = NULL;
	stp->n_todos = 0;

	// initialise the large object queues.
	stp->scavenged_large_objects = NULL;
	stp->n_scavenged_large_blocks = 0;

	// mark the large objects as not evacuated yet 
	for (bd = stp->large_objects; bd; bd = bd->link) {
	    bd->flags &= ~BF_EVACUATED;
	}

	// for a compacted step, we need to allocate the bitmap
	if (stp->is_compacted) {
	    nat bitmap_size; // in bytes
	    bdescr *bitmap_bdescr;
	    StgWord *bitmap;
	    
	    bitmap_size = stp->n_old_blocks * BLOCK_SIZE / (sizeof(W_)*BITS_PER_BYTE);
	    
	    if (bitmap_size > 0) {
		bitmap_bdescr = allocGroup((lnat)BLOCK_ROUND_UP(bitmap_size) 
					   / BLOCK_SIZE);
		stp->bitmap = bitmap_bdescr;
		bitmap = bitmap_bdescr->start;
		
		debugTrace(DEBUG_gc, "bitmap_size: %d, bitmap: %p",
			   bitmap_size, bitmap);
		
		// don't forget to fill it with zeros!
		memset(bitmap, 0, bitmap_size);
		
		// For each block in this step, point to its bitmap from the
		// block descriptor.
		for (bd=stp->old_blocks; bd != NULL; bd = bd->link) {
		    bd->u.bitmap = bitmap;
		    bitmap += BLOCK_SIZE_W / (sizeof(W_)*BITS_PER_BYTE);
		    
		    // Also at this point we set the BF_COMPACTED flag
		    // for this block.  The invariant is that
		    // BF_COMPACTED is always unset, except during GC
		    // when it is set on those blocks which will be
		    // compacted.
		    bd->flags |= BF_COMPACTED;
		}
	    }
	}
    }

    // For each GC thread, for each step, allocate a "todo" block to
    // store evacuated objects to be scavenged, and a block to store
    // evacuated objects that do not need to be scavenged.
    for (t = 0; t < n_threads; t++) {
	for (s = 0; s < generations[g].n_steps; s++) {

	    // we don't copy objects into g0s0, unless -G0
	    if (g==0 && s==0 && RtsFlags.GcFlags.generations > 1) continue;

	    ws = &gc_threads[t].steps[g][s];

	    ws->scan_bd = NULL;
	    ws->scan = NULL;

	    ws->todo_large_objects = NULL;

	    // allocate the first to-space block; extra blocks will be
	    // chained on as necessary.
	    ws->todo_bd = NULL;
	    ws->buffer_todo_bd = NULL;
	    gc_alloc_todo_block(ws);

	    // allocate a block for "already scavenged" objects.  This goes
	    // on the front of the stp->blocks list, so it won't be
	    // traversed by the scavenging sweep.
	    ws->scavd_list = NULL;
	    ws->n_scavd_blocks = 0;
	    gc_alloc_scavd_block(ws);
	}
    }
}


/* ----------------------------------------------------------------------------
   Initialise a generation that is *not* to be collected 
   ------------------------------------------------------------------------- */

static void
init_uncollected_gen (nat g, nat threads)
{
    nat s, t, i;
    step_workspace *ws;
    step *stp;
    bdescr *bd;

    for (s = 0; s < generations[g].n_steps; s++) {
	stp = &generations[g].steps[s];
	stp->scavenged_large_objects = NULL;
	stp->n_scavenged_large_blocks = 0;
    }
    
    for (t = 0; t < threads; t++) {
	for (s = 0; s < generations[g].n_steps; s++) {
	    
	    ws = &gc_threads[t].steps[g][s];
	    stp = ws->stp;
	    
	    ws->buffer_todo_bd = NULL;
	    ws->todo_large_objects = NULL;

	    // If the block at the head of the list in this generation
	    // is less than 3/4 full, then use it as a todo block.
	    if (isPartiallyFull(stp->blocks))
	    {
		ws->todo_bd = stp->blocks;
		stp->blocks = stp->blocks->link;
		stp->n_blocks -= 1;
		ws->todo_bd->link = NULL;

		// this block is also the scan block; we must scan
		// from the current end point.
		ws->scan_bd = ws->todo_bd;
		ws->scan = ws->scan_bd->free;

		// subtract the contents of this block from the stats,
		// because we'll count the whole block later.
		copied -= ws->scan_bd->free - ws->scan_bd->start;
	    } 
	    else
	    {
		ws->scan_bd = NULL;
		ws->scan = NULL;
		ws->todo_bd = NULL;
		gc_alloc_todo_block(ws);
	    }

	    // Do the same trick for the scavd block
	    if (isPartiallyFull(stp->blocks))
	    {
		ws->scavd_list = stp->blocks;
		stp->blocks = stp->blocks->link;
		stp->n_blocks -= 1;
		ws->scavd_list->link = NULL;
		ws->n_scavd_blocks = 1;
		// subtract the contents of this block from the stats,
		// because we'll count the whole block later.
		scavd_copied -= ws->scavd_list->free - ws->scavd_list->start;
	    }
	    else
	    {
		ws->scavd_list = NULL;
		ws->n_scavd_blocks = 0;
		gc_alloc_scavd_block(ws);
	    }
	}
    }

    // Move the private mutable lists from each capability onto the
    // main mutable list for the generation.
    for (i = 0; i < n_capabilities; i++) {
	for (bd = capabilities[i].mut_lists[g]; 
	     bd->link != NULL; bd = bd->link) {
	    /* nothing */
	}
	bd->link = generations[g].mut_list;
	generations[g].mut_list = capabilities[i].mut_lists[g];
	capabilities[i].mut_lists[g] = allocBlock();
    }
}

/* -----------------------------------------------------------------------------
   Initialise a gc_thread before GC
   -------------------------------------------------------------------------- */

static void
init_gc_thread (gc_thread *t)
{
    t->evac_gen = 0;
    t->failed_to_evac = rtsFalse;
    t->eager_promotion = rtsTrue;
    t->thunk_selector_depth = 0;
}

/* -----------------------------------------------------------------------------
   Function we pass to GetRoots to evacuate roots.
   -------------------------------------------------------------------------- */

static void
mark_root(StgClosure **root)
{
  *root = evacuate(*root);
}

/* -----------------------------------------------------------------------------
   Initialising the static object & mutable lists
   -------------------------------------------------------------------------- */

static void
zero_static_object_list(StgClosure* first_static)
{
  StgClosure* p;
  StgClosure* link;
  const StgInfoTable *info;

  for (p = first_static; p != END_OF_STATIC_LIST; p = link) {
    info = get_itbl(p);
    link = *STATIC_LINK(info, p);
    *STATIC_LINK(info,p) = NULL;
  }
}

/* -----------------------------------------------------------------------------
   Reverting CAFs
   -------------------------------------------------------------------------- */

void
revertCAFs( void )
{
    StgIndStatic *c;

    for (c = (StgIndStatic *)revertible_caf_list; c != NULL; 
	 c = (StgIndStatic *)c->static_link) 
    {
	SET_INFO(c, c->saved_info);
	c->saved_info = NULL;
	// could, but not necessary: c->static_link = NULL; 
    }
    revertible_caf_list = NULL;
}

void
markCAFs( evac_fn evac )
{
    StgIndStatic *c;

    for (c = (StgIndStatic *)caf_list; c != NULL; 
	 c = (StgIndStatic *)c->static_link) 
    {
	evac(&c->indirectee);
    }
    for (c = (StgIndStatic *)revertible_caf_list; c != NULL; 
	 c = (StgIndStatic *)c->static_link) 
    {
	evac(&c->indirectee);
    }
}

/* ----------------------------------------------------------------------------
   Update the pointers from the task list

   These are treated as weak pointers because we want to allow a main
   thread to get a BlockedOnDeadMVar exception in the same way as any
   other thread.  Note that the threads should all have been retained
   by GC by virtue of being on the all_threads list, we're just
   updating pointers here.
   ------------------------------------------------------------------------- */

static void
update_task_list (void)
{
    Task *task;
    StgTSO *tso;
    for (task = all_tasks; task != NULL; task = task->all_link) {
	if (!task->stopped && task->tso) {
	    ASSERT(task->tso->bound == task);
	    tso = (StgTSO *) isAlive((StgClosure *)task->tso);
	    if (tso == NULL) {
		barf("task %p: main thread %d has been GC'd", 
#ifdef THREADED_RTS
		     (void *)task->id, 
#else
		     (void *)task,
#endif
		     task->tso->id);
	    }
	    task->tso = tso;
	}
    }
}

/* ----------------------------------------------------------------------------
   Reset the sizes of the older generations when we do a major
   collection.
  
   CURRENT STRATEGY: make all generations except zero the same size.
   We have to stay within the maximum heap size, and leave a certain
   percentage of the maximum heap size available to allocate into.
   ------------------------------------------------------------------------- */

static void
resize_generations (void)
{
    nat g;

    if (major_gc && RtsFlags.GcFlags.generations > 1) {
	nat live, size, min_alloc;
	nat max  = RtsFlags.GcFlags.maxHeapSize;
	nat gens = RtsFlags.GcFlags.generations;
	
	// live in the oldest generations
	live = oldest_gen->steps[0].n_blocks +
	    oldest_gen->steps[0].n_large_blocks;
	
	// default max size for all generations except zero
	size = stg_max(live * RtsFlags.GcFlags.oldGenFactor,
		       RtsFlags.GcFlags.minOldGenSize);
	
	// minimum size for generation zero
	min_alloc = stg_max((RtsFlags.GcFlags.pcFreeHeap * max) / 200,
			    RtsFlags.GcFlags.minAllocAreaSize);

	// Auto-enable compaction when the residency reaches a
	// certain percentage of the maximum heap size (default: 30%).
	if (RtsFlags.GcFlags.generations > 1 &&
	    (RtsFlags.GcFlags.compact ||
	     (max > 0 &&
	      oldest_gen->steps[0].n_blocks > 
	      (RtsFlags.GcFlags.compactThreshold * max) / 100))) {
	    oldest_gen->steps[0].is_compacted = 1;
//	  debugBelch("compaction: on\n", live);
	} else {
	    oldest_gen->steps[0].is_compacted = 0;
//	  debugBelch("compaction: off\n", live);
	}

	// if we're going to go over the maximum heap size, reduce the
	// size of the generations accordingly.  The calculation is
	// different if compaction is turned on, because we don't need
	// to double the space required to collect the old generation.
	if (max != 0) {
	    
	    // this test is necessary to ensure that the calculations
	    // below don't have any negative results - we're working
	    // with unsigned values here.
	    if (max < min_alloc) {
		heapOverflow();
	    }
	    
	    if (oldest_gen->steps[0].is_compacted) {
		if ( (size + (size - 1) * (gens - 2) * 2) + min_alloc > max ) {
		    size = (max - min_alloc) / ((gens - 1) * 2 - 1);
		}
	    } else {
		if ( (size * (gens - 1) * 2) + min_alloc > max ) {
		    size = (max - min_alloc) / ((gens - 1) * 2);
		}
	    }
	    
	    if (size < live) {
		heapOverflow();
	    }
	}
	
#if 0
	debugBelch("live: %d, min_alloc: %d, size : %d, max = %d\n", live,
		   min_alloc, size, max);
#endif
	
	for (g = 0; g < gens; g++) {
	    generations[g].max_blocks = size;
	}
    }
}

/* -----------------------------------------------------------------------------
   Calculate the new size of the nursery, and resize it.
   -------------------------------------------------------------------------- */

static void
resize_nursery (void)
{
    if (RtsFlags.GcFlags.generations == 1)
    {   // Two-space collector:
	nat blocks;
    
	/* set up a new nursery.  Allocate a nursery size based on a
	 * function of the amount of live data (by default a factor of 2)
	 * Use the blocks from the old nursery if possible, freeing up any
	 * left over blocks.
	 *
	 * If we get near the maximum heap size, then adjust our nursery
	 * size accordingly.  If the nursery is the same size as the live
	 * data (L), then we need 3L bytes.  We can reduce the size of the
	 * nursery to bring the required memory down near 2L bytes.
	 * 
	 * A normal 2-space collector would need 4L bytes to give the same
	 * performance we get from 3L bytes, reducing to the same
	 * performance at 2L bytes.
	 */
	blocks = g0s0->n_old_blocks;
	
	if ( RtsFlags.GcFlags.maxHeapSize != 0 &&
	     blocks * RtsFlags.GcFlags.oldGenFactor * 2 > 
	     RtsFlags.GcFlags.maxHeapSize )
	{
	    long adjusted_blocks;  // signed on purpose 
	    int pc_free; 
	    
	    adjusted_blocks = (RtsFlags.GcFlags.maxHeapSize - 2 * blocks);
	    
	    debugTrace(DEBUG_gc, "near maximum heap size of 0x%x blocks, blocks = %d, adjusted to %ld", 
		       RtsFlags.GcFlags.maxHeapSize, blocks, adjusted_blocks);
	    
	    pc_free = adjusted_blocks * 100 / RtsFlags.GcFlags.maxHeapSize;
	    if (pc_free < RtsFlags.GcFlags.pcFreeHeap) /* might even * be < 0 */
	    {
		heapOverflow();
	    }
	    blocks = adjusted_blocks;
	}
	else
	{
	    blocks *= RtsFlags.GcFlags.oldGenFactor;
	    if (blocks < RtsFlags.GcFlags.minAllocAreaSize)
	    {
		blocks = RtsFlags.GcFlags.minAllocAreaSize;
	    }
	}
	resizeNurseries(blocks);
    }
    else  // Generational collector
    {
	/* 
	 * If the user has given us a suggested heap size, adjust our
	 * allocation area to make best use of the memory available.
	 */
	if (RtsFlags.GcFlags.heapSizeSuggestion)
	{
	    long blocks;
	    nat needed = calcNeeded(); 	// approx blocks needed at next GC 
	    
	    /* Guess how much will be live in generation 0 step 0 next time.
	     * A good approximation is obtained by finding the
	     * percentage of g0s0 that was live at the last minor GC.
	     *
	     * We have an accurate figure for the amount of copied data in
	     * 'copied', but we must convert this to a number of blocks, with
	     * a small adjustment for estimated slop at the end of a block
	     * (- 10 words).
	     */
	    if (N == 0)
	    {
		g0s0_pcnt_kept = ((copied / (BLOCK_SIZE_W - 10)) * 100)
		    / countNurseryBlocks();
	    }
	    
	    /* Estimate a size for the allocation area based on the
	     * information available.  We might end up going slightly under
	     * or over the suggested heap size, but we should be pretty
	     * close on average.
	     *
	     * Formula:            suggested - needed
	     *                ----------------------------
	     *                    1 + g0s0_pcnt_kept/100
	     *
	     * where 'needed' is the amount of memory needed at the next
	     * collection for collecting all steps except g0s0.
	     */
	    blocks = 
		(((long)RtsFlags.GcFlags.heapSizeSuggestion - (long)needed) * 100) /
		(100 + (long)g0s0_pcnt_kept);
	    
	    if (blocks < (long)RtsFlags.GcFlags.minAllocAreaSize) {
		blocks = RtsFlags.GcFlags.minAllocAreaSize;
	    }
	    
	    resizeNurseries((nat)blocks);
	}
	else
	{
	    // we might have added extra large blocks to the nursery, so
	    // resize back to minAllocAreaSize again.
	    resizeNurseriesFixed(RtsFlags.GcFlags.minAllocAreaSize);
	}
    }
}

/* -----------------------------------------------------------------------------
   Sanity code for CAF garbage collection.

   With DEBUG turned on, we manage a CAF list in addition to the SRT
   mechanism.  After GC, we run down the CAF list and blackhole any
   CAFs which have been garbage collected.  This means we get an error
   whenever the program tries to enter a garbage collected CAF.

   Any garbage collected CAFs are taken off the CAF list at the same
   time. 
   -------------------------------------------------------------------------- */

#if 0 && defined(DEBUG)

static void
gcCAFs(void)
{
  StgClosure*  p;
  StgClosure** pp;
  const StgInfoTable *info;
  nat i;

  i = 0;
  p = caf_list;
  pp = &caf_list;

  while (p != NULL) {
    
    info = get_itbl(p);

    ASSERT(info->type == IND_STATIC);

    if (STATIC_LINK(info,p) == NULL) {
	debugTrace(DEBUG_gccafs, "CAF gc'd at 0x%04lx", (long)p);
	// black hole it 
	SET_INFO(p,&stg_BLACKHOLE_info);
	p = STATIC_LINK2(info,p);
	*pp = p;
    }
    else {
      pp = &STATIC_LINK2(info,p);
      p = *pp;
      i++;
    }

  }

  debugTrace(DEBUG_gccafs, "%d CAFs live", i); 
}
#endif
