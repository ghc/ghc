/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

// #include "PosixSource.h"
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
#include "Papi.h"

#include "GC.h"
#include "GCThread.h"
#include "Compact.h"
#include "Evac.h"
#include "Scav.h"
#include "GCUtils.h"
#include "MarkWeak.h"
#include "Sparks.h"
#include "Sweep.h"

#include <string.h> // for memset()
#include <unistd.h>

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
gc_thread **gc_threads = NULL;
// gc_thread *gct = NULL;  // this thread's gct TODO: make thread-local

// Number of threads running in *this* GC.  Affects how many
// step->todos[] lists we have to look in to find work.
nat n_gc_threads;

// For stats:
long copied;        // *words* copied & scavenged during this GC

#ifdef THREADED_RTS
SpinLock recordMutableGen_sync;
#endif

DECLARE_GCT

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static void mark_root               (void *user, StgClosure **root);
static void zero_static_object_list (StgClosure* first_static);
static nat  initialise_N            (rtsBool force_major_gc);
static void alloc_gc_threads        (void);
static void init_collected_gen      (nat g, nat threads);
static void init_uncollected_gen    (nat g, nat threads);
static void init_gc_thread          (gc_thread *t);
static void update_task_list        (void);
static void resize_generations      (void);
static void resize_nursery          (void);
static void start_gc_threads        (void);
static void scavenge_until_all_done (void);
static nat  inc_running             (void);
static nat  dec_running             (void);
static void wakeup_gc_threads       (nat n_threads);
static void shutdown_gc_threads     (nat n_threads);

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
  lnat live, allocated, max_copied, avg_copied, slop;
  gc_thread *saved_gct;
  nat g, s, t, n;

  // necessary if we stole a callee-saves register for gct:
  saved_gct = gct;

#ifdef PROFILING
  CostCentreStack *prev_CCS;
#endif

  ACQUIRE_SM_LOCK;

#if defined(RTS_USER_SIGNALS)
  if (RtsFlags.MiscFlags.install_signal_handlers) {
    // block signals
    blockUserSignals();
  }
#endif

  ASSERT(sizeof(step_workspace) == 16 * sizeof(StgWord));
  // otherwise adjust the padding in step_workspace.

  // tell the stats department that we've started a GC 
  stat_startGC();

  // tell the STM to discard any cached closures it's hoping to re-use
  stmPreGCHook();

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
  n = initialise_N(force_major_gc);

  /* Allocate + initialise the gc_thread structures.
   */
  alloc_gc_threads();

  /* Start threads, so they can be spinning up while we finish initialisation.
   */
  start_gc_threads();

  /* How many threads will be participating in this GC?
   * We don't try to parallelise minor GC, or mark/compact/sweep GC.
   */
#if defined(THREADED_RTS)
  if (n < (4*1024*1024 / BLOCK_SIZE) || oldest_gen->steps[0].mark) {
      n_gc_threads = 1;
  } else {
      n_gc_threads = RtsFlags.ParFlags.gcThreads;
  }
#else
  n_gc_threads = 1;
#endif
  trace(TRACE_gc|DEBUG_gc, "GC (gen %d): %d KB to collect, %ld MB in use, using %d thread(s)",
        N, n * (BLOCK_SIZE / 1024), mblocks_allocated, n_gc_threads);

#ifdef RTS_GTK_FRONTPANEL
  if (RtsFlags.GcFlags.frontpanel) {
      updateFrontPanelBeforeGC(N);
  }
#endif

#ifdef DEBUG
  // check for memory leaks if DEBUG is on 
  memInventory(traceClass(DEBUG_gc));
#endif

  // check stack sanity *before* GC
  IF_DEBUG(sanity, checkFreeListSanity());
  IF_DEBUG(sanity, checkMutableLists());

  // Initialise all our gc_thread structures
  for (t = 0; t < n_gc_threads; t++) {
      init_gc_thread(gc_threads[t]);
  }

  // Initialise all the generations/steps that we're collecting.
  for (g = 0; g <= N; g++) {
      init_collected_gen(g,n_gc_threads);
  }
  
  // Initialise all the generations/steps that we're *not* collecting.
  for (g = N+1; g < RtsFlags.GcFlags.generations; g++) {
      init_uncollected_gen(g,n_gc_threads);
  }

  /* Allocate a mark stack if we're doing a major collection.
   */
  if (major_gc) {
      nat mark_stack_blocks;
      mark_stack_blocks = stg_max(MARK_STACK_BLOCKS, 
                                  oldest_gen->steps[0].n_old_blocks / 100);
      mark_stack_bdescr = allocGroup(mark_stack_blocks);
      mark_stack = (StgPtr *)mark_stack_bdescr->start;
      mark_sp    = mark_stack;
      mark_splim = mark_stack + (mark_stack_blocks * BLOCK_SIZE_W);
  } else {
      mark_stack_bdescr = NULL;
  }

  // this is the main thread
  gct = gc_threads[0];

  /* -----------------------------------------------------------------------
   * follow all the roots that we know about:
   *   - mutable lists from each generation > N
   * we want to *scavenge* these roots, not evacuate them: they're not
   * going to move in this GC.
   * Also do them in reverse generation order, for the usual reason:
   * namely to reduce the likelihood of spurious old->new pointers.
   */
  for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      generations[g].saved_mut_list = generations[g].mut_list;
      generations[g].mut_list = allocBlock(); 
      // mut_list always has at least one block.
  }

  // the main thread is running: this prevents any other threads from
  // exiting prematurely, so we can start them now.
  // NB. do this after the mutable lists have been saved above, otherwise
  // the other GC threads will be writing into the old mutable lists.
  inc_running();
  wakeup_gc_threads(n_gc_threads);

  for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      scavenge_mutable_list(&generations[g]);
  }

  // follow roots from the CAF list (used by GHCi)
  gct->evac_step = 0;
  markCAFs(mark_root, gct);

  // follow all the roots that the application knows about.
  gct->evac_step = 0;
  markSomeCapabilities(mark_root, gct, gct->thread_index, n_gc_threads);

#if defined(RTS_USER_SIGNALS)
  // mark the signal handlers (signals should be already blocked)
  markSignalHandlers(mark_root, gct);
#endif

  // Mark the weak pointer list, and prepare to detect dead weak pointers.
  markWeakPtrList();
  initWeakForGC();

  // Mark the stable pointer table.
  markStablePtrTable(mark_root, gct);

  /* -------------------------------------------------------------------------
   * Repeatedly scavenge all the areas we know about until there's no
   * more scavenging to be done.
   */
  for (;;)
  {
      scavenge_until_all_done();
      // The other threads are now stopped.  We might recurse back to
      // here, but from now on this is the only thread.
      
      // if any blackholes are alive, make the threads that wait on
      // them alive too.
      if (traverseBlackholeQueue()) {
	  inc_running(); 
	  continue;
      }
  
      // must be last...  invariant is that everything is fully
      // scavenged at this point.
      if (traverseWeakPtrList()) { // returns rtsTrue if evaced something 
	  inc_running();
	  continue;
      }

      // If we get to here, there's really nothing left to do.
      break;
  }

  shutdown_gc_threads(n_gc_threads);

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

  // Two-space collector: free the old to-space.
  // g0s0->old_blocks is the old nursery
  // g0s0->blocks is to-space from the previous GC
  if (RtsFlags.GcFlags.generations == 1) {
      if (g0s0->blocks != NULL) {
	  freeChain(g0s0->blocks);
	  g0s0->blocks = NULL;
      }
  }

  // For each workspace, in each thread, move the copied blocks to the step
  {
      gc_thread *thr;
      step_workspace *ws;
      bdescr *prev, *next;

      for (t = 0; t < n_gc_threads; t++) {
	  thr = gc_threads[t];

          // not step 0
          if (RtsFlags.GcFlags.generations == 1) {
              s = 0;
          } else {
              s = 1;
          }
          for (; s < total_steps; s++) {
              ws = &thr->steps[s];

              // Push the final block
              if (ws->todo_bd) { 
                  push_scanned_block(ws->todo_bd, ws);
              }

              ASSERT(gct->scan_bd == NULL);
              ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks);
              
              prev = NULL;
              for (bd = ws->scavd_list; bd != NULL; bd = bd->link) {
                  ws->step->n_words += bd->free - bd->start;
                  prev = bd;
              }
              if (prev != NULL) {
                  prev->link = ws->step->blocks;
                  ws->step->blocks = ws->scavd_list;
              } 
              ws->step->n_blocks += ws->n_scavd_blocks;
          }
      }

      // Add all the partial blocks *after* we've added all the full
      // blocks.  This is so that we can grab the partial blocks back
      // again and try to fill them up in the next GC.
      for (t = 0; t < n_gc_threads; t++) {
	  thr = gc_threads[t];

          // not step 0
          if (RtsFlags.GcFlags.generations == 1) {
              s = 0;
          } else {
              s = 1;
          }
          for (; s < total_steps; s++) {
              ws = &thr->steps[s];

              prev = NULL;
              for (bd = ws->part_list; bd != NULL; bd = next) {
                  next = bd->link;
                  if (bd->free == bd->start) {
                      if (prev == NULL) {
                          ws->part_list = next;
                      } else {
                          prev->link = next;
                      }
                      freeGroup(bd);
                      ws->n_part_blocks--;
                  } else {
                      ws->step->n_words += bd->free - bd->start;
                      prev = bd;
                  }
              }
              if (prev != NULL) {
                  prev->link = ws->step->blocks;
                  ws->step->blocks = ws->part_list;
              }
              ws->step->n_blocks += ws->n_part_blocks;

              ASSERT(countBlocks(ws->step->blocks) == ws->step->n_blocks);
              ASSERT(countOccupied(ws->step->blocks) == ws->step->n_words);
	  }
      }
  }

  // Finally: compact or sweep the oldest generation.
  if (major_gc && oldest_gen->steps[0].mark) {
      if (oldest_gen->steps[0].compact) 
          compact(gct->scavenged_static_objects);
      else
          sweep(&oldest_gen->steps[0]);
  }

  /* run through all the generations/steps and tidy up 
   */
  copied = 0;
  max_copied = 0;
  avg_copied = 0;
  { 
      nat i;
      for (i=0; i < n_gc_threads; i++) {
          if (n_gc_threads > 1) {
              trace(TRACE_gc,"thread %d:", i);
              trace(TRACE_gc,"   copied           %ld", gc_threads[i]->copied * sizeof(W_));
              trace(TRACE_gc,"   scanned          %ld", gc_threads[i]->scanned * sizeof(W_));
              trace(TRACE_gc,"   any_work         %ld", gc_threads[i]->any_work);
              trace(TRACE_gc,"   no_work          %ld", gc_threads[i]->no_work);
              trace(TRACE_gc,"   scav_find_work %ld",   gc_threads[i]->scav_find_work);
          }
          copied += gc_threads[i]->copied;
          max_copied = stg_max(gc_threads[i]->copied, max_copied);
      }
      if (n_gc_threads == 1) {
          max_copied = 0;
          avg_copied = 0;
      } else {
          avg_copied = copied;
      }
  }

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {

    if (g == N) {
      generations[g].collections++; // for stats 
      if (n_gc_threads > 1) generations[g].par_collections++;
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
      bdescr *next, *prev;
      stp = &generations[g].steps[s];

      // for generations we collected... 
      if (g <= N) {

	/* free old memory and shift to-space into from-space for all
	 * the collected steps (except the allocation area).  These
	 * freed blocks will probaby be quickly recycled.
	 */
	if (!(g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1)) {
	    if (stp->mark)
            {
		// tack the new blocks on the end of the existing blocks
		if (stp->old_blocks != NULL) {

                    prev = NULL;
		    for (bd = stp->old_blocks; bd != NULL; bd = next) {

                        next = bd->link;

                        if (!(bd->flags & BF_MARKED))
                        {
                            if (prev == NULL) {
                                stp->old_blocks = next;
                            } else {
                                prev->link = next;
                            }
                            freeGroup(bd);
                            stp->n_old_blocks--;
                        }
                        else
                        {
                            stp->n_words += bd->free - bd->start;

                            // NB. this step might not be compacted next
                            // time, so reset the BF_MARKED flags.
                            // They are set before GC if we're going to
                            // compact.  (search for BF_MARKED above).
                            bd->flags &= ~BF_MARKED;
                            
                            // between GCs, all blocks in the heap except
                            // for the nursery have the BF_EVACUATED flag set.
                            bd->flags |= BF_EVACUATED;

                            prev = bd;
                        }
		    }

                    if (prev != NULL) {
                        prev->link = stp->blocks;
                        stp->blocks = stp->old_blocks;
                    }
		}
		// add the new blocks to the block tally
		stp->n_blocks += stp->n_old_blocks;
		ASSERT(countBlocks(stp->blocks) == stp->n_blocks);
                ASSERT(countOccupied(stp->blocks) == stp->n_words);
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
	  dbl_link_onto(bd, &stp->large_objects);
	}

	// add the new blocks we promoted during this GC 
	stp->n_large_blocks += stp->n_scavenged_large_blocks;
      }
    }
  }

  // update the max size of older generations after a major GC
  resize_generations();
  
  // Calculate the amount of live data for stats.
  live = calcLiveWords();

  // Free the small objects allocated via allocate(), since this will
  // all have been copied into G0S1 now.  
  if (RtsFlags.GcFlags.generations > 1) {
      if (g0s0->blocks != NULL) {
          freeChain(g0s0->blocks);
          g0s0->blocks = NULL;
      }
      g0s0->n_blocks = 0;
      g0s0->n_words = 0;
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
  if (n_gc_threads > 1) {
      barf("profiling is currently broken with multi-threaded GC");
      // ToDo: fix the gct->scavenged_static_objects below
  }
  resetStaticObjectForRetainerProfiling(gct->scavenged_static_objects);
#endif

  // zero the scavenged static object list 
  if (major_gc) {
      nat i;
      for (i = 0; i < n_gc_threads; i++) {
          zero_static_object_list(gc_threads[i]->scavenged_static_objects);
      }
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
  performPendingThrowTos(exception_threads);
  ACQUIRE_SM_LOCK;

  // Update the stable pointer hash table.
  updateStablePtrTable(major_gc);

  // Remove useless sparks from the spark pools
#ifdef THREADED_RTS
  pruneSparkQueues();
#endif

  // check sanity after GC 
  IF_DEBUG(sanity, checkSanity());

  // extra GC trace info 
  if (traceClass(TRACE_gc|DEBUG_gc)) statDescribeGens();

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
  memInventory(traceClass(DEBUG_gc));
#endif

#ifdef RTS_GTK_FRONTPANEL
  if (RtsFlags.GcFlags.frontpanel) {
      updateFrontPanelAfterGC( N, live );
  }
#endif

  // ok, GC over: tell the stats department what happened. 
  slop = calcLiveBlocks() * BLOCK_SIZE_W - live;
  stat_endGC(allocated, live, copied, N, max_copied, avg_copied, slop);

#if defined(RTS_USER_SIGNALS)
  if (RtsFlags.MiscFlags.install_signal_handlers) {
    // unblock signals again
    unblockUserSignals();
  }
#endif

  RELEASE_SM_LOCK;

  gct = saved_gct;
}

/* -----------------------------------------------------------------------------
   Figure out which generation to collect, initialise N and major_gc.

   Also returns the total number of blocks in generations that will be
   collected.
   -------------------------------------------------------------------------- */

static nat
initialise_N (rtsBool force_major_gc)
{
    int g;
    nat s, blocks, blocks_total;

    blocks = 0;
    blocks_total = 0;

    if (force_major_gc) {
        N = RtsFlags.GcFlags.generations - 1;
    } else {
        N = 0;
    }

    for (g = RtsFlags.GcFlags.generations - 1; g >= 0; g--) {
        blocks = 0;
        for (s = 0; s < generations[g].n_steps; s++) {
            blocks += generations[g].steps[s].n_words / BLOCK_SIZE_W;
            blocks += generations[g].steps[s].n_large_blocks;
        }
        if (blocks >= generations[g].max_blocks) {
            N = stg_max(N,g);
        }
        if ((nat)g <= N) {
            blocks_total += blocks;
        }
    }

    blocks_total += countNurseryBlocks();

    major_gc = (N == RtsFlags.GcFlags.generations-1);
    return blocks_total;
}

/* -----------------------------------------------------------------------------
   Initialise the gc_thread structures.
   -------------------------------------------------------------------------- */

static gc_thread *
alloc_gc_thread (int n)
{
    nat s;
    step_workspace *ws;
    gc_thread *t;

    t = stgMallocBytes(sizeof(gc_thread) + total_steps * sizeof(step_workspace),
                       "alloc_gc_thread");

#ifdef THREADED_RTS
    t->id = 0;
    initCondition(&t->wake_cond);
    initMutex(&t->wake_mutex);
    t->wakeup = rtsTrue;  // starts true, so we can wait for the
                          // thread to start up, see wakeup_gc_threads
    t->exit   = rtsFalse;
#endif

    t->thread_index = n;
    t->free_blocks = NULL;
    t->gc_count = 0;

    init_gc_thread(t);
    
#ifdef USE_PAPI
    t->papi_events = -1;
#endif

    for (s = 0; s < total_steps; s++)
    {
        ws = &t->steps[s];
        ws->step = &all_steps[s];
        ASSERT(s == ws->step->abs_no);
        ws->gct = t;
        
        ws->todo_bd = NULL;
        ws->buffer_todo_bd = NULL;
        
        ws->part_list = NULL;
        ws->n_part_blocks = 0;

        ws->scavd_list = NULL;
        ws->n_scavd_blocks = 0;
    }

    return t;
}


static void
alloc_gc_threads (void)
{
    if (gc_threads == NULL) {
#if defined(THREADED_RTS)
        nat i;
	gc_threads = stgMallocBytes (RtsFlags.ParFlags.gcThreads * 
				     sizeof(gc_thread*), 
				     "alloc_gc_threads");

	for (i = 0; i < RtsFlags.ParFlags.gcThreads; i++) {
	    gc_threads[i] = alloc_gc_thread(i);
	}
#else
	gc_threads = stgMallocBytes (sizeof(gc_thread*), 
				     "alloc_gc_threads");

	gc_threads[0] = alloc_gc_thread(0);
#endif
    }
}

/* ----------------------------------------------------------------------------
   Start GC threads
   ------------------------------------------------------------------------- */

static nat gc_running_threads;

#if defined(THREADED_RTS)
static Mutex gc_running_mutex;
#endif

static nat
inc_running (void)
{
    nat n_running;
    ACQUIRE_LOCK(&gc_running_mutex);
    n_running = ++gc_running_threads;
    RELEASE_LOCK(&gc_running_mutex);
    ASSERT(n_running <= n_gc_threads);
    return n_running;
}

static nat
dec_running (void)
{
    nat n_running;
    ACQUIRE_LOCK(&gc_running_mutex);
    ASSERT(n_gc_threads != 0);
    n_running = --gc_running_threads;
    RELEASE_LOCK(&gc_running_mutex);
    return n_running;
}

static rtsBool
any_work (void)
{
    int s;
    step_workspace *ws;

    gct->any_work++;

    write_barrier();

    // scavenge objects in compacted generation
    if (mark_stack_overflowed || oldgen_scan_bd != NULL ||
	(mark_stack_bdescr != NULL && !mark_stack_empty())) {
	return rtsTrue;
    }
    
    // Check for global work in any step.  We don't need to check for
    // local work, because we have already exited scavenge_loop(),
    // which means there is no local work for this thread.
    for (s = total_steps-1; s >= 0; s--) {
        if (s == 0 && RtsFlags.GcFlags.generations > 1) { 
            continue; 
        }
        ws = &gct->steps[s];
        if (ws->todo_large_objects) return rtsTrue;
        if (ws->step->todos) return rtsTrue;
    }

    gct->no_work++;

    return rtsFalse;
}    

static void
scavenge_until_all_done (void)
{
    nat r;
	
    debugTrace(DEBUG_gc, "GC thread %d working", gct->thread_index);

loop:
#if defined(THREADED_RTS)
    if (n_gc_threads > 1) {
        scavenge_loop();
    } else {
        scavenge_loop1();
    }
#else
    scavenge_loop();
#endif

    // scavenge_loop() only exits when there's no work to do
    r = dec_running();
    
    debugTrace(DEBUG_gc, "GC thread %d idle (%d still running)", 
	       gct->thread_index, r);

    while (gc_running_threads != 0) {
        // usleep(1);
	if (any_work()) {
	    inc_running();
	    goto loop;
	}
	// any_work() does not remove the work from the queue, it
	// just checks for the presence of work.  If we find any,
	// then we increment gc_running_threads and go back to 
	// scavenge_loop() to perform any pending work.
    }
    
    // All threads are now stopped
    debugTrace(DEBUG_gc, "GC thread %d finished.", gct->thread_index);
}

#if defined(THREADED_RTS)
//
// gc_thread_work(): Scavenge until there's no work left to do and all
// the running threads are idle.
//
static void
gc_thread_work (void)
{
    // gc_running_threads has already been incremented for us; this is
    // a worker thread and the main thread bumped gc_running_threads
    // before waking us up.

    // Every thread evacuates some roots.
    gct->evac_step = 0;
    markSomeCapabilities(mark_root, gct, gct->thread_index, n_gc_threads);

    scavenge_until_all_done();
}


static void
gc_thread_mainloop (void)
{
    while (!gct->exit) {

	// Wait until we're told to wake up
	ACQUIRE_LOCK(&gct->wake_mutex);
	gct->wakeup = rtsFalse;
	while (!gct->wakeup) {
	    debugTrace(DEBUG_gc, "GC thread %d standing by...", 
		       gct->thread_index);
	    waitCondition(&gct->wake_cond, &gct->wake_mutex);
	}
	RELEASE_LOCK(&gct->wake_mutex);
	if (gct->exit) break;

#ifdef USE_PAPI
        // start performance counters in this thread...
        if (gct->papi_events == -1) {
            papi_init_eventset(&gct->papi_events);
        }
        papi_thread_start_gc1_count(gct->papi_events);
#endif

	gc_thread_work();

#ifdef USE_PAPI
        // count events in this thread towards the GC totals
        papi_thread_stop_gc1_count(gct->papi_events);
#endif
    }
}	
#endif

#if defined(THREADED_RTS)
static void
gc_thread_entry (gc_thread *my_gct)
{
    gct = my_gct;
    debugTrace(DEBUG_gc, "GC thread %d starting...", gct->thread_index);
    gct->id = osThreadId();
    gc_thread_mainloop();
}
#endif

static void
start_gc_threads (void)
{
#if defined(THREADED_RTS)
    nat i;
    OSThreadId id;
    static rtsBool done = rtsFalse;

    gc_running_threads = 0;
    initMutex(&gc_running_mutex);

    if (!done) {
	// Start from 1: the main thread is 0
	for (i = 1; i < RtsFlags.ParFlags.gcThreads; i++) {
	    createOSThread(&id, (OSThreadProc*)&gc_thread_entry, 
			   gc_threads[i]);
	}
	done = rtsTrue;
    }
#endif
}

static void
wakeup_gc_threads (nat n_threads USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;
    for (i=1; i < n_threads; i++) {
	inc_running();
        debugTrace(DEBUG_gc, "waking up gc thread %d", i);
        do {
            ACQUIRE_LOCK(&gc_threads[i]->wake_mutex);
            if (gc_threads[i]->wakeup) {
                RELEASE_LOCK(&gc_threads[i]->wake_mutex);
                continue;
            } else {
                break;
            }
        } while (1);
	gc_threads[i]->wakeup = rtsTrue;
	signalCondition(&gc_threads[i]->wake_cond);
	RELEASE_LOCK(&gc_threads[i]->wake_mutex);
    }
#endif
}

// After GC is complete, we must wait for all GC threads to enter the
// standby state, otherwise they may still be executing inside
// any_work(), and may even remain awake until the next GC starts.
static void
shutdown_gc_threads (nat n_threads USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;
    rtsBool wakeup;
    for (i=1; i < n_threads; i++) {
        do {
            ACQUIRE_LOCK(&gc_threads[i]->wake_mutex);
            wakeup = gc_threads[i]->wakeup;
            // wakeup is false while the thread is waiting
            RELEASE_LOCK(&gc_threads[i]->wake_mutex);
        } while (wakeup);
    }
#endif
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

	stp = &generations[g].steps[s];
	ASSERT(stp->gen_no == g);

        // we'll construct a new list of threads in this step
        // during GC, throw away the current list.
        stp->old_threads = stp->threads;
        stp->threads = END_TSO_QUEUE;

	// generation 0, step 0 doesn't need to-space 
	if (g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1) { 
	    continue; 
	}
	
	// deprecate the existing blocks
	stp->old_blocks   = stp->blocks;
	stp->n_old_blocks = stp->n_blocks;
	stp->blocks       = NULL;
	stp->n_blocks     = 0;
	stp->n_words      = 0;
	stp->live_estimate = 0;

	// we don't have any to-be-scavenged blocks yet
	stp->todos = NULL;
        stp->todos_last = NULL;
	stp->n_todos = 0;

	// initialise the large object queues.
	stp->scavenged_large_objects = NULL;
	stp->n_scavenged_large_blocks = 0;

	// mark the small objects as from-space
	for (bd = stp->old_blocks; bd; bd = bd->link) {
	    bd->flags &= ~BF_EVACUATED;
	}

	// mark the large objects as from-space
	for (bd = stp->large_objects; bd; bd = bd->link) {
	    bd->flags &= ~BF_EVACUATED;
	}

	// for a compacted step, we need to allocate the bitmap
	if (stp->mark) {
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
		    
		    // Also at this point we set the BF_MARKED flag
		    // for this block.  The invariant is that
		    // BF_MARKED is always unset, except during GC
		    // when it is set on those blocks which will be
		    // compacted.
                    if (!(bd->flags & BF_FRAGMENTED)) {
                        bd->flags |= BF_MARKED;
                    }
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

	    ws = &gc_threads[t]->steps[g * RtsFlags.GcFlags.steps + s];

	    ws->todo_large_objects = NULL;

            ws->part_list = NULL;
            ws->n_part_blocks = 0;

	    // allocate the first to-space block; extra blocks will be
	    // chained on as necessary.
	    ws->todo_bd = NULL;
	    ws->buffer_todo_bd = NULL;
	    alloc_todo_block(ws,0);

	    ws->scavd_list = NULL;
	    ws->n_scavd_blocks = 0;
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
    
    for (s = 0; s < generations[g].n_steps; s++) {
	    
        stp = &generations[g].steps[s];

        for (t = 0; t < threads; t++) {
	    ws = &gc_threads[t]->steps[g * RtsFlags.GcFlags.steps + s];
	    
	    ws->buffer_todo_bd = NULL;
	    ws->todo_large_objects = NULL;

            ws->part_list = NULL;
            ws->n_part_blocks = 0;

	    ws->scavd_list = NULL;
	    ws->n_scavd_blocks = 0;

	    // If the block at the head of the list in this generation
	    // is less than 3/4 full, then use it as a todo block.
	    if (stp->blocks && isPartiallyFull(stp->blocks))
	    {
		ws->todo_bd = stp->blocks;
                ws->todo_free = ws->todo_bd->free;
                ws->todo_lim = ws->todo_bd->start + BLOCK_SIZE_W;
		stp->blocks = stp->blocks->link;
		stp->n_blocks -= 1;
                stp->n_words -= ws->todo_bd->free - ws->todo_bd->start;
		ws->todo_bd->link = NULL;
		// we must scan from the current end point.
		ws->todo_bd->u.scan = ws->todo_bd->free;
	    } 
	    else
	    {
		ws->todo_bd = NULL;
		alloc_todo_block(ws,0);
	    }
	}

        // deal out any more partial blocks to the threads' part_lists
        t = 0;
        while (stp->blocks && isPartiallyFull(stp->blocks))
        {
            bd = stp->blocks;
            stp->blocks = bd->link;
	    ws = &gc_threads[t]->steps[g * RtsFlags.GcFlags.steps + s];
            bd->link = ws->part_list;
            ws->part_list = bd;
            ws->n_part_blocks += 1;
            bd->u.scan = bd->free;
            stp->n_blocks -= 1;
            stp->n_words -= bd->free - bd->start;
            t++;
            if (t == n_gc_threads) t = 0;
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
    t->static_objects = END_OF_STATIC_LIST;
    t->scavenged_static_objects = END_OF_STATIC_LIST;
    t->scan_bd = NULL;
    t->evac_step = 0;
    t->failed_to_evac = rtsFalse;
    t->eager_promotion = rtsTrue;
    t->thunk_selector_depth = 0;
    t->copied = 0;
    t->scanned = 0;
    t->any_work = 0;
    t->no_work = 0;
    t->scav_find_work = 0;
}

/* -----------------------------------------------------------------------------
   Function we pass to evacuate roots.
   -------------------------------------------------------------------------- */

static void
mark_root(void *user, StgClosure **root)
{
    // we stole a register for gct, but this function is called from
    // *outside* the GC where the register variable is not in effect,
    // so we need to save and restore it here.  NB. only call
    // mark_root() from the main GC thread, otherwise gct will be
    // incorrect.
    gc_thread *saved_gct;
    saved_gct = gct;
    gct = user;
    
    evacuate(root);
    
    gct = saved_gct;
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
	nat live, size, min_alloc, words;
	nat max  = RtsFlags.GcFlags.maxHeapSize;
	nat gens = RtsFlags.GcFlags.generations;
	
	// live in the oldest generations
        if (oldest_gen->steps[0].live_estimate != 0) {
            words = oldest_gen->steps[0].live_estimate;
        } else {
            words = oldest_gen->steps[0].n_words;
        }
        live = (words + BLOCK_SIZE_W - 1) / BLOCK_SIZE_W +
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
	    oldest_gen->steps[0].mark = 1;
	    oldest_gen->steps[0].compact = 1;
//	  debugBelch("compaction: on\n", live);
	} else {
	    oldest_gen->steps[0].mark = 0;
	    oldest_gen->steps[0].compact = 0;
//	  debugBelch("compaction: off\n", live);
	}

        if (RtsFlags.GcFlags.sweep) {
	    oldest_gen->steps[0].mark = 1;
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
	    
	    if (oldest_gen->steps[0].compact) {
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
	blocks = g0s0->n_blocks;
	
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
