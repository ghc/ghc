/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "HsFFI.h"

#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"            // NB. before RtsSignals.h which
                                // clobbers REG_R1 on arm/Linux
#include "Compact.h"
#include "Evac.h"
#include "Scav.h"
#include "GCUtils.h"
#include "MarkStack.h"
#include "MarkWeak.h"
#include "Sparks.h"
#include "Sweep.h"

#include "Arena.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "Apply.h"
#include "Updates.h"
#include "Stats.h"
#include "Schedule.h"
#include "Sanity.h"
#include "BlockAlloc.h"
#include "ProfHeap.h"
#include "Weak.h"
#include "Prelude.h"
#include "RtsSignals.h"
#include "STM.h"
#include "Trace.h"
#include "RetainerProfile.h"
#include "LdvProfile.h"
#include "RaiseAsync.h"
#include "StableName.h"
#include "StablePtr.h"
#include "CheckUnload.h"
#include "CNF.h"
#include "RtsFlags.h"
#include "NonMoving.h"

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
 *
 * See also: Note [STATIC_LINK fields] in Storage.h.
 */

/* N is the oldest generation being collected, where the generations
 * are numbered starting at 0.  A major GC (indicated by the major_gc
 * flag) is when we're collecting all generations.  We only attempt to
 * deal with static objects and GC CAFs when doing a major GC.
 */
uint32_t N;
bool major_gc;
bool deadlock_detect_gc;

/* Data used for allocation area sizing.
 */
static W_ g0_pcnt_kept = 30; // percentage of g0 live at last minor GC

/* Mut-list stats */
#if defined(DEBUG)
uint32_t mutlist_MUTVARS,
    mutlist_MUTARRS,
    mutlist_MVARS,
    mutlist_TVAR,
    mutlist_TVAR_WATCH_QUEUE,
    mutlist_TREC_CHUNK,
    mutlist_TREC_HEADER,
    mutlist_OTHERS;
#endif

/* Thread-local data for each GC thread
 */
gc_thread **gc_threads = NULL;

#if !defined(THREADED_RTS)
// Must be aligned to 64-bytes to meet stated 64-byte alignment of gen_workspace
StgWord8 the_gc_thread[sizeof(gc_thread) + 64 * sizeof(gen_workspace)]
    ATTRIBUTE_ALIGNED(64);
#endif

// Number of threads running in *this* GC.  Affects how many
// step->todos[] lists we have to look in to find work.
uint32_t n_gc_threads;

// For stats:
static long copied;        // *words* copied & scavenged during this GC

#if defined(PROF_SPIN) && defined(THREADED_RTS)
// spin and yield counts for the quasi-SpinLock in waitForGcThreads
volatile StgWord64 waitForGcThreads_spin = 0;
volatile StgWord64 waitForGcThreads_yield = 0;
volatile StgWord64 whitehole_gc_spin = 0;
#endif

bool work_stealing;

uint32_t static_flag = STATIC_FLAG_B;
uint32_t prev_static_flag = STATIC_FLAG_A;

DECLARE_GCT

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static void mark_root               (void *user, StgClosure **root);
static void prepare_collected_gen   (generation *gen);
static void prepare_uncollected_gen (generation *gen);
static void init_gc_thread          (gc_thread *t);
static void resize_nursery          (void);
static void start_gc_threads        (void);
static void scavenge_until_all_done (void);
static StgWord inc_running          (void);
static StgWord dec_running          (void);
static void wakeup_gc_threads       (uint32_t me, bool idle_cap[]);
static void shutdown_gc_threads     (uint32_t me, bool idle_cap[]);
static void collect_gct_blocks      (void);
static void collect_pinned_object_blocks (void);
static void heapOverflow            (void);

#if defined(DEBUG)
static void gcCAFs                  (void);
#endif

/* -----------------------------------------------------------------------------
   The mark stack.
   -------------------------------------------------------------------------- */

bdescr *mark_stack_top_bd; // topmost block in the mark stack
bdescr *mark_stack_bd;     // current block in the mark stack
StgPtr mark_sp;            // pointer to the next unallocated mark stack entry

/* -----------------------------------------------------------------------------
   GarbageCollect: the main entry point to the garbage collector.

   The collect_gen parameter is gotten by calling calcNeeded().

   Locks held: all capabilities are held throughout GarbageCollect().
   -------------------------------------------------------------------------- */

void
GarbageCollect (uint32_t collect_gen,
                const bool do_heap_census,
                const bool deadlock_detect,
                uint32_t gc_type USED_IF_THREADS,
                Capability *cap,
                bool idle_cap[])
{
  bdescr *bd;
  generation *gen;
  StgWord live_blocks, live_words, par_max_copied, par_balanced_copied,
      gc_spin_spin, gc_spin_yield, mut_spin_spin, mut_spin_yield,
      any_work, no_work, scav_find_work;
#if defined(THREADED_RTS)
  gc_thread *saved_gct;
#endif
  uint32_t g, n;
  // The time we should report our heap census as occurring at, if necessary.
  Time mut_time = 0;

  if (do_heap_census) {
      RTSStats stats;
      getRTSStats(&stats);
      mut_time = stats.mutator_cpu_ns;
  }

  // necessary if we stole a callee-saves register for gct:
#if defined(THREADED_RTS)
  saved_gct = gct;
#endif

#if defined(PROFILING)
  CostCentreStack *save_CCS[n_capabilities];
#endif

  ACQUIRE_SM_LOCK;

#if defined(RTS_USER_SIGNALS)
  if (RtsFlags.MiscFlags.install_signal_handlers) {
    // block signals
    blockUserSignals();
  }
#endif

  ASSERT(sizeof(gen_workspace) == 16 * sizeof(StgWord));
  // otherwise adjust the padding in gen_workspace.

  // this is the main thread
  SET_GCT(gc_threads[cap->no]);

  // tell the stats department that we've started a GC
  stat_startGC(cap, gct);

  // Lock the StablePtr table. This prevents FFI calls manipulating
  // the table from occurring during GC.
  stablePtrLock();

#if defined(DEBUG)
  mutlist_MUTVARS = 0;
  mutlist_MUTARRS = 0;
  mutlist_MVARS = 0;
  mutlist_TVAR = 0;
  mutlist_TVAR_WATCH_QUEUE = 0;
  mutlist_TREC_CHUNK = 0;
  mutlist_TREC_HEADER = 0;
  mutlist_OTHERS = 0;
#endif

  // attribute any costs to CCS_GC
#if defined(PROFILING)
  for (n = 0; n < n_capabilities; n++) {
      save_CCS[n] = capabilities[n]->r.rCCCS;
      capabilities[n]->r.rCCCS = CCS_GC;
  }
#endif

  /* Figure out which generation to collect
   */
  N = collect_gen;
  major_gc = (N == RtsFlags.GcFlags.generations-1);

  /* See Note [Deadlock detection under nonmoving collector]. */
  deadlock_detect_gc = deadlock_detect;

#if defined(THREADED_RTS)
  if (major_gc && RtsFlags.GcFlags.useNonmoving && concurrent_coll_running) {
      /* If there is already a concurrent major collection running then
       * there is no benefit to starting another.
       * TODO: Catch heap-size runaway.
       */
      N--;
      collect_gen--;
      major_gc = false;
  }
#endif

  /* N.B. The nonmoving collector works a bit differently. See
   * Note [Static objects under the nonmoving collector].
   */
  if (major_gc && !RtsFlags.GcFlags.useNonmoving) {
      prev_static_flag = static_flag;
      static_flag =
          static_flag == STATIC_FLAG_A ? STATIC_FLAG_B : STATIC_FLAG_A;
  }

  if (major_gc) {
      prepareUnloadCheck();
  }

#if defined(THREADED_RTS)
  work_stealing = RtsFlags.ParFlags.parGcLoadBalancingEnabled &&
                  N >= RtsFlags.ParFlags.parGcLoadBalancingGen;
      // It's not always a good idea to do load balancing in parallel
      // GC.  In particular, for a parallel program we don't want to
      // lose locality by moving cached data into another CPU's cache
      // (this effect can be quite significant).
      //
      // We could have a more complex way to determine whether to do
      // work stealing or not, e.g. it might be a good idea to do it
      // if the heap is big.  For now, we just turn it on or off with
      // a flag.
#endif

  /* Start threads, so they can be spinning up while we finish initialisation.
   */
  start_gc_threads();

#if defined(THREADED_RTS)
  /* How many threads will be participating in this GC?
   * We don't try to parallelise minor GCs (unless the user asks for
   * it with +RTS -gn0), or mark/compact/sweep GC.
   */
  if (gc_type == SYNC_GC_PAR) {
      n_gc_threads = n_capabilities;
  } else {
      n_gc_threads = 1;
  }
#else
  n_gc_threads = 1;
#endif

  debugTrace(DEBUG_gc, "GC (gen %d, using %d thread(s))",
             N, n_gc_threads);

#if defined(DEBUG)
  // check for memory leaks if DEBUG is on
  memInventory(DEBUG_gc);
#endif

  // do this *before* we start scavenging
  collectFreshWeakPtrs();

  // check sanity *before* GC
  IF_DEBUG(sanity, checkSanity(false /* before GC */, major_gc));

  // gather blocks allocated using allocatePinned() from each capability
  // and put them on the g0->large_object list.
  collect_pinned_object_blocks();

  // Initialise all the generations that we're collecting.
  for (g = 0; g <= N; g++) {
      prepare_collected_gen(&generations[g]);
  }
  // Initialise all the generations that we're *not* collecting.
  for (g = N+1; g < RtsFlags.GcFlags.generations; g++) {
      prepare_uncollected_gen(&generations[g]);
  }

  // Prepare this gc_thread
  init_gc_thread(gct);

  /* Allocate a mark stack if we're doing a major collection.
   */
  if (major_gc && oldest_gen->mark) {
      mark_stack_bd     = allocBlock();
      mark_stack_top_bd = mark_stack_bd;
      mark_stack_bd->link = NULL;
      mark_stack_bd->u.back = NULL;
      mark_sp           = mark_stack_bd->start;
  } else {
      mark_stack_bd     = NULL;
      mark_stack_top_bd = NULL;
      mark_sp           = NULL;
  }

  /* -----------------------------------------------------------------------
   * follow all the roots that we know about:
   */

  // the main thread is running: this prevents any other threads from
  // exiting prematurely, so we can start them now.
  // NB. do this after the mutable lists have been saved above, otherwise
  // the other GC threads will be writing into the old mutable lists.
  inc_running();
  wakeup_gc_threads(gct->thread_index, idle_cap);

  traceEventGcWork(gct->cap);

  // scavenge the capability-private mutable lists.  This isn't part
  // of markSomeCapabilities() because markSomeCapabilities() can only
  // call back into the GC via mark_root() (due to the gct register
  // variable).
  if (n_gc_threads == 1) {
      for (n = 0; n < n_capabilities; n++) {
#if defined(THREADED_RTS)
          scavenge_capability_mut_Lists1(capabilities[n]);
#else
          scavenge_capability_mut_lists(capabilities[n]);
#endif
      }
  } else {
      scavenge_capability_mut_lists(gct->cap);
      for (n = 0; n < n_capabilities; n++) {
          if (idle_cap[n]) {
              markCapability(mark_root, gct, capabilities[n],
                             true/*don't mark sparks*/);
              scavenge_capability_mut_lists(capabilities[n]);
          }
      }
  }

  // follow roots from the CAF list (used by GHCi)
  gct->evac_gen_no = 0;
  markCAFs(mark_root, gct);

  // follow all the roots that the application knows about.
  gct->evac_gen_no = 0;
  if (n_gc_threads == 1) {
      for (n = 0; n < n_capabilities; n++) {
          markCapability(mark_root, gct, capabilities[n],
                         true/*don't mark sparks*/);
      }
  } else {
      markCapability(mark_root, gct, cap, true/*don't mark sparks*/);
  }

  markScheduler(mark_root, gct);

  // Mark the weak pointer list, and prepare to detect dead weak pointers.
  markWeakPtrList();
  initWeakForGC();

  // Mark the stable pointer table.
  markStablePtrTable(mark_root, gct);

  // Remember old stable name addresses.
  rememberOldStableNameAddresses ();

  /* -------------------------------------------------------------------------
   * Repeatedly scavenge all the areas we know about until there's no
   * more scavenging to be done.
   */

  StgWeak *dead_weak_ptr_list = NULL;
  StgTSO *resurrected_threads = END_TSO_QUEUE;

  for (;;)
  {
      scavenge_until_all_done();

      // The other threads are now stopped.  We might recurse back to
      // here, but from now on this is the only thread.

      // must be last...  invariant is that everything is fully
      // scavenged at this point.
      if (traverseWeakPtrList(&dead_weak_ptr_list, &resurrected_threads)) { // returns true if evaced something
          inc_running();
          continue;
      }

      // If we get to here, there's really nothing left to do.
      break;
  }

  shutdown_gc_threads(gct->thread_index, idle_cap);

  // Now see which stable names are still alive.
  gcStableNameTable();

#if defined(THREADED_RTS)
  if (n_gc_threads == 1) {
      for (n = 0; n < n_capabilities; n++) {
          pruneSparkQueue(false, capabilities[n]);
      }
  } else {
      for (n = 0; n < n_capabilities; n++) {
          if (n == cap->no || idle_cap[n]) {
              pruneSparkQueue(false, capabilities[n]);
         }
      }
  }
#endif

#if defined(PROFILING)
  // We call processHeapClosureForDead() on every closure destroyed during
  // the current garbage collection, so we invoke LdvCensusForDead().
  if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_LDV
      || RtsFlags.ProfFlags.bioSelector != NULL) {
      RELEASE_SM_LOCK; // LdvCensusForDead may need to take the lock
      LdvCensusForDead(N);
      ACQUIRE_SM_LOCK;
  }
#endif

  // NO MORE EVACUATION AFTER THIS POINT!

  // Finally: compact or sweep the oldest generation.
  if (major_gc && oldest_gen->mark) {
      if (oldest_gen->compact)
          compact(gct->scavenged_static_objects,
                  &dead_weak_ptr_list,
                  &resurrected_threads);
      else
          sweep(oldest_gen);
  }

  copied = 0;
  par_max_copied = 0;
  par_balanced_copied = 0;
  gc_spin_spin = 0;
  gc_spin_yield = 0;
  mut_spin_spin = 0;
  mut_spin_yield = 0;
  any_work = 0;
  no_work = 0;
  scav_find_work = 0;
  {
      uint32_t i;
      uint64_t par_balanced_copied_acc = 0;
      const gc_thread* thread;

      for (i=0; i < n_gc_threads; i++) {
          copied += gc_threads[i]->copied;
      }
      for (i=0; i < n_gc_threads; i++) {
          thread = gc_threads[i];
          if (n_gc_threads > 1) {
              debugTrace(DEBUG_gc,"thread %d:", i);
              debugTrace(DEBUG_gc,"   copied           %ld",
                         thread->copied * sizeof(W_));
              debugTrace(DEBUG_gc,"   scanned          %ld",
                         thread->scanned * sizeof(W_));
              debugTrace(DEBUG_gc,"   any_work         %ld",
                         thread->any_work);
              debugTrace(DEBUG_gc,"   no_work          %ld",
                         thread->no_work);
              debugTrace(DEBUG_gc,"   scav_find_work %ld",
                         thread->scav_find_work);

#if defined(THREADED_RTS) && defined(PROF_SPIN)
              gc_spin_spin += thread->gc_spin.spin;
              gc_spin_yield += thread->gc_spin.yield;
              mut_spin_spin += thread->mut_spin.spin;
              mut_spin_yield += thread->mut_spin.yield;
#endif

              any_work += thread->any_work;
              no_work += thread->no_work;
              scav_find_work += thread->scav_find_work;

              par_max_copied = stg_max(gc_threads[i]->copied, par_max_copied);
              par_balanced_copied_acc +=
                  stg_min(n_gc_threads * gc_threads[i]->copied, copied);
          }
      }
      if (n_gc_threads > 1) {
          // See Note [Work Balance] for an explanation of this computation
          par_balanced_copied =
              (par_balanced_copied_acc - copied + (n_gc_threads - 1) / 2) /
              (n_gc_threads - 1);
      }
  }

  // Run through all the generations and tidy up.
  // We're going to:
  //   - count the amount of "live" data (live_words, live_blocks)
  //   - count the amount of "copied" data in this GC (copied)
  //   - free from-space
  //   - make to-space the new from-space (set BF_EVACUATED on all blocks)
  //
  live_words = 0;
  live_blocks = 0;

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {

    if (g == N) {
      generations[g].collections++; // for stats
      if (n_gc_threads > 1) generations[g].par_collections++;
    }

    // Count the mutable list as bytes "copied" for the purposes of
    // stats.  Every mutable list is copied during every GC.
    if (g > 0) {
        W_ mut_list_size = 0;
        for (n = 0; n < n_capabilities; n++) {
            mut_list_size += countOccupied(capabilities[n]->mut_lists[g]);
        }
        copied +=  mut_list_size;

        debugTrace(DEBUG_gc,
                   "mut_list_size: %lu (%d vars, %d arrays, %d MVARs, %d TVARs, %d TVAR_WATCH_QUEUEs, %d TREC_CHUNKs, %d TREC_HEADERs, %d others)",
                   (unsigned long)(mut_list_size * sizeof(W_)),
                   mutlist_MUTVARS, mutlist_MUTARRS, mutlist_MVARS,
                   mutlist_TVAR, mutlist_TVAR_WATCH_QUEUE,
                   mutlist_TREC_CHUNK, mutlist_TREC_HEADER,
                   mutlist_OTHERS);
    }

    bdescr *next, *prev;
    gen = &generations[g];

    // for generations we collected...
    if (g <= N && !(RtsFlags.GcFlags.useNonmoving && gen == oldest_gen)) {

        /* free old memory and shift to-space into from-space for all
         * the collected generations (except the allocation area).  These
         * freed blocks will probably be quickly recycled.
         */
        if (gen->mark)
        {
            // tack the new blocks on the end of the existing blocks
            if (gen->old_blocks != NULL) {

                prev = NULL;
                for (bd = gen->old_blocks; bd != NULL; bd = next) {

                    next = bd->link;

                    if (!(bd->flags & BF_MARKED))
                    {
                        if (prev == NULL) {
                            gen->old_blocks = next;
                        } else {
                            prev->link = next;
                        }
                        freeGroup(bd);
                        gen->n_old_blocks--;
                    }
                    else
                    {
                        gen->n_words += bd->free - bd->start;

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
                    prev->link = gen->blocks;
                    gen->blocks = gen->old_blocks;
                }
            }
            // add the new blocks to the block tally
            gen->n_blocks += gen->n_old_blocks;
            ASSERT(countBlocks(gen->blocks) == gen->n_blocks);
            ASSERT(countOccupied(gen->blocks) == gen->n_words);
        }
        else // not compacted
        {
            freeChain(gen->old_blocks);
        }

        gen->old_blocks = NULL;
        gen->n_old_blocks = 0;

        /* LARGE OBJECTS.  The current live large objects are chained on
         * scavenged_large, having been moved during garbage
         * collection from large_objects.  Any objects left on the
         * large_objects list are therefore dead, so we free them here.
         */
        freeChain(gen->large_objects);
        gen->large_objects  = gen->scavenged_large_objects;
        gen->n_large_blocks = gen->n_scavenged_large_blocks;
        gen->n_large_words  = countOccupied(gen->large_objects);
        gen->n_new_large_words = 0;

        /* COMPACT_NFDATA. The currently live compacts are chained
         * to live_compact_objects, quite like large objects. And
         * objects left on the compact_objects list are dead.
         *
         * We don't run a simple freeChain because want to give the
         * CNF module some chance to free memory that freeChain would
         * not see (namely blocks appended to a CNF through a compactResize).
         *
         * See Note [Compact Normal Forms] for details.
         */
        for (bd = gen->compact_objects; bd; bd = next) {
            next = bd->link;
            compactFree(((StgCompactNFDataBlock*)bd->start)->owner);
        }
        gen->compact_objects = gen->live_compact_objects;
        gen->n_compact_blocks = gen->n_live_compact_blocks;
    }
    else // for generations > N
    {
        /* For older generations, we need to append the
         * scavenged_large_object list (i.e. large objects that have been
         * promoted during this GC) to the large_object list for that step.
         */
        for (bd = gen->scavenged_large_objects; bd; bd = next) {
            next = bd->link;
            dbl_link_onto(bd, &gen->large_objects);
            gen->n_large_words += bd->free - bd->start;
        }

        // And same for compacts
        for (bd = gen->live_compact_objects; bd; bd = next) {
            next = bd->link;
            dbl_link_onto(bd, &gen->compact_objects);
        }

        // add the new blocks we promoted during this GC
        gen->n_large_blocks += gen->n_scavenged_large_blocks;
        gen->n_compact_blocks += gen->n_live_compact_blocks;
    }

    ASSERT(countBlocks(gen->large_objects) == gen->n_large_blocks);
    ASSERT(countOccupied(gen->large_objects) == gen->n_large_words);
    // We can run the same assertion on compact objects because there
    // is memory "the GC doesn't see" (directly), but which is still
    // accounted in gen->n_compact_blocks

    gen->scavenged_large_objects = NULL;
    gen->n_scavenged_large_blocks = 0;
    gen->live_compact_objects = NULL;
    gen->n_live_compact_blocks = 0;

    // Count "live" data
    live_words  += genLiveWords(gen);
    live_blocks += genLiveBlocks(gen);

    // add in the partial blocks in the gen_workspaces
    {
        uint32_t i;
        for (i = 0; i < n_capabilities; i++) {
            live_words  += gcThreadLiveWords(i, gen->no);
            live_blocks += gcThreadLiveBlocks(i, gen->no);
        }
    }
  } // for all generations

  // Flush the update remembered sets. See Note [Eager update remembered set
  // flushing] in NonMovingMark.c
  if (RtsFlags.GcFlags.useNonmoving) {
      RELEASE_SM_LOCK;
      for (n = 0; n < n_capabilities; n++) {
          nonmovingAddUpdRemSetBlocks(&capabilities[n]->upd_rem_set.queue);
      }
      ACQUIRE_SM_LOCK;
  }

  // Mark and sweep the oldest generation.
  // N.B. This can only happen after we've moved
  // oldest_gen->scavenged_large_objects back to oldest_gen->large_objects.
  ASSERT(oldest_gen->scavenged_large_objects == NULL);
  if (RtsFlags.GcFlags.useNonmoving && major_gc) {
      // All threads in non-moving heap should be found to be alive, because
      // threads in the non-moving generation's list should live in the
      // non-moving heap, and we consider non-moving objects alive during
      // preparation.
      ASSERT(oldest_gen->old_threads == END_TSO_QUEUE);
      // For weaks, remember that we evacuated all weaks to the non-moving heap
      // in markWeakPtrList(), and then moved the weak_ptr_list list to
      // old_weak_ptr_list. We then moved weaks with live keys to the
      // weak_ptr_list again. Then, in collectDeadWeakPtrs() we moved weaks in
      // old_weak_ptr_list to dead_weak_ptr_list. So at this point
      // old_weak_ptr_list should be empty.
      ASSERT(oldest_gen->old_weak_ptr_list == NULL);

      // we may need to take the lock to allocate mark queue blocks
      RELEASE_SM_LOCK;
      // dead_weak_ptr_list contains weak pointers with dead keys. Those need to
      // be kept alive because we'll use them in finalizeSchedulers(). Similarly
      // resurrected_threads are also going to be used in resurrectedThreads()
      // so we need to mark those too.
      // Note that in sequential case these lists will be appended with more
      // weaks and threads found to be dead in mark.
#if !defined(THREADED_RTS)
      // In the non-threaded runtime this is the only time we push to the
      // upd_rem_set
      nonmovingAddUpdRemSetBlocks(&gct->cap->upd_rem_set.queue);
#endif
      nonmovingCollect(&dead_weak_ptr_list, &resurrected_threads);
      ACQUIRE_SM_LOCK;
  }

  // Update the max size of older generations after a major GC:
  // We can't resize here in the case of the concurrent collector since we
  // don't yet know how much live data we have. This will be instead done
  // once we finish marking.
  if (major_gc && RtsFlags.GcFlags.generations > 1 && ! RtsFlags.GcFlags.useNonmoving)
      resizeGenerations();

  // Free the mark stack.
  if (mark_stack_top_bd != NULL) {
      debugTrace(DEBUG_gc, "mark stack: %d blocks",
                 countBlocks(mark_stack_top_bd));
      freeChain(mark_stack_top_bd);
  }

  // Free any bitmaps.
  for (g = 0; g <= N; g++) {
      gen = &generations[g];
      if (gen->bitmap != NULL) {
          freeGroup(gen->bitmap);
          gen->bitmap = NULL;
      }
  }

  resize_nursery();

  resetNurseries();

#if defined(DEBUG)
  // Mark the garbage collected CAFs as dead. Done in `nonmovingGcCafs()` when
  // non-moving GC is enabled.
  if (major_gc && !RtsFlags.GcFlags.useNonmoving) {
      gcCAFs();
  }
#endif

  // Update the stable name hash table
  updateStableNameTable(major_gc);

  // unlock the StablePtr table.  Must be before scheduleFinalizers(),
  // because a finalizer may call hs_free_fun_ptr() or
  // hs_free_stable_ptr(), both of which access the StablePtr table.
  stablePtrUnlock();

  // Unload dynamically-loaded object code after a major GC.
  // See Note [Object unloading] in CheckUnload.c for details.
  //
  // TODO: Similar to `nonmovingGcCafs` non-moving GC should have its own
  // collector for these objects, but that's currently not implemented, so we
  // simply don't unload object code when non-moving GC is enabled.
  if (major_gc && !RtsFlags.GcFlags.useNonmoving) {
      checkUnload();
  }

#if defined(PROFILING)
  // resetStaticObjectForProfiling() must be called before
  // zeroing below.

  // ToDo: fix the gct->scavenged_static_objects below
  resetStaticObjectForProfiling(gct->scavenged_static_objects);
#endif

  // Start any pending finalizers.  Must be after
  // updateStableTables() and stableUnlock() (see #4221).
  RELEASE_SM_LOCK;
  scheduleFinalizers(cap, dead_weak_ptr_list);
  ACQUIRE_SM_LOCK;

  // check sanity after GC
  // before resurrectThreads(), because that might overwrite some
  // closures, which will cause problems with THREADED where we don't
  // fill slop. If we are using the nonmoving collector then we can't claim to
  // be *after* the major GC; it's now running concurrently.
  IF_DEBUG(sanity, checkSanity(true /* after GC */, major_gc && !RtsFlags.GcFlags.useNonmoving));

  // If a heap census is due, we need to do it before
  // resurrectThreads(), for the same reason as checkSanity above:
  // resurrectThreads() will overwrite some closures and leave slop
  // behind.
  if (do_heap_census) {
      debugTrace(DEBUG_sched, "performing heap census");
      RELEASE_SM_LOCK;
      heapCensus(mut_time);
      ACQUIRE_SM_LOCK;
  }

  // send exceptions to any threads which were about to die
  RELEASE_SM_LOCK;
  resurrectThreads(resurrected_threads);
  ACQUIRE_SM_LOCK;

  if (major_gc) {
      W_ need_prealloc, need_live, need, got;
      uint32_t i;

      need_live = 0;
      for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
          need_live += genLiveBlocks(&generations[i]);
      }
      need_live = stg_max(RtsFlags.GcFlags.minOldGenSize, need_live);

      need_prealloc = 0;
      for (i = 0; i < n_nurseries; i++) {
          need_prealloc += nurseries[i].n_blocks;
      }
      need_prealloc += RtsFlags.GcFlags.largeAllocLim;
      need_prealloc += countAllocdBlocks(exec_block);
      need_prealloc += arenaBlocks();
#if defined(PROFILING)
      if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER) {
          need_prealloc += retainerStackBlocks();
      }
#endif

      /* If the amount of data remains constant, next major GC we'll
       * require (F+1)*live + prealloc. We leave (F+2)*live + prealloc
       * in order to reduce repeated deallocation and reallocation. #14702
       */
      need = need_prealloc + (RtsFlags.GcFlags.oldGenFactor + 2) * need_live;

      /* Also, if user set heap size, do not drop below it.
       */
      need = stg_max(RtsFlags.GcFlags.heapSizeSuggestion, need);

      /* But with a large nursery, the above estimate might exceed
       * maxHeapSize.  A large resident set size might make the OS
       * kill this process, or swap unnecessarily.  Therefore we
       * ensure that our estimate does not exceed maxHeapSize.
       */
      if (RtsFlags.GcFlags.maxHeapSize != 0) {
          need = stg_min(RtsFlags.GcFlags.maxHeapSize, need);
      }

      need = BLOCKS_TO_MBLOCKS(need);

      got = mblocks_allocated;

      if (got > need) {
          returnMemoryToOS(got - need);
      }
  }

  // extra GC trace info
  IF_DEBUG(gc, statDescribeGens());

#if defined(DEBUG)
  // symbol-table based profiling
  /*  heapCensus(to_blocks); */ /* ToDo */
#endif

  // restore enclosing cost centre
#if defined(PROFILING)
  for (n = 0; n < n_capabilities; n++) {
      capabilities[n]->r.rCCCS = save_CCS[n];
  }
#endif

#if defined(DEBUG)
  // check for memory leaks if DEBUG is on
  memInventory(DEBUG_gc);
#endif

  // ok, GC over: tell the stats department what happened.
  stat_endGCWorker(cap, gct);
  stat_endGC(cap, gct, live_words, copied,
             live_blocks * BLOCK_SIZE_W - live_words /* slop */,
             N, n_gc_threads, gc_threads,
             par_max_copied, par_balanced_copied,
             gc_spin_spin, gc_spin_yield, mut_spin_spin, mut_spin_yield,
             any_work, no_work, scav_find_work);

#if defined(RTS_USER_SIGNALS)
  if (RtsFlags.MiscFlags.install_signal_handlers) {
    // unblock signals again
    unblockUserSignals();
  }
#endif

  RELEASE_SM_LOCK;

  SET_GCT(saved_gct);
}

/* -----------------------------------------------------------------------------
   Heap overflow is indicated by setting a flag that the caller of
   GarbageCollect can check.  (not ideal, TODO: better)
   -------------------------------------------------------------------------- */

static void heapOverflow(void)
{
    heap_overflow = true;
}

/* -----------------------------------------------------------------------------
   Initialise the gc_thread structures.
   -------------------------------------------------------------------------- */

static void
new_gc_thread (uint32_t n, gc_thread *t)
{
    uint32_t g;
    gen_workspace *ws;

    t->cap = capabilities[n];

#if defined(THREADED_RTS)
    t->id = 0;
    initSpinLock(&t->gc_spin);
    initSpinLock(&t->mut_spin);
    ACQUIRE_SPIN_LOCK(&t->gc_spin);
    ACQUIRE_SPIN_LOCK(&t->mut_spin);
    t->wakeup = GC_THREAD_INACTIVE;  // starts true, so we can wait for the
                          // thread to start up, see wakeup_gc_threads
#endif

    t->thread_index = n;
    t->free_blocks = NULL;
    t->gc_count = 0;

    init_gc_thread(t);

    for (g = 0; g < RtsFlags.GcFlags.generations; g++)
    {
        ws = &t->gens[g];
        ws->gen = &generations[g];
        ASSERT(g == ws->gen->no);
        ws->my_gct = t;

        // We want to call
        //   alloc_todo_block(ws,0);
        // but can't, because it uses gct which isn't set up at this point.
        // Hence, allocate a block for todo_bd manually:
        {
            bdescr *bd = allocBlockOnNode(capNoToNumaNode(n));
                // no lock, locks aren't initialised yet
            initBdescr(bd, ws->gen, ws->gen->to);
            bd->flags = BF_EVACUATED;
            bd->u.scan = bd->free = bd->start;

            ws->todo_bd = bd;
            ws->todo_free = bd->free;
            ws->todo_lim = bd->start + BLOCK_SIZE_W;
        }

        ws->todo_q = newWSDeque(128);
        ws->todo_overflow = NULL;
        ws->n_todo_overflow = 0;
        ws->todo_large_objects = NULL;
        ws->todo_seg = END_NONMOVING_TODO_LIST;

        ws->part_list = NULL;
        ws->n_part_blocks = 0;
        ws->n_part_words = 0;

        ws->scavd_list = NULL;
        ws->n_scavd_blocks = 0;
        ws->n_scavd_words = 0;
    }
}


void
initGcThreads (uint32_t from USED_IF_THREADS, uint32_t to USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    uint32_t i;

    if (from > 0) {
        gc_threads = stgReallocBytes (gc_threads, to * sizeof(gc_thread*),
                                      "initGcThreads");
    } else {
        gc_threads = stgMallocBytes (to * sizeof(gc_thread*),
                                     "initGcThreads");
    }

    for (i = from; i < to; i++) {
        gc_threads[i] =
            stgMallocBytes(sizeof(gc_thread) +
                           RtsFlags.GcFlags.generations * sizeof(gen_workspace),
                           "alloc_gc_threads");

        new_gc_thread(i, gc_threads[i]);
    }
#else
    ASSERT(from == 0 && to == 1);
    gc_threads = stgMallocBytes (sizeof(gc_thread*),"alloc_gc_threads");
    gc_threads[0] = gct;
    new_gc_thread(0,gc_threads[0]);
#endif
}

void
freeGcThreads (void)
{
    uint32_t g;
    if (gc_threads != NULL) {
#if defined(THREADED_RTS)
        uint32_t i;
        for (i = 0; i < n_capabilities; i++) {
            for (g = 0; g < RtsFlags.GcFlags.generations; g++)
            {
                freeWSDeque(gc_threads[i]->gens[g].todo_q);
            }
            stgFree (gc_threads[i]);
        }
        stgFree (gc_threads);
#else
        for (g = 0; g < RtsFlags.GcFlags.generations; g++)
        {
            freeWSDeque(gc_threads[0]->gens[g].todo_q);
        }
        stgFree (gc_threads);
#endif
        gc_threads = NULL;
    }
}

/* ----------------------------------------------------------------------------
   Start GC threads
   ------------------------------------------------------------------------- */

static volatile StgWord gc_running_threads;

static StgWord
inc_running (void)
{
    StgWord new;
    new = atomic_inc(&gc_running_threads, 1);
    ASSERT(new <= n_gc_threads);
    return new;
}

static StgWord
dec_running (void)
{
    ASSERT(gc_running_threads != 0);
    return atomic_dec(&gc_running_threads);
}

static bool
any_work (void)
{
    int g;
    gen_workspace *ws;

    gct->any_work++;

    write_barrier();

    // scavenge objects in compacted generation
    if (mark_stack_bd != NULL && !mark_stack_empty()) {
        return true;
    }

    // Check for global work in any gen.  We don't need to check for
    // local work, because we have already exited scavenge_loop(),
    // which means there is no local work for this thread.
    for (g = 0; g < (int)RtsFlags.GcFlags.generations; g++) {
        ws = &gct->gens[g];
        if (ws->todo_large_objects) return true;
        if (!looksEmptyWSDeque(ws->todo_q)) return true;
        if (ws->todo_overflow) return true;
    }

#if defined(THREADED_RTS)
    if (work_stealing) {
        uint32_t n;
        // look for work to steal
        for (n = 0; n < n_gc_threads; n++) {
            if (n == gct->thread_index) continue;
            for (g = RtsFlags.GcFlags.generations-1; g >= 0; g--) {
                ws = &gc_threads[n]->gens[g];
                if (!looksEmptyWSDeque(ws->todo_q)) return true;
            }
        }
    }
#endif

    gct->no_work++;
#if defined(THREADED_RTS)
    yieldThread();
#endif

    return false;
}

static void
scavenge_until_all_done (void)
{
    DEBUG_ONLY( uint32_t r );


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

    collect_gct_blocks();

    // scavenge_loop() only exits when there's no work to do

    // This atomic decrement also serves as a full barrier to ensure that any
    // writes we made during scavenging are visible to other threads.
#if defined(DEBUG)
    r = dec_running();
#else
    dec_running();
#endif

    traceEventGcIdle(gct->cap);

    debugTrace(DEBUG_gc, "%d GC threads still running", r);

    while (gc_running_threads != 0) {
        // usleep(1);
        if (any_work()) {
            inc_running();
            traceEventGcWork(gct->cap);
            goto loop;
        }
        // any_work() does not remove the work from the queue, it
        // just checks for the presence of work.  If we find any,
        // then we increment gc_running_threads and go back to
        // scavenge_loop() to perform any pending work.
    }

    traceEventGcDone(gct->cap);
}

#if defined(THREADED_RTS)

void
gcWorkerThread (Capability *cap)
{
    gc_thread *saved_gct;

    // necessary if we stole a callee-saves register for gct:
    saved_gct = gct;

    SET_GCT(gc_threads[cap->no]);
    gct->id = osThreadId();
    stat_startGCWorker (cap, gct);

    // Wait until we're told to wake up
    RELEASE_SPIN_LOCK(&gct->mut_spin);
    // yieldThread();
    //    Strangely, adding a yieldThread() here makes the CPU time
    //    measurements more accurate on Linux, perhaps because it syncs
    //    the CPU time across the multiple cores.  Without this, CPU time
    //    is heavily skewed towards GC rather than MUT.
    gct->wakeup = GC_THREAD_STANDING_BY;
    debugTrace(DEBUG_gc, "GC thread %d standing by...", gct->thread_index);
    ACQUIRE_SPIN_LOCK(&gct->gc_spin);

    init_gc_thread(gct);

    traceEventGcWork(gct->cap);

    // Every thread evacuates some roots.
    gct->evac_gen_no = 0;
    markCapability(mark_root, gct, cap, true/*prune sparks*/);
    scavenge_capability_mut_lists(cap);

    scavenge_until_all_done();

#if defined(THREADED_RTS)
    // Now that the whole heap is marked, we discard any sparks that
    // were found to be unreachable.  The main GC thread is currently
    // marking heap reachable via weak pointers, so it is
    // non-deterministic whether a spark will be retained if it is
    // only reachable via weak pointers.  To fix this problem would
    // require another GC barrier, which is too high a price.
    pruneSparkQueue(false, cap);
#endif

    // Wait until we're told to continue
    RELEASE_SPIN_LOCK(&gct->gc_spin);
    gct->wakeup = GC_THREAD_WAITING_TO_CONTINUE;
    debugTrace(DEBUG_gc, "GC thread %d waiting to continue...",
               gct->thread_index);
    stat_endGCWorker (cap, gct);
    ACQUIRE_SPIN_LOCK(&gct->mut_spin);
    debugTrace(DEBUG_gc, "GC thread %d on my way...", gct->thread_index);

    SET_GCT(saved_gct);
}

#endif

#if defined(THREADED_RTS)

void
waitForGcThreads (Capability *cap USED_IF_THREADS, bool idle_cap[])
{
    const uint32_t n_threads = n_capabilities;
    const uint32_t me = cap->no;
    uint32_t i, j;
    bool retry = true;
    Time t0, t1, t2;

    t0 = t1 = t2 = getProcessElapsedTime();

    while(retry) {
        for (i=0; i < n_threads; i++) {
            if (i == me || idle_cap[i]) continue;
            if (gc_threads[i]->wakeup != GC_THREAD_STANDING_BY) {
                prodCapability(capabilities[i], cap->running_task);
            }
        }
        for (j=0; j < 10; j++) {
            retry = false;
            for (i=0; i < n_threads; i++) {
                if (i == me || idle_cap[i]) continue;
                write_barrier();
                interruptCapability(capabilities[i]);
                if (gc_threads[i]->wakeup != GC_THREAD_STANDING_BY) {
                    retry = true;
                }
            }
            if (!retry) break;
#if defined(PROF_SPIN)
            waitForGcThreads_yield++;
#endif
            yieldThread();
        }

        t2 = getProcessElapsedTime();
        if (RtsFlags.GcFlags.longGCSync != 0 &&
            t2 - t1 > RtsFlags.GcFlags.longGCSync) {
            /* call this every longGCSync of delay */
            rtsConfig.longGCSync(cap->no, t2 - t0);
            t1 = t2;
        }
        if (retry) {
#if defined(PROF_SPIN)
            // This is a bit strange, we'll get more yields than spins.
            // I guess that means it's not a spin-lock at all, but these
            // numbers are still useful (I think).
            waitForGcThreads_spin++;
#endif
        }
    }

    if (RtsFlags.GcFlags.longGCSync != 0 &&
        t2 - t0 > RtsFlags.GcFlags.longGCSync) {
        rtsConfig.longGCSyncEnd(t2 - t0);
    }
}

#endif // THREADED_RTS

static void
start_gc_threads (void)
{
#if defined(THREADED_RTS)
    gc_running_threads = 0;
#endif
}

static void
wakeup_gc_threads (uint32_t me USED_IF_THREADS,
                   bool idle_cap[] USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    uint32_t i;

    if (n_gc_threads == 1) return;

    for (i=0; i < n_gc_threads; i++) {
        if (i == me || idle_cap[i]) continue;
        inc_running();
        debugTrace(DEBUG_gc, "waking up gc thread %d", i);
        if (gc_threads[i]->wakeup != GC_THREAD_STANDING_BY)
            barf("wakeup_gc_threads");

        gc_threads[i]->wakeup = GC_THREAD_RUNNING;
        ACQUIRE_SPIN_LOCK(&gc_threads[i]->mut_spin);
        RELEASE_SPIN_LOCK(&gc_threads[i]->gc_spin);
    }
#endif
}

// After GC is complete, we must wait for all GC threads to enter the
// standby state, otherwise they may still be executing inside
// any_work(), and may even remain awake until the next GC starts.
static void
shutdown_gc_threads (uint32_t me USED_IF_THREADS,
                     bool idle_cap[] USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    uint32_t i;

    if (n_gc_threads == 1) return;

    for (i=0; i < n_gc_threads; i++) {
        if (i == me || idle_cap[i]) continue;
        while (gc_threads[i]->wakeup != GC_THREAD_WAITING_TO_CONTINUE) {
            busy_wait_nop();
            write_barrier();
        }
    }
#endif
}

#if defined(THREADED_RTS)
void
releaseGCThreads (Capability *cap USED_IF_THREADS, bool idle_cap[])
{
    const uint32_t n_threads = n_capabilities;
    const uint32_t me = cap->no;
    uint32_t i;
    for (i=0; i < n_threads; i++) {
        if (i == me || idle_cap[i]) continue;
        if (gc_threads[i]->wakeup != GC_THREAD_WAITING_TO_CONTINUE)
            barf("releaseGCThreads");

        gc_threads[i]->wakeup = GC_THREAD_INACTIVE;
        ACQUIRE_SPIN_LOCK(&gc_threads[i]->gc_spin);
        RELEASE_SPIN_LOCK(&gc_threads[i]->mut_spin);
    }
}
#endif

/* ----------------------------------------------------------------------------
   Save the mutable lists in saved_mut_lists where it will be scavenged
   during GC
   ------------------------------------------------------------------------- */

static void
stash_mut_list (Capability *cap, uint32_t gen_no)
{
    cap->saved_mut_lists[gen_no] = cap->mut_lists[gen_no];
    cap->mut_lists[gen_no] = allocBlockOnNode_sync(cap->node);
}

/* ----------------------------------------------------------------------------
   Initialise a generation that is to be collected
   ------------------------------------------------------------------------- */

static void
prepare_collected_gen (generation *gen)
{
    uint32_t i, g, n;
    gen_workspace *ws;
    bdescr *bd, *next;

    g = gen->no;

    if (RtsFlags.GcFlags.useNonmoving && g == oldest_gen->no) {
        // Nonmoving heap's mutable list is always a root.
        for (i = 0; i < n_capabilities; i++) {
            stash_mut_list(capabilities[i], g);
        }
    } else if (g != 0) {
        // Otherwise throw away the current mutable list. Invariant: the
        // mutable list always has at least one block; this means we can avoid
        // a check for NULL in recordMutable().
        for (i = 0; i < n_capabilities; i++) {
            freeChain(capabilities[i]->mut_lists[g]);
            capabilities[i]->mut_lists[g] =
                allocBlockOnNode(capNoToNumaNode(i));
        }
    }

    gen = &generations[g];
    ASSERT(gen->no == g);

    // we'll construct a new list of threads in this step
    // during GC, throw away the current list.
    gen->old_threads = gen->threads;
    gen->threads = END_TSO_QUEUE;

    // deprecate the existing blocks (except in the case of the nonmoving
    // collector since these will be preserved in nonmovingCollect for the
    // concurrent GC).
    if (!(RtsFlags.GcFlags.useNonmoving && g == oldest_gen->no)) {
        gen->old_blocks   = gen->blocks;
        gen->n_old_blocks = gen->n_blocks;
        gen->blocks       = NULL;
        gen->n_blocks     = 0;
        gen->n_words      = 0;
        gen->live_estimate = 0;
    }

    // initialise the large object queues.
    ASSERT(gen->scavenged_large_objects == NULL);
    ASSERT(gen->n_scavenged_large_blocks == 0);
    ASSERT(gen->live_compact_objects == NULL);
    ASSERT(gen->n_live_compact_blocks == 0);

    // grab all the partial blocks stashed in the gc_thread workspaces and
    // move them to the old_blocks list of this gen.
    for (n = 0; n < n_capabilities; n++) {
        ws = &gc_threads[n]->gens[gen->no];

        for (bd = ws->part_list; bd != NULL; bd = next) {
            next = bd->link;
            bd->link = gen->old_blocks;
            gen->old_blocks = bd;
            gen->n_old_blocks += bd->blocks;
        }
        ws->part_list = NULL;
        ws->n_part_blocks = 0;
        ws->n_part_words = 0;

        ASSERT(ws->scavd_list == NULL);
        ASSERT(ws->n_scavd_blocks == 0);
        ASSERT(ws->n_scavd_words == 0);

        if (ws->todo_free != ws->todo_bd->start) {
            ws->todo_bd->free = ws->todo_free;
            ws->todo_bd->link = gen->old_blocks;
            gen->old_blocks = ws->todo_bd;
            gen->n_old_blocks += ws->todo_bd->blocks;
            alloc_todo_block(ws,0); // always has one block.
        }
    }

    // mark the small objects as from-space
    for (bd = gen->old_blocks; bd; bd = bd->link) {
        bd->flags &= ~BF_EVACUATED;
    }

    // mark the large objects as from-space
    for (bd = gen->large_objects; bd; bd = bd->link) {
        bd->flags &= ~BF_EVACUATED;
    }

    // mark the compact objects as from-space
    for (bd = gen->compact_objects; bd; bd = bd->link) {
        bd->flags &= ~BF_EVACUATED;
    }

    // for a compacted generation, we need to allocate the bitmap
    if (gen->mark) {
        StgWord bitmap_size; // in bytes
        bdescr *bitmap_bdescr;
        StgWord *bitmap;

        bitmap_size = gen->n_old_blocks * BLOCK_SIZE / BITS_IN(W_);

        if (bitmap_size > 0) {
            bitmap_bdescr = allocGroup((StgWord)BLOCK_ROUND_UP(bitmap_size)
                                       / BLOCK_SIZE);
            gen->bitmap = bitmap_bdescr;
            bitmap = bitmap_bdescr->start;

            debugTrace(DEBUG_gc, "bitmap_size: %d, bitmap: %p",
                       bitmap_size, bitmap);

            // don't forget to fill it with zeros!
            memset(bitmap, 0, bitmap_size);

            // For each block in this step, point to its bitmap from the
            // block descriptor.
            for (bd=gen->old_blocks; bd != NULL; bd = bd->link) {
                bd->u.bitmap = bitmap;
                bitmap += BLOCK_SIZE_W / BITS_IN(W_);

                // Also at this point we set the BF_MARKED flag
                // for this block.  The invariant is that
                // BF_MARKED is always unset, except during GC
                // when it is set on those blocks which will be
                // compacted.
                if (!(bd->flags & BF_FRAGMENTED)) {
                    bd->flags |= BF_MARKED;
                }

                // BF_SWEPT should be marked only for blocks that are being
                // collected in sweep()
                bd->flags &= ~BF_SWEPT;
            }
        }
    }
}

/* ----------------------------------------------------------------------------
   Initialise a generation that is *not* to be collected
   ------------------------------------------------------------------------- */

static void
prepare_uncollected_gen (generation *gen)
{
    uint32_t i;


    ASSERT(gen->no > 0);

    // save the current mutable lists for this generation, and
    // allocate a fresh block for each one.  We'll traverse these
    // mutable lists as roots early on in the GC.
    for (i = 0; i < n_capabilities; i++) {
        stash_mut_list(capabilities[i], gen->no);
    }

    ASSERT(gen->scavenged_large_objects == NULL);
    ASSERT(gen->n_scavenged_large_blocks == 0);
}

/* -----------------------------------------------------------------------------
   Collect the completed blocks from a GC thread and attach them to
   the generation.
   -------------------------------------------------------------------------- */

static void
collect_gct_blocks (void)
{
    uint32_t g;
    gen_workspace *ws;
    bdescr *bd, *prev;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        ws = &gct->gens[g];

        // there may still be a block attached to ws->todo_bd;
        // leave it there to use next time.

        if (ws->scavd_list != NULL) {
            ACQUIRE_SPIN_LOCK(&ws->gen->sync);

            ASSERT(gct->scan_bd == NULL);
            ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks);

            prev = NULL;
            for (bd = ws->scavd_list; bd != NULL; bd = bd->link) {
                prev = bd;
            }
            if (prev != NULL) {
                prev->link = ws->gen->blocks;
                ws->gen->blocks = ws->scavd_list;
            }
            ws->gen->n_blocks += ws->n_scavd_blocks;
            ws->gen->n_words += ws->n_scavd_words;

            ws->scavd_list = NULL;
            ws->n_scavd_blocks = 0;
            ws->n_scavd_words = 0;

            RELEASE_SPIN_LOCK(&ws->gen->sync);
        }
    }
}

/* -----------------------------------------------------------------------------
   During mutation, any blocks that are filled by allocatePinned() are stashed
   on the local pinned_object_blocks list, to avoid needing to take a global
   lock.  Here we collect those blocks from the cap->pinned_object_blocks lists
   and put them on the g0->large_object or oldest_gen->large_objects.

   How to decide which list to put them on?

   - When non-moving heap is enabled and this is a major GC, we put them on
     oldest_gen. This is because after preparation we really want no
     old-to-young references, and we want to be able to reset mut_lists. For
     this we need to promote every potentially live object to the oldest gen.

   - Otherwise we put them on g0.
   -------------------------------------------------------------------------- */

static void
collect_pinned_object_blocks (void)
{
    generation *gen;
    const bool use_nonmoving = RtsFlags.GcFlags.useNonmoving;
    if (use_nonmoving && major_gc) {
        gen = oldest_gen;
    } else {
        gen = g0;
    }

    for (uint32_t n = 0; n < n_capabilities; n++) {
        bdescr *last = NULL;
        if (use_nonmoving && gen == oldest_gen) {
            // Mark objects as belonging to the nonmoving heap
            for (bdescr *bd = capabilities[n]->pinned_object_blocks; bd != NULL; bd = bd->link) {
                bd->flags |= BF_NONMOVING;
                bd->gen = oldest_gen;
                bd->gen_no = oldest_gen->no;
                oldest_gen->n_large_words += bd->free - bd->start;
                oldest_gen->n_large_blocks += bd->blocks;
                last = bd;
            }
        } else {
            for (bdescr *bd = capabilities[n]->pinned_object_blocks; bd != NULL; bd = bd->link) {
                last = bd;
            }
        }

        if (last != NULL) {
            last->link = gen->large_objects;
            if (gen->large_objects != NULL) {
                gen->large_objects->u.back = last;
            }
            gen->large_objects = capabilities[n]->pinned_object_blocks;
            capabilities[n]->pinned_object_blocks = NULL;
        }
    }
}

/* -----------------------------------------------------------------------------
   Initialise a gc_thread before GC
   -------------------------------------------------------------------------- */

static void
init_gc_thread (gc_thread *t)
{
    t->static_objects = END_OF_STATIC_OBJECT_LIST;
    t->scavenged_static_objects = END_OF_STATIC_OBJECT_LIST;
    t->scan_bd = NULL;
    t->mut_lists = t->cap->mut_lists;
    t->evac_gen_no = 0;
    t->failed_to_evac = false;
    t->eager_promotion = true;
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
mark_root(void *user USED_IF_THREADS, StgClosure **root)
{
    // we stole a register for gct, but this function is called from
    // *outside* the GC where the register variable is not in effect,
    // so we need to save and restore it here.  NB. only call
    // mark_root() from the main GC thread, otherwise gct will be
    // incorrect.
#if defined(THREADED_RTS)
    gc_thread *saved_gct;
    saved_gct = gct;
#endif
    SET_GCT(user);

    evacuate(root);

    SET_GCT(saved_gct);
}

/* ----------------------------------------------------------------------------
   Reset the sizes of the older generations when we do a major
   collection.

   CURRENT STRATEGY: make all generations except zero the same size.
   We have to stay within the maximum heap size, and leave a certain
   percentage of the maximum heap size available to allocate into.
   ------------------------------------------------------------------------- */

void
resizeGenerations (void)
{
    uint32_t g;
    W_ live, size, min_alloc, words;
    const W_ max  = RtsFlags.GcFlags.maxHeapSize;
    const W_ gens = RtsFlags.GcFlags.generations;

    // live in the oldest generations
    if (oldest_gen->live_estimate != 0) {
        words = oldest_gen->live_estimate;
    } else {
        words = oldest_gen->n_words;
    }
    live = (words + BLOCK_SIZE_W - 1) / BLOCK_SIZE_W +
        oldest_gen->n_large_blocks +
        oldest_gen->n_compact_blocks;

    // default max size for all generations except zero
    size = stg_max(live * RtsFlags.GcFlags.oldGenFactor,
                   RtsFlags.GcFlags.minOldGenSize);

    if (RtsFlags.GcFlags.heapSizeSuggestionAuto) {
        if (max > 0) {
            RtsFlags.GcFlags.heapSizeSuggestion = stg_min(max, size);
        } else {
            RtsFlags.GcFlags.heapSizeSuggestion = size;
        }
    }

    // minimum size for generation zero
    min_alloc = stg_max((RtsFlags.GcFlags.pcFreeHeap * max) / 200,
                        RtsFlags.GcFlags.minAllocAreaSize
                        * (W_)n_capabilities);

    // Auto-enable compaction when the residency reaches a
    // certain percentage of the maximum heap size (default: 30%).
    // Except when non-moving GC is enabled.
    if (!RtsFlags.GcFlags.useNonmoving &&
        (RtsFlags.GcFlags.compact ||
         (max > 0 &&
          oldest_gen->n_blocks >
          (RtsFlags.GcFlags.compactThreshold * max) / 100))) {
        oldest_gen->mark = 1;
        oldest_gen->compact = 1;
//        debugBelch("compaction: on\n", live);
    } else {
        oldest_gen->mark = 0;
        oldest_gen->compact = 0;
//        debugBelch("compaction: off\n", live);
    }

    if (RtsFlags.GcFlags.sweep) {
        oldest_gen->mark = 1;
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

        if (oldest_gen->compact) {
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
    debugBelch("resize_gen: n_blocks: %lu, n_large_block: %lu, n_compact_blocks: %lu\n",
               oldest_gen->n_blocks, oldest_gen->n_large_blocks, oldest_gen->n_compact_blocks);
    debugBelch("resize_gen: max_blocks: %lu -> %lu\n", oldest_gen->max_blocks, oldest_gen->n_blocks);
#endif

    for (g = 0; g < gens; g++) {
        generations[g].max_blocks = size;
    }
}

/* -----------------------------------------------------------------------------
   Calculate the new size of the nursery, and resize it.
   -------------------------------------------------------------------------- */

static void
resize_nursery (void)
{
    const StgWord min_nursery =
      RtsFlags.GcFlags.minAllocAreaSize * (StgWord)n_capabilities;

    if (RtsFlags.GcFlags.generations == 1)
    {   // Two-space collector:
        W_ blocks;

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
        blocks = generations[0].n_blocks;

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
            if (blocks < min_nursery)
            {
                blocks = min_nursery;
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
            StgWord needed;

            calcNeeded(false, &needed); // approx blocks needed at next GC

            /* Guess how much will be live in generation 0 step 0 next time.
             * A good approximation is obtained by finding the
             * percentage of g0 that was live at the last minor GC.
             *
             * We have an accurate figure for the amount of copied data in
             * 'copied', but we must convert this to a number of blocks, with
             * a small adjustment for estimated slop at the end of a block
             * (- 10 words).
             */
            if (N == 0)
            {
                g0_pcnt_kept = ((copied / (BLOCK_SIZE_W - 10)) * 100)
                    / countNurseryBlocks();
            }

            /* Estimate a size for the allocation area based on the
             * information available.  We might end up going slightly under
             * or over the suggested heap size, but we should be pretty
             * close on average.
             *
             * Formula:            suggested - needed
             *                ----------------------------
             *                    1 + g0_pcnt_kept/100
             *
             * where 'needed' is the amount of memory needed at the next
             * collection for collecting all gens except g0.
             */
            blocks =
                (((long)RtsFlags.GcFlags.heapSizeSuggestion - (long)needed) * 100) /
                (100 + (long)g0_pcnt_kept);

            if (blocks < (long)min_nursery) {
                blocks = min_nursery;
            }

            resizeNurseries((W_)blocks);
        }
        else
        {
            // we might have added extra blocks to the nursery, so
            // resize back to the original size again.
            resizeNurseriesFixed();
        }
    }
}

/* -----------------------------------------------------------------------------
   Sanity code for CAF garbage collection.

   With DEBUG turned on, we manage a CAF list in addition to the SRT
   mechanism.  After GC, we run down the CAF list and make any
   CAFs which have been garbage collected GCD_CAF.  This means we get an error
   whenever the program tries to enter a garbage collected CAF.

   Any garbage collected CAFs are taken off the CAF list at the same
   time.
   -------------------------------------------------------------------------- */

#if defined(DEBUG)

void gcCAFs(void)
{
    uint32_t i = 0;
    StgIndStatic *prev = NULL;

    for (StgIndStatic *p = debug_caf_list;
         p != (StgIndStatic*) END_OF_CAF_LIST;
         p = (StgIndStatic*) p->saved_info)
    {
        const StgInfoTable *info = get_itbl((StgClosure*)p);
        ASSERT(info->type == IND_STATIC);

        // See Note [STATIC_LINK fields] in Storage.h
        // This condition identifies CAFs that have just been GC'd and
        // don't have static_link==3 which means they should be ignored.
        if ((((StgWord)(p->static_link)&STATIC_BITS) | prev_static_flag) != 3) {
            debugTrace(DEBUG_gccafs, "CAF gc'd at %p", p);
            SET_INFO((StgClosure*)p,&stg_GCD_CAF_info); // stub it
            if (prev == NULL) {
                debug_caf_list = (StgIndStatic*)p->saved_info;
            } else {
                prev->saved_info = p->saved_info;
            }
        } else {
            prev = p;
            i++;
        }
    }

    debugTrace(DEBUG_gccafs, "%d CAFs live", i);
}
#endif


/* -----------------------------------------------------------------------------
   The GC can leave some work for the mutator to do before the next
   GC, provided the work can be safely overlapped with mutation.  This
   can help reduce the GC pause time.

   The mutator can call doIdleGCWork() any time it likes, but
   preferably when it is idle.  It's safe for multiple capabilities to
   call doIdleGCWork().

   When 'all' is
     * false: doIdleGCWork() should only take a short, bounded, amount
       of time.
     * true: doIdleGCWork() will complete all the outstanding GC work.

   The return value is
     * true if there's more to do (only if 'all' is false).
     * false otherwise.
  -------------------------------------------------------------------------- */

bool doIdleGCWork(Capability *cap STG_UNUSED, bool all)
{
    return runSomeFinalizers(all);
}
