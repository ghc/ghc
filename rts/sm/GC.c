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

#include "rts/PosixSource.h"
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
#include "Proftimer.h"
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
#include "Ticky.h"

#include <stdalign.h>
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

/* Hot GC globals
 * ~~~~~~~~~~~~~~
 * The globals below are quite hot during GC but read-only, initialized during
 * the beginning of collection. It is important that they reside in the same
 * cache-line to minimize unnecessary cache misses.
 */

/* N is the oldest generation being collected, where the generations
 * are numbered starting at 0.  A major GC (indicated by the major_gc
 * flag) is when we're collecting all generations.  We only attempt to
 * deal with static objects and GC CAFs when doing a major GC.
 */
uint32_t N;
bool major_gc;
bool deadlock_detect_gc;
bool unload_mark_needed;

/* Data used for allocation area sizing.
 */
static W_ g0_pcnt_kept = 30; // percentage of g0 live at last minor GC

static int consec_idle_gcs = 0;

/* Mut-list stats */
#if defined(DEBUG)
// For lack of a better option we protect mutlist_scav_stats with oldest_gen->sync
MutListScavStats mutlist_scav_stats;
#endif

/* Thread-local data for each GC thread
 */
gc_thread **gc_threads = NULL;


// see Note [Synchronising work stealing]
static StgWord gc_running_threads;

#if defined(THREADED_RTS)

static Mutex gc_running_mutex;
static Condition gc_running_cv;

static Mutex gc_entry_mutex;
static StgInt n_gc_entered = 0;
static Condition gc_entry_arrived_cv;
static Condition gc_entry_start_now_cv;

static Mutex gc_exit_mutex;
static StgInt n_gc_exited = 0;
static Condition gc_exit_arrived_cv;
static Condition gc_exit_leave_now_cv;

#else // THREADED_RTS
// Must be aligned to 64-bytes to meet stated 64-byte alignment of gen_workspace
StgWord8 the_gc_thread[sizeof(gc_thread) + 64 * sizeof(gen_workspace)]
    ATTRIBUTE_ALIGNED(64);
#endif // THREADED_RTS

/* Note [n_gc_threads]
   ~~~~~~~~~~~~~~~~~~~
This is a global variable that originally tracked the number of threads
participating in the current gc. It's meaning has diverged from this somewhat,
as it does not distinguish betweeen idle and non-idle threads. An idle thread
is a thread i where `idle[i]` is true, using the idle array passed to
GarbageCollect.

In practice, it now takes one of the values {1, n_capabilities}, when a SYNC_GC_SEQ
is requested it takes 1, when a SYNC_GC_PAR is requested it takes n_capabilities.

Clearly this is in need of some tidying up, but for now we tread carefully. We
call is_par_gc() to see whether we are in a parallel or sequential collection.
If we are in a parallel collection we iterate over gc_threads, being careful to
account for idle caps. If we are in a sequential collection we deal only with
the thread local gct.
Of course this is valid only inside GarbageCollect ().

Omitting this check has led to issues such as #19147.
*/

static bool is_par_gc(void);

uint32_t n_gc_threads;
static uint32_t n_gc_idle_threads;
bool work_stealing;

static bool is_par_gc(void) {
#if defined(THREADED_RTS)
    if(n_gc_threads == 1) { return false; }
    ASSERT(n_gc_threads > n_gc_idle_threads);
    return n_gc_threads - n_gc_idle_threads > 1;
#else
    return false;
#endif
}

// For stats:
static long copied;        // *words* copied & scavenged during this GC

#if defined(PROF_SPIN) && defined(THREADED_RTS)
// spin and yield counts for the quasi-SpinLock in waitForGcThreads
volatile StgWord64 waitForGcThreads_spin = 0;
volatile StgWord64 waitForGcThreads_yield = 0;
volatile StgWord64 whitehole_gc_spin = 0;
#endif // PROF_SPIN

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
   Statistics from mut_list scavenging
   -------------------------------------------------------------------------- */

#if defined(DEBUG)
void
zeroMutListScavStats(MutListScavStats *src)
{
    memset(src, 0, sizeof(MutListScavStats));
}

void
addMutListScavStats(const MutListScavStats *src,
                    MutListScavStats *dest)
{
#define ADD_STATS(field) dest->field += src->field;
    ADD_STATS(n_MUTVAR);
    ADD_STATS(n_MUTARR);
    ADD_STATS(n_MVAR);
    ADD_STATS(n_TVAR);
    ADD_STATS(n_TREC_CHUNK);
    ADD_STATS(n_TVAR_WATCH_QUEUE);
    ADD_STATS(n_TREC_HEADER);
    ADD_STATS(n_OTHERS);
#undef ADD_STATS
}
#endif /* DEBUG */


/* -----------------------------------------------------------------------------
   GarbageCollect: the main entry point to the garbage collector.

   The collect_gen parameter is gotten by calling calcNeeded().

   Locks held: all capabilities are held throughout GarbageCollect().
   -------------------------------------------------------------------------- */

void
GarbageCollect (struct GcConfig config,
                Capability *cap,
                bool idle_cap[])
{
  bdescr *bd;
  generation *gen;
  StgWord live_blocks, live_words, par_max_copied, par_balanced_copied,
      any_work, scav_find_work, max_n_todo_overflow;
#if defined(THREADED_RTS)
  gc_thread *saved_gct;
  bool gc_sparks_all_caps;
#endif
  uint32_t g, n;
  // The time we should report our heap census as occurring at, if necessary.
  Time mut_time = 0;

  if (config.do_heap_census) {
      RTSStats stats;
      getRTSStats(&stats);
      mut_time = stats.mutator_cpu_ns;
  }

  // necessary if we stole a callee-saves register for gct:
#if defined(THREADED_RTS)
  saved_gct = gct;
#else
  ASSERT(!config.parallel);
#endif

#if defined(PROFILING)
  CostCentreStack *save_CCS[getNumCapabilities()];
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
  zeroMutListScavStats(&mutlist_scav_stats);
#endif

  // attribute any costs to CCS_GC
#if defined(PROFILING)
  for (n = 0; n < getNumCapabilities(); n++) {
      save_CCS[n] = RELAXED_LOAD(&getCapability(n)->r.rCCCS);
      RELAXED_STORE(&getCapability(n)->r.rCCCS, CCS_GC);
  }
#endif

  /* Figure out which generation to collect
   */
  N = config.collect_gen;
  major_gc = (N == RtsFlags.GcFlags.generations-1);

  /* See Note [Deadlock detection under the nonmoving collector]. */
  deadlock_detect_gc = config.deadlock_detect;

#if defined(THREADED_RTS)
  if (major_gc && RtsFlags.GcFlags.useNonmoving && nonmovingConcurrentMarkIsRunning()) {
      /* If there is already a concurrent major collection running then
       * there is no benefit to starting another.
       * TODO: Catch heap-size runaway.
       */
      N--;
      config.collect_gen--;
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

  /* N.B. We currently don't unload code with the non-moving collector. */
  if (major_gc && !RtsFlags.GcFlags.useNonmoving) {
      unload_mark_needed = prepareUnloadCheck();
  } else {
      unload_mark_needed = false;
  }

#if defined(THREADED_RTS)
  /* How many threads will be participating in this GC?
   * We don't always parallelise minor GCs, or mark/compact/sweep GC.
   * The policy on when to do a parallel GC is controlled by RTS flags (see
   * below)

   * There are subtleties here. In the PAR case, we copy n_gc_threads from
   * n_capabilities, presumably so that n_capabailites doesn' change under us. I
   * don't understand quite how that happens, but the test setnumcapabilities001
   * demonstrates it.
   *
   * we set n_gc_threads, work_stealing, n_gc_idle_threads, gc_running_threads
   * here
  */
  if (config.parallel) {
      n_gc_threads = getNumCapabilities();
      n_gc_idle_threads = 0;
      for (uint32_t i = 0; i < getNumCapabilities(); ++i) {
          if (idle_cap[i]) {
              ASSERT(i != gct->thread_index);
              ++n_gc_idle_threads;
          }
      }
  } else {
      n_gc_threads = 1;
      n_gc_idle_threads = getNumCapabilities() - 1;
  }
  work_stealing = RtsFlags.ParFlags.parGcLoadBalancingEnabled &&
      N >= RtsFlags.ParFlags.parGcLoadBalancingGen &&
      is_par_gc();
      // It's not always a good idea to do load balancing in parallel
      // GC.  In particular, for a parallel program we don't want to
      // lose locality by moving cached data into another CPU's cache
      // (this effect can be quite significant).
      //
      // We could have a more complex way to determine whether to do
      // work stealing or not, e.g. it might be a good idea to do it
      // if the heap is big.  For now, we just turn it on or off with
      // a flag.
#else
  n_gc_threads = 1;
  work_stealing = false;
  n_gc_idle_threads = 0;
#endif

  SEQ_CST_STORE(&gc_running_threads, 0);

  ASSERT(n_gc_threads > 0);
  ASSERT(n_gc_threads <= getNumCapabilities());
  ASSERT(n_gc_idle_threads < getNumCapabilities());
  // If we are work stealing, there better be another(i.e. not us) non-idle gc
  // thread
  ASSERT(!work_stealing || n_gc_threads - 1 > n_gc_idle_threads);


  debugTrace(DEBUG_gc, "GC (gen %d, using %d thread(s), %s work stealing)",
             N, (int)getNumCapabilities() - (int)n_gc_idle_threads,
             work_stealing ? "with": "without");

#if defined(DEBUG)
  // check for memory leaks if DEBUG is on
  memInventory(DEBUG_gc);
#endif

  // Defer all free calls for the megablock allocator to avoid quadratic runtime
  // explosion when freeing a lot of memory in a single GC
  // (https://gitlab.haskell.org/ghc/ghc/-/issues/19897).
  deferMBlockFreeing();

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
      mark_sp           = bdescr_start(mark_stack_bd);
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
  if (!is_par_gc()) {
      for (n = 0; n < getNumCapabilities(); n++) {
#if defined(THREADED_RTS)
          scavenge_capability_mut_Lists1(getCapability(n));
#else
          scavenge_capability_mut_lists(getCapability(n));
#endif
      }
  } else {
      scavenge_capability_mut_lists(gct->cap);
      for (n = 0; n < getNumCapabilities(); n++) {
          if (idle_cap[n]) {
              markCapability(mark_root, gct, getCapability(n),
                             true/*don't mark sparks*/);
              scavenge_capability_mut_lists(getCapability(n));
          }
      }
  }

  // follow roots from the CAF list (used by GHCi)
  gct->evac_gen_no = 0;
  markCAFs(mark_root, gct);

  // follow all the roots that the application knows about.
  gct->evac_gen_no = 0;
  if (!is_par_gc()) {
      for (n = 0; n < getNumCapabilities(); n++) {
          markCapability(mark_root, gct, getCapability(n),
                         true/*don't mark sparks*/);
      }
  } else {
      markCapability(mark_root, gct, cap, true/*don't mark sparks*/);
  }

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
   * see Note [Synchronising work stealing]
   */
  scavenge_until_all_done();
  shutdown_gc_threads(gct->thread_index, idle_cap);

  StgWeak *dead_weak_ptr_list = NULL;
  StgTSO *resurrected_threads = END_TSO_QUEUE;
  // must be last...  invariant is that everything is fully
  // scavenged at this point.
#if defined(THREADED_RTS)
  gc_sparks_all_caps = !work_stealing || !is_par_gc();
#endif
  work_stealing = false;
  while (traverseWeakPtrList(&dead_weak_ptr_list, &resurrected_threads))
  {
      inc_running();
      scavenge_until_all_done();
  }


  // Now see which stable names are still alive.
  gcStableNameTable();

#if defined(THREADED_RTS)
  // See Note [Pruning the spark pool]
  if(gc_sparks_all_caps) {
      for (n = 0; n < n_capabilities; n++) {
          pruneSparkQueue(false, getCapability(n));
      }
  } else {
      for (n = 0; n < getNumCapabilities(); n++) {
          if (n == cap->no || idle_cap[n]) {
              pruneSparkQueue(false, getCapability(n));
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
  any_work = 0;
  scav_find_work = 0;
  max_n_todo_overflow = 0;
  {
      uint32_t i;
      uint64_t par_balanced_copied_acc = 0;
      const gc_thread* thread;

      // see Note [n_gc_threads]
      if (is_par_gc()) {
          int other_active_threads = n_gc_threads - n_gc_idle_threads - 1;
          ASSERT(other_active_threads > 0);

          for (i=0; i < n_gc_threads; i++) {
              if(idle_cap[i]) { continue; }
              copied += RELAXED_LOAD(&gc_threads[i]->copied);
          }
          for (i=0; i < n_gc_threads; i++) {
              if(idle_cap[i]) { continue; }
              thread = gc_threads[i];
              debugTrace(DEBUG_gc,"thread %d:", i);
              debugTrace(DEBUG_gc,"   copied           %ld",
                         RELAXED_LOAD(&thread->copied) * sizeof(W_));
              debugTrace(DEBUG_gc,"   scanned          %ld",
                         RELAXED_LOAD(&thread->scanned) * sizeof(W_));
              debugTrace(DEBUG_gc,"   any_work         %ld",
                         RELAXED_LOAD(&thread->any_work));
              debugTrace(DEBUG_gc,"   scav_find_work %ld",
                         RELAXED_LOAD(&thread->scav_find_work));

              any_work += RELAXED_LOAD(&thread->any_work);
              scav_find_work += RELAXED_LOAD(&thread->scav_find_work);
              max_n_todo_overflow = stg_max(RELAXED_LOAD(&thread->max_n_todo_overflow), max_n_todo_overflow);

              par_max_copied = stg_max(RELAXED_LOAD(&thread->copied), par_max_copied);
              par_balanced_copied_acc +=
                  stg_min((other_active_threads + 1) * RELAXED_LOAD(&thread->copied), copied);
          }

          // See Note [Work Balance] for an explanation of this computation
          par_balanced_copied =
              (par_balanced_copied_acc - copied + other_active_threads / 2) /
              other_active_threads;
      } else {
          copied += gct->copied;
          any_work += gct->any_work;
          scav_find_work += gct->scav_find_work;
          max_n_todo_overflow += gct->max_n_todo_overflow;
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
      if (is_par_gc()) generations[g].par_collections++;
    }

    // Count the mutable list as bytes "copied" for the purposes of
    // stats.  Every mutable list is copied during every GC.
    if (g > 0) {
        W_ mut_list_size = 0;
        for (n = 0; n < getNumCapabilities(); n++) {
            mut_list_size += countOccupied(getCapability(n)->mut_lists[g]);
        }
        copied +=  mut_list_size;

#if defined(DEBUG)
        debugTrace(DEBUG_gc,
                   "mut_list_size: %lu (%d vars, %d arrays, %d MVARs, %d TVARs, %d TVAR_WATCH_QUEUEs, %d TREC_CHUNKs, %d TREC_HEADERs, %d others)",
                   (unsigned long)(mut_list_size * sizeof(W_)),
                   mutlist_scav_stats.n_MUTVAR,
                   mutlist_scav_stats.n_MUTARR,
                   mutlist_scav_stats.n_MVAR,
                   mutlist_scav_stats.n_TVAR,
                   mutlist_scav_stats.n_TVAR_WATCH_QUEUE,
                   mutlist_scav_stats.n_TREC_CHUNK,
                   mutlist_scav_stats.n_TREC_HEADER,
                   mutlist_scav_stats.n_OTHERS);
#endif
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
                        gen->n_words += bdescr_free(bd) - bdescr_start(bd);

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
            compactFree(((StgCompactNFDataBlock*)bdescr_start(bd))->owner);
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
            gen->n_large_words += bdescr_free(bd) - bdescr_start(bd);
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
    for (uint32_t i = 0; i < getNumCapabilities(); i++) {
        live_words  += gcThreadLiveWords(i, gen->no);
        live_blocks += gcThreadLiveBlocks(i, gen->no);
    }
  } // for all generations

  // Flush the update remembered sets. See Note [Eager update remembered set
  // flushing] in NonMovingMark.c
  if (RtsFlags.GcFlags.useNonmoving) {
      for (n = 0; n < getNumCapabilities(); n++) {
          nonmovingAddUpdRemSetBlocks(&getCapability(n)->upd_rem_set);
      }
  }

  // Mark and sweep the oldest generation.
  // N.B. This can only happen after we've moved
  // oldest_gen->scavenged_large_objects back to oldest_gen->large_objects.
  ASSERT(oldest_gen->scavenged_large_objects == NULL);
  if (RtsFlags.GcFlags.useNonmoving && major_gc) {
      bool concurrent = false;

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

#if defined(THREADED_RTS)
      // Concurrent collection is currently incompatible with heap profiling.
      // See Note [Non-concurrent nonmoving collector heap census]
      concurrent = !config.nonconcurrent && !RtsFlags.ProfFlags.doHeapProfile;
#else
      // In the non-threaded runtime this is the only time we push to the
      // upd_rem_set
      nonmovingAddUpdRemSetBlocks(&gct->cap->upd_rem_set);
#endif

      // dead_weak_ptr_list contains weak pointers with dead keys. Those need to
      // be kept alive because we'll use them in finalizeSchedulers(). Similarly
      // resurrected_threads are also going to be used in resurrectedThreads()
      // so we need to mark those too.
      // Note that in sequential case these lists will be appended with more
      // weaks and threads found to be dead in mark.
      nonmovingCollect(&dead_weak_ptr_list, &resurrected_threads, concurrent);
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
  resetStaticObjectForProfiling(&g_retainerTraverseState, gct->scavenged_static_objects);
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
  if (config.do_heap_census) {
      debugTrace(DEBUG_sched, "performing heap census");
      RELEASE_SM_LOCK;
      heapCensus(mut_time);
      ACQUIRE_SM_LOCK;
  }

#if defined(TICKY_TICKY)
  // Post ticky counter sample.
  // We do this at the end of execution since tickers are registered in the
  // course of program execution.
  if (RELAXED_LOAD(&performTickySample)) {
      emitTickyCounterSamples();
      RELAXED_STORE(&performTickySample, false);
  }
#endif

  // send exceptions to any threads which were about to die
  RELEASE_SM_LOCK;
  resurrectThreads(resurrected_threads);
  ACQUIRE_SM_LOCK;

  // Finally free the deferred mblocks by sorting the deferred free list and
  // merging it into the actual sorted free list. This needs to happen here so
  // that the `returnMemoryToOS` call down below can successfully free memory.
  commitMBlockFreeing();

  if (major_gc) {
      W_ need_prealloc, need_copied_live, need_uncopied_live, need, got, extra_needed;
      uint32_t i;

      need_copied_live = 0;
      need_uncopied_live = 0;
      for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
          need_copied_live += genLiveCopiedWords(&generations[i]);
          need_uncopied_live += genLiveUncopiedWords(&generations[i]);
      }

      // Convert the live words into live blocks
      // See Note [Statistics for retaining memory]
      need_copied_live = BLOCK_ROUND_UP(need_copied_live) / BLOCK_SIZE_W;
      need_uncopied_live = BLOCK_ROUND_UP(need_uncopied_live) / BLOCK_SIZE_W;

      debugTrace(DEBUG_gc, "(before) copied_live: %d; uncopied_live: %d", need_copied_live, need_uncopied_live );


      // minOldGenSize states that the size of the oldest generation must be at least
      // as big as a certain value, so make sure to save enough memory for that.
      extra_needed = 0;
      if (RtsFlags.GcFlags.minOldGenSize >= need_copied_live + need_uncopied_live){
        extra_needed = RtsFlags.GcFlags.minOldGenSize - (need_copied_live + need_uncopied_live);
      }
      debugTrace(DEBUG_gc, "(minOldGen: %d; extra_needed: %d", RtsFlags.GcFlags.minOldGenSize, extra_needed);

      // If oldest gen is uncopying in some manner (compact or non-moving) then
      // add the extra requested by minOldGenSize to uncopying portion of memory.
      // Otherwise, the last generation is copying so add it to copying portion.
      if (oldest_gen -> compact || RtsFlags.GcFlags.useNonmoving) {
        need_uncopied_live += extra_needed;
      }
      else {
        need_copied_live += extra_needed;
      }

      ASSERT(need_uncopied_live + need_copied_live >= RtsFlags.GcFlags.minOldGenSize );

      debugTrace(DEBUG_gc, "(after) copied_live: %d; uncopied_live: %d", need_copied_live, need_uncopied_live );

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

      // Reset the counter if the major GC was caused by a heap overflow
      consec_idle_gcs = config.overflow_gc ? 0 : consec_idle_gcs + 1;

      // See Note [Scaling retained memory]
      double scaled_factor =
        RtsFlags.GcFlags.returnDecayFactor > 0
          ? RtsFlags.GcFlags.oldGenFactor / pow(2, (float) consec_idle_gcs / RtsFlags.GcFlags.returnDecayFactor)
          : RtsFlags.GcFlags.oldGenFactor;

      debugTrace(DEBUG_gc, "factors: %f %d %f", RtsFlags.GcFlags.oldGenFactor, consec_idle_gcs, scaled_factor  );

      // Unavoidable need for copying memory depends on GC strategy
      // * Copying need 2 * live
      // * Compacting need 1.x * live (we choose 1.2)
      double unavoidable_copied_need_factor = (oldest_gen->compact)
                                              ? 1.2 : 2;

      // Unmoving blocks (compacted, pinned, nonmoving GC blocks) are not going
      // to be copied so don't need to save 2* the memory for them.
      double unavoidable_uncopied_need_factor = 1.2;

      W_ scaled_needed = ((scaled_factor + unavoidable_copied_need_factor) * need_copied_live)
                       + ((scaled_factor + unavoidable_uncopied_need_factor) * need_uncopied_live);
      debugTrace(DEBUG_gc, "factors_2: %f %f", ((scaled_factor + unavoidable_copied_need_factor) * need_copied_live), ((scaled_factor + unavoidable_uncopied_need_factor) * need_uncopied_live));
      need = need_prealloc + scaled_needed;

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
      debugTrace(DEBUG_gc,"Returning: %d %d", got, need);

      uint32_t returned = 0;
      if (got > need) {
          returned = returnMemoryToOS(got - need);
      }
      traceEventMemReturn(cap, got, need, returned);

      // Ensure that we've returned enough mblocks to place us under maxHeapSize.
      // This may fail for instance due to block fragmentation.
      W_ after = got - returned;
      if (RtsFlags.GcFlags.maxHeapSize != 0 && after > BLOCKS_TO_MBLOCKS(RtsFlags.GcFlags.maxHeapSize)) {
        heapOverflow();
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
  for (n = 0; n < getNumCapabilities(); n++) {
      getCapability(n)->r.rCCCS = save_CCS[n];
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
             any_work, scav_find_work, max_n_todo_overflow);

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

    t->cap = getCapability(n);

#if defined(THREADED_RTS)
    t->id = 0;
    SEQ_CST_STORE(&t->wakeup, GC_THREAD_INACTIVE);  // starts true, so we can wait for the
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
            bd->u.scan = bd->free = bdescr_start(bd);

            ws->todo_bd = bd;
            ws->todo_free = bdescr_free(bd);
            ws->todo_lim = bdescr_start(bd) + BLOCK_SIZE_W;
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
        initMutex(&gc_entry_mutex);
        initCondition(&gc_entry_arrived_cv);
        initCondition(&gc_entry_start_now_cv);
        initMutex(&gc_exit_mutex);
        initCondition(&gc_exit_arrived_cv);
        initCondition(&gc_exit_leave_now_cv);
        initMutex(&gc_running_mutex);
        initCondition(&gc_running_cv);
    }

    for (i = from; i < to; i++) {
        gc_threads[i] =
            stgMallocAlignedBytes(sizeof(gc_thread) +
                           RtsFlags.GcFlags.generations * sizeof(gen_workspace),
                           alignof(gc_thread),
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
        for (i = 0; i < getNumCapabilities(); i++) {
            for (g = 0; g < RtsFlags.GcFlags.generations; g++)
            {
                freeWSDeque(gc_threads[i]->gens[g].todo_q);
            }
            stgFreeAligned (gc_threads[i]);
        }
        closeCondition(&gc_running_cv);
        closeMutex(&gc_running_mutex);
        closeCondition(&gc_exit_leave_now_cv);
        closeCondition(&gc_exit_arrived_cv);
        closeMutex(&gc_exit_mutex);
        closeCondition(&gc_entry_start_now_cv);
        closeCondition(&gc_entry_arrived_cv);
        closeMutex(&gc_entry_mutex);
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

static StgWord
inc_running (void)
{
    // We don't hold gc_running_mutex.
    // See Note [Synchronising work stealing]
    StgWord new;
    new = atomic_inc(&gc_running_threads, 1);
    return new;
}

static StgWord
dec_running (void)
{
    // ref Note [Synchronising work stealing]
    ASSERT(RELAXED_LOAD(&gc_running_threads) != 0);
#if defined(THREADED_RTS)
    ACQUIRE_LOCK(&gc_running_mutex);
#endif

    StgWord r = atomic_dec(&gc_running_threads, 1);

#if defined(THREADED_RTS)
    if (r == 0) {
        broadcastCondition(&gc_running_cv);
    }
    RELEASE_LOCK(&gc_running_mutex);
#endif

    return r;
}

# if defined(THREADED_RTS)
void notifyTodoBlock(void) {
    // See Note [Synchronising work stealing]
    // We check work_stealing here because we can actually be called when
    // gc_running_threads == 0, which triggers the asserts. This happens inside
    // traverseWeakPtrList.
    // So the gc leader conveniently sets work_stealing to false before it
    // begins the final sequential collections.
    if(work_stealing) {
        // This is racy. However the consequences are slight.
        // If we see too many threads running we won't signal the condition
        // variable. That's ok, we'll signal when the next block is pushed.
        // There are loads of blocks.
        // If we see too few threads running we will signal the condition
        // variable, but there will be no waiters. This is very cheap.
        StgInt running_threads = SEQ_CST_LOAD(&gc_running_threads);
        StgInt max_running_threads = (StgInt)n_gc_threads - (StgInt)n_gc_idle_threads;
        // These won't hold if !work_stealing, because it may be that:
        // n_gc_threads < n_gc_idle_threads
        ASSERT(running_threads > 0);
        ASSERT(max_running_threads > 0);
        ASSERT(running_threads <= max_running_threads);
        if(running_threads < max_running_threads) {
            signalCondition(&gc_running_cv);
        }
    }
}
#endif


static void
scavenge_until_all_done (void)
{
    uint32_t r USED_IF_DEBUG USED_IF_THREADS;

    for(;;) {
#if defined(THREADED_RTS)
        if (is_par_gc()) {
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
        r = dec_running();

        traceEventGcIdle(gct->cap);

        debugTrace(DEBUG_gc, "%d GC threads still running", r);

        // If there's no hope of stealing more work, then there's nowhere else
        // work can come from and we are finished
#if defined(THREADED_RTS)
        if(is_par_gc() && work_stealing && r != 0) {
            NONATOMIC_ADD(&gct->any_work, 1);
            ACQUIRE_LOCK(&gc_running_mutex);
            // this is SEQ_CST because I haven't considered if it could be
            // weaker
            r = SEQ_CST_LOAD(&gc_running_threads);
            if (r != 0) {
                waitCondition(&gc_running_cv, &gc_running_mutex);
                // this is SEQ_CST because I haven't considered if it could be
                // weaker
                r = SEQ_CST_LOAD(&gc_running_threads);
            }
            // here, if r is 0 then all threads are finished
            // if r > 0 then either:
            //  - waitCondition was subject to spurious wakeup
            //  - a worker thread just pushed a block to it's todo_q
            // so we loop back, looking for more work.
            RELEASE_LOCK(&gc_running_mutex);
            if (r != 0) {
                inc_running();
                traceEventGcWork(gct->cap);
                continue; // for(;;) loop
            }
        }
#endif
        break; // for(;;) loop
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

    SEQ_CST_STORE(&gct->wakeup, GC_THREAD_STANDING_BY);
    debugTrace(DEBUG_gc, "GC thread %d standing by...", gct->thread_index);

    ACQUIRE_LOCK(&gc_entry_mutex);
    SEQ_CST_ADD(&n_gc_entered, 1);
    signalCondition(&gc_entry_arrived_cv);
    while(SEQ_CST_LOAD(&n_gc_entered) != 0) {
        waitCondition(&gc_entry_start_now_cv, &gc_entry_mutex);
    }
    RELEASE_LOCK(&gc_entry_mutex);

    init_gc_thread(gct);

    traceEventGcWork(gct->cap);

    // Every thread evacuates some roots.
    gct->evac_gen_no = 0;
    markCapability(mark_root, gct, cap, true/*prune sparks*/);
    scavenge_capability_mut_lists(cap);

    scavenge_until_all_done();

#if defined(THREADED_RTS)
    // See Note [Pruning the spark pool]
    if(work_stealing && is_par_gc()) {
        pruneSparkQueue(false, cap);
    }
#endif

    // Wait until we're told to continue
    debugTrace(DEBUG_gc, "GC thread %d waiting to continue...",
               gct->thread_index);
    stat_endGCWorker (cap, gct);

    // This must come *after* stat_endGCWorker since it serves to
    // synchronize us with the GC leader, which will later aggregate the
    // GC statistics  (#17964,#18717)
    ACQUIRE_LOCK(&gc_exit_mutex);
    SEQ_CST_STORE(&gct->wakeup, GC_THREAD_WAITING_TO_CONTINUE);
    SEQ_CST_ADD(&n_gc_exited, 1);
    signalCondition(&gc_exit_arrived_cv);
    while(SEQ_CST_LOAD(&n_gc_exited) != 0) {
        waitCondition(&gc_exit_leave_now_cv, &gc_exit_mutex);
    }
    RELEASE_LOCK(&gc_exit_mutex);

    debugTrace(DEBUG_gc, "GC thread %d on my way...", gct->thread_index);

    SET_GCT(saved_gct);
}

#endif

#if defined(THREADED_RTS)

void
waitForGcThreads (Capability *cap, bool idle_cap[])
{
    // n_gc_threads is not valid here, we're too early
    uint32_t n_threads = getNumCapabilities();
    const uint32_t me = cap->no;
    uint32_t i, cur_n_gc_entered;
    Time t0, t1, t2;

    t0 = t1 = t2 = getProcessElapsedTime();

    for(i = 0; i < getNumCapabilities(); ++i) {
        if (i == me || idle_cap[i]) {
            --n_threads;
        }
    }

    ASSERT(n_threads < getNumCapabilities()); // must be less because we don't count ourself
    if(n_threads == 0) { return; }

    ACQUIRE_LOCK(&gc_entry_mutex);
    while((cur_n_gc_entered = SEQ_CST_LOAD(&n_gc_entered)) != n_threads) {
        ASSERT(cur_n_gc_entered < n_threads);
        for(i = 0; i < getNumCapabilities(); ++i) {
            if (i == me || idle_cap[i]) { continue; }
            if (SEQ_CST_LOAD(&gc_threads[i]->wakeup) != GC_THREAD_STANDING_BY) {
                prodCapability(getCapability(i), cap->running_task);
                interruptCapability(getCapability(i));
            }
        }
        // this 1ms timeout is not well justified. It's the shortest timeout we
        // can use on windows. It seems to work well for me.
        timedWaitCondition(&gc_entry_arrived_cv, &gc_entry_mutex, USToTime(1000));
        t2 = getProcessElapsedTime();
        if (RtsFlags.GcFlags.longGCSync != 0 &&
            t2 - t1 > RtsFlags.GcFlags.longGCSync) {
            // best not to hold the mutex while we call a hook function
            RELEASE_LOCK(&gc_entry_mutex);

            /* call this every longGCSync of delay */
            rtsConfig.longGCSync(cap->no, t2 - t0);
            t1 = t2;

            ACQUIRE_LOCK(&gc_entry_mutex);
        }
    }
    RELEASE_LOCK(&gc_entry_mutex);

    if (RtsFlags.GcFlags.longGCSync != 0 &&
        t2 - t0 > RtsFlags.GcFlags.longGCSync) {
        rtsConfig.longGCSyncEnd(t2 - t0);
    }
}

#endif // THREADED_RTS

static void
wakeup_gc_threads (uint32_t me USED_IF_THREADS,
                   bool idle_cap[] USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    uint32_t i;

    if (!is_par_gc()) return;

#if defined(DEBUG)
    StgWord num_idle = 0;
    for(i=0; i < n_gc_threads; ++i) {
        ASSERT(!(i==me && idle_cap[i]));
        if (idle_cap[i]) { ++num_idle;}
    }
    ASSERT(num_idle == n_gc_idle_threads);
#endif

    ACQUIRE_LOCK(&gc_entry_mutex);
    for (i=0; i < n_gc_threads; i++) {
        if (i == me || idle_cap[i]) continue;
        inc_running();
        debugTrace(DEBUG_gc, "waking up gc thread %d", i);
        ASSERT(SEQ_CST_LOAD(&gc_threads[i]->wakeup) == GC_THREAD_STANDING_BY);
        SEQ_CST_STORE(&gc_threads[i]->wakeup, GC_THREAD_RUNNING);
    }
    ASSERT(SEQ_CST_LOAD(&n_gc_entered) ==
           (StgInt)n_gc_threads - 1 - (StgInt)n_gc_idle_threads);
    SEQ_CST_STORE(&n_gc_entered, 0);
    broadcastCondition(&gc_entry_start_now_cv);
    RELEASE_LOCK(&gc_entry_mutex);
#endif
}

// After GC is complete, we must wait for all GC threads to enter the
// standby state, otherwise they may still be executing inside
// scavenge_until_all_done(), and may even remain awake until the next GC starts.
static void
shutdown_gc_threads (uint32_t me USED_IF_THREADS USED_IF_DEBUG,
                     bool idle_cap[] USED_IF_THREADS USED_IF_DEBUG)
{
#if defined(THREADED_RTS)

    if (!is_par_gc()) return;

    // we need to wait for `n_threads` threads. -1 because that's ourself
    StgInt n_threads = (StgInt)n_gc_threads - 1 - (StgInt)n_gc_idle_threads;
    StgInt cur_n_gc_exited;
    ACQUIRE_LOCK(&gc_exit_mutex);
    while((cur_n_gc_exited = SEQ_CST_LOAD(&n_gc_exited)) != n_threads) {
        ASSERT(cur_n_gc_exited >= 0);
        ASSERT(cur_n_gc_exited < n_threads);
        waitCondition(&gc_exit_arrived_cv, &gc_exit_mutex);
    }
#if defined(DEBUG)
    uint32_t i;
    for (i=0; i < getNumCapabilities(); i++) {
        if (i == me || idle_cap[i]) continue;
        ASSERT(SEQ_CST_LOAD(&gc_threads[i]->wakeup) == GC_THREAD_WAITING_TO_CONTINUE);
    }
#endif // DEBUG
    RELEASE_LOCK(&gc_exit_mutex);
#endif // THREADED_RTS
}

#if defined(THREADED_RTS)
void
releaseGCThreads (Capability *cap USED_IF_THREADS, bool idle_cap[])
{
    const uint32_t n_threads = getNumCapabilities();
    const uint32_t me = cap->no;
    uint32_t i;
    uint32_t num_idle = 0;
#if defined(ASSERTS_ENABLED)
    for(i=0; i < n_threads; ++i) {
        ASSERT(!(i==me && idle_cap[i]));
        if (idle_cap[i]) { ++num_idle;}
    }
#endif

    for (i=0; i < n_threads; i++) {
        if (i == me || idle_cap[i]) continue;
        ASSERT(SEQ_CST_LOAD(&gc_threads[i]->wakeup) == GC_THREAD_WAITING_TO_CONTINUE);
        SEQ_CST_STORE(&gc_threads[i]->wakeup, GC_THREAD_INACTIVE);
    }

    ACQUIRE_LOCK(&gc_exit_mutex);
    ASSERT(SEQ_CST_LOAD(&n_gc_exited) == (StgInt)n_threads - 1 - (StgInt)num_idle);
    SEQ_CST_STORE(&n_gc_exited, 0);
    broadcastCondition(&gc_exit_leave_now_cv);
    RELEASE_LOCK(&gc_exit_mutex);
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
    RELEASE_STORE(&cap->mut_lists[gen_no], allocBlockOnNode_sync(cap->node));
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
        for (i = 0; i < getNumCapabilities(); i++) {
            stash_mut_list(getCapability(i), g);
        }
    } else if (g != 0) {
        // Otherwise throw away the current mutable list. Invariant: the
        // mutable list always has at least one block; this means we can avoid
        // a check for NULL in recordMutable().
        for (i = 0; i < getNumCapabilities(); i++) {
            bdescr *old = RELAXED_LOAD(&getCapability(i)->mut_lists[g]);
            freeChain(old);

            bdescr *new = allocBlockOnNode(capNoToNumaNode(i));
            RELAXED_STORE(&getCapability(i)->mut_lists[g], new);
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
    for (n = 0; n < getNumCapabilities(); n++) {
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

        if (ws->todo_free != bdescr_start(ws->todo_bd)) {
            bdescr_set_free(ws->todo_bd, ws->todo_free);
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
            bitmap = bdescr_start(bitmap_bdescr);

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
    for (i = 0; i < getNumCapabilities(); i++) {
        stash_mut_list(getCapability(i), gen->no);
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
    const bool use_nonmoving = RtsFlags.GcFlags.useNonmoving;
    generation *const gen = (use_nonmoving && major_gc) ? oldest_gen : g0;

    for (uint32_t n = 0; n < getNumCapabilities(); n++) {
        bdescr *last = NULL;
        if (use_nonmoving && gen == oldest_gen) {
            // Mark objects as belonging to the nonmoving heap
            for (bdescr *bd = RELAXED_LOAD(&getCapability(n)->pinned_object_blocks); bd != NULL; bd = bd->link) {
                bd->flags |= BF_NONMOVING;
                bd->gen_no = oldest_gen->no;
                oldest_gen->n_large_words += bdescr_free(bd) - bdescr_start(bd);
                oldest_gen->n_large_blocks += bd->blocks;
                last = bd;
            }
        } else {
            for (bdescr *bd = getCapability(n)->pinned_object_blocks; bd != NULL; bd = bd->link) {
                last = bd;
            }
        }

        if (last != NULL) {
            last->link = gen->large_objects;
            if (gen->large_objects != NULL) {
                gen->large_objects->u.back = last;
            }
            gen->large_objects = RELAXED_LOAD(&getCapability(n)->pinned_object_blocks);
            RELAXED_STORE(&getCapability(n)->pinned_object_blocks, NULL);
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
    t->scav_find_work = 0;
    t->max_n_todo_overflow = 0;
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
                        * (W_)getNumCapabilities());

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

        if (oldest_gen->compact || RtsFlags.GcFlags.useNonmoving) {
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
      RtsFlags.GcFlags.minAllocAreaSize * (StgWord)getNumCapabilities();

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


/* Note [Synchronising work stealing]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * During parallel garbage collections, idle gc threads will steal work from
 * other threads. If they see no work to steal then they will wait on a
 * condition variabl(gc_running_cv).
 *
 * There are two synchronisation primitivees:
 *  - gc_running_mutex
 *  - gc_running_cv
 *
 *  Two mutable variables
 *  - gc_running_threads
 *  - work_stealing
 *
 *  Two immutable variables
 *  - n_gc_threads
 *  - n_gc_idle_threads
 *
 *  gc_running_threads is modified only through the functions inc_running and
 *  dec_running are called when a gc thread starts(wakeup_gc_threads), runs out
 *  of work(scavenge_until_all_done), or finds more
 *  work(scavenge_until_all_done).
 *
 *  We care about the value of gc_running_threads in two places.
 *   (a) in dec_running, if gc_running_threads reaches 0 then we broadcast
 *     gc_running_cv so that all gc_threads can exis scavenge_until_all_done.
 *   (b) in notifyTodoBlock, if there are any threads not running, then we
 *     signal gc_running_cv so a thread can try stealing some work.
 *
 *  Note that:
 *    (c) inc_running does not hold gc_running_mutex while incrementing
 *      gc_running_threads.
 *    (d) notifyTodoBlock does not hold gc_running_mutex while inspecting
 *      gc_running_mutex.
 *    (d) The gc leader calls shutdown_gc_threads before it begins the final
 *      sequential collections (i.e. traverseWeakPtrList)
 *    (e) A gc worker thread can never observe gc_running_threads increasing
 *      from 0. gc_running_threads will increase from 0, but this is after (d),
 *      where gc worker threads are all finished.
 *    (f) The check in (b) tolerates wrong values of gc_running_threads. See the
 *      function for details.
 *
 * work_stealing is "mostly immutable". We set it to false when we begin the
 * final sequential collections, for the benefit of notifyTodoBlock.
 * */

/* Note [Scaling retained memory]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Tickets: #19381 #19359 #14702
 *
 * After a spike in memory usage we have been conservative about returning
 * allocated blocks to the OS in case we are still allocating a lot and would
 * end up just reallocating them. The result of this was that up to 4 * live_bytes
 * of blocks would be retained once they were allocated even if memory usage ended up
 * a lot lower.
 *
 * For a heap of size ~1.5G, this would result in OS memory reporting 6G which is
 * both misleading and worrying for users.
 * In long-lived server applications this results in consistent high memory
 * usage when the live data size is much more reasonable (for example ghcide)
 *
 * Therefore we have a new (2021) strategy which starts by retaining up to 4 * live_bytes
 * of blocks before gradually returning unneeded memory back to the OS on subsequent
 * major GCs which are NOT caused by a heap overflow.
 *
 * Each major GC which is NOT caused by heap overflow increases the consec_idle_gcs
 * counter and the amount of memory which is retained is inversely proportional to this number.
 * By default the excess memory retained is
 *  oldGenFactor (controlled by -F) / 2 ^ (consec_idle_gcs * returnDecayFactor)
 *
 * On a major GC caused by a heap overflow, the `consec_idle_gcs` variable is reset to 0
 * (as we could continue to allocate more, so retaining all the memory might make sense).
 *
 * Therefore setting bigger values for `-Fd` makes the rate at which memory is returned slower.
 * Smaller values make it get returned faster. Setting `-Fd0` means no additional memory
 * is retained.
 *
 * The default is `-Fd4` which results in the following scaling:
 *
 * > mapM print [(x, 1/ (2**(x / 4))) | x <- [1 :: Double ..20]]
 * (1.0,0.8408964152537146)
 * ...
 * (4.0,0.5)
 * ...
 * (12.0,0.125)
 * ...
 * (20.0,3.125e-2)
 *
 * So after 12 consecutive GCs only 0.1 of the maximum memory used will be retained.
 *
 * Further to this decay factor, the amount of memory we attempt to retain is
 * also influenced by the GC strategy for the oldest generation. If we are using
 * a copying strategy then we will need at least 2 * live_bytes for copying to take
 * place, so we always keep that much. If using compacting or nonmoving then we need a lower number,
 * so we just retain at least `1.2 * live_bytes` for some protection.
 */

/* Note [-Fd and thrashing]
 * ~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * See the discussion on #21483 and how low we should scale memory usage and whether
 * the current behaviour will result in thrashing.
 *
 >  The post is correct to say that we need 2L memory to do copying collection
 >  when there is L live bytes. But it seems to ignore the fact that we don't
 >  collect until the heap has grown to F*L bytes (where F=2 by default), which
 >  means that in a steady state of L live bytes we actually need (1+F)L bytes
 >  (plus the nursery). This is where the original default of returning memory
 >  above (2+F)L came from: we know we need (1+F)L, plus a bit for the nursery,
 >  plus we add on another L so that we don't thrash.

 >  If I'm understanding the way -Fd works, it will return memory until we're
 >  below the 3L value, which will definitely lead to thrashing because we'll
 >  reallocate the memory to get back to 3L at the next major GC, assuming L
 >  remains constant.

 >  I think you would want to decay from (2+F)L to (1+F)L, but no lower than
 >  that. Also, saying that we're "scaling F" is not really the right way to
 >  think about it, since F is not actually changing, it's the fudge factor we
 >  want to change.

 * The situation where -Fd kicks in is when the process is idle (ie not
 * allocating at all), if a process is idle then the memory usage returns down to
 * a minimal baseline (2L) over quite an extended period by default. Which I
 * think is what you want because if your program memory usage was increasing,
 * this is the state you would get into during an idle period. The expectation is
 * that the amount of program the RTS retains is related to the actual live bytes
 * rather than the historical memory usage of the program.

 * You are then right, that if after this idle period then we start allocating
 * again then we will need to get more memory from the OS but this is on the scale
 * of 10s of minutes rather than milliseconds or seconds (due to the delay
 * factor).
 *
 * It seems in your reply you assume that there will certainly be a major GC due
 * to allocation in the near future -- but this isn't true. I am often leaving a
 * high memory consumption Haskell process idle for periods of days/weeks on my
 * machine and so if that retained 3*L bytes then I would certainly notice!
 *
 *
 * > True enough, I was mainly thinking about programs that allocate continuously.
 * >
 * > So let's be a bit more precise
 * >
 * >     At the next major GC, the program will need (1+F)L
 * >     Until the next major GC, it needs from L up to FL bytes
 * >
 * > (ignoring the nursery and other things, for simplicity)
 * >
 * > if you expect to be in state (2) for a long time, then you could free as
 * > much memory as you like. Indeed you could free everything except the nursery
 * > and L after a major GC. Gradually freeing memory instead is a way to say "if
 * > I've already been idle for a while, then I'm likely to be idle for a while
 * > longer", which might be true (but it's a hypothesis, like the generational
 * > hypothesis).
 * > So if we're hypothesising that the program is idle, why stop freeing at 2L, why not go further?
 *
 * Because to my understanding when you are idle you are going to do an idle
 * major collection which will require 2L so if you free below that you will
 * get thrashing.
 *
 * > But why would you do an idle GC at all in that case? If the program is
 * > mostly idle, and we think it's going to be mostly idle in the future, and
 * > we've already free'd all the memory, then there's no reason to do any more
 * > idle GCs.
 *
 * I think it's rare for programs to be completely idle for long periods. Even
 * during "idle" periods, there is probably still a small amount of allocation
 * happening before each idle period (hence the reason for flags like -Iw which
 * prevent many idle collections happening in close succession during periods
 * of "idleness".
 *

*/

/* Note [Statistics for retaining memory]
*  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*
* At the end of GC, we want to determine the size of the heap in order to
* determine the amount of memory we wish to return to the OS, or if we want
* to increase the heap size to the minimum.
*
* There's two promising candidates for this metric: live words, and live blocks.
*
* Measuring live blocks is promising because blocks are the smallest unit
* that the storage manager can (de)allocate.
* Most of the time live words and live blocks are very similar.
*
* But the two metrics can come apart when the heap is dominated
* by small pinned objects, or when using the non-moving collector.
*
* In both cases, this happens because objects cannot be copied, so
* block occupancy can fall as objects in a block become garbage.
* In situations like this, using live blocks to determine memory
* retention behaviour can lead to us being overly conservative.
*
* Instead we use live words rounded up to the block size to measure
* heap size. This gives us a more accurate picture of the heap.
*
* This works particularly well with the nonmoving collector as we
* can reuse the space taken up by dead heap objects. This choice is less good
* for fragmentation caused by a few pinned objects retaining blocks.
* In that case, the block can only be reused if it is deallocated in its entirety.
* And therefore using live blocks would be more accurate in this case.
* We assume that this is relatively rare and when it does happen,
* this fragmentation is a problem that should be addressed in its own right.
*
* See: #23397
*
*/
