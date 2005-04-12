/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2003
 *
 * Generational garbage collector
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Apply.h"
#include "OSThreads.h"
#include "Storage.h"
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
#include "GCCompact.h"
#include "Signals.h"
#include "STM.h"
#if defined(GRAN) || defined(PAR)
# include "GranSimRts.h"
# include "ParallelRts.h"
# include "FetchMe.h"
# if defined(DEBUG)
#  include "Printer.h"
#  include "ParallelDebug.h"
# endif
#endif
#include "HsFFI.h"
#include "Linker.h"
#if defined(RTS_GTK_FRONTPANEL)
#include "FrontPanel.h"
#endif

#include "RetainerProfile.h"

#include <string.h>

// Turn off inlining when debugging - it obfuscates things
#ifdef DEBUG
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

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
static StgClosure* static_objects;      // live static objects
StgClosure* scavenged_static_objects;   // static objects scavenged so far

/* N is the oldest generation being collected, where the generations
 * are numbered starting at 0.  A major GC (indicated by the major_gc
 * flag) is when we're collecting all generations.  We only attempt to
 * deal with static objects and GC CAFs when doing a major GC.
 */
static nat N;
static rtsBool major_gc;

/* Youngest generation that objects should be evacuated to in
 * evacuate().  (Logically an argument to evacuate, but it's static
 * a lot of the time so we optimise it into a global variable).
 */
static nat evac_gen;

/* Weak pointers
 */
StgWeak *old_weak_ptr_list; // also pending finaliser list

/* Which stage of processing various kinds of weak pointer are we at?
 * (see traverse_weak_ptr_list() below for discussion).
 */
typedef enum { WeakPtrs, WeakThreads, WeakDone } WeakStage;
static WeakStage weak_stage;

/* List of all threads during GC
 */
static StgTSO *old_all_threads;
StgTSO *resurrected_threads;

/* Flag indicating failure to evacuate an object to the desired
 * generation.
 */
static rtsBool failed_to_evac;

/* Old to-space (used for two-space collector only)
 */
static bdescr *old_to_blocks;

/* Data used for allocation area sizing.
 */
static lnat new_blocks;		 // blocks allocated during this GC 
static lnat g0s0_pcnt_kept = 30; // percentage of g0s0 live at last minor GC 

/* Used to avoid long recursion due to selector thunks
 */
static lnat thunk_selector_depth = 0;
#define MAX_THUNK_SELECTOR_DEPTH 8

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static bdescr *     gc_alloc_block          ( step *stp );
static void         mark_root               ( StgClosure **root );

// Use a register argument for evacuate, if available.
#if __GNUC__ >= 2
#define REGPARM1 __attribute__((regparm(1)))
#else
#define REGPARM1
#endif

REGPARM1 static StgClosure * evacuate (StgClosure *q);

static void         zero_static_object_list ( StgClosure* first_static );

static rtsBool      traverse_weak_ptr_list  ( void );
static void         mark_weak_ptr_list      ( StgWeak **list );

static StgClosure * eval_thunk_selector     ( nat field, StgSelector * p );


static void    scavenge                ( step * );
static void    scavenge_mark_stack     ( void );
static void    scavenge_stack          ( StgPtr p, StgPtr stack_end );
static rtsBool scavenge_one            ( StgPtr p );
static void    scavenge_large          ( step * );
static void    scavenge_static         ( void );
static void    scavenge_mutable_list   ( generation *g );

static void    scavenge_large_bitmap   ( StgPtr p, 
					 StgLargeBitmap *large_bitmap, 
					 nat size );

#if 0 && defined(DEBUG)
static void         gcCAFs                  ( void );
#endif

/* -----------------------------------------------------------------------------
   inline functions etc. for dealing with the mark bitmap & stack.
   -------------------------------------------------------------------------- */

#define MARK_STACK_BLOCKS 4

static bdescr *mark_stack_bdescr;
static StgPtr *mark_stack;
static StgPtr *mark_sp;
static StgPtr *mark_splim;

// Flag and pointers used for falling back to a linear scan when the
// mark stack overflows.
static rtsBool mark_stack_overflowed;
static bdescr *oldgen_scan_bd;
static StgPtr  oldgen_scan;

STATIC_INLINE rtsBool
mark_stack_empty(void)
{
    return mark_sp == mark_stack;
}

STATIC_INLINE rtsBool
mark_stack_full(void)
{
    return mark_sp >= mark_splim;
}

STATIC_INLINE void
reset_mark_stack(void)
{
    mark_sp = mark_stack;
}

STATIC_INLINE void
push_mark_stack(StgPtr p)
{
    *mark_sp++ = p;
}

STATIC_INLINE StgPtr
pop_mark_stack(void)
{
    return *--mark_sp;
}

/* -----------------------------------------------------------------------------
   Allocate a new to-space block in the given step.
   -------------------------------------------------------------------------- */

static bdescr *
gc_alloc_block(step *stp)
{
    bdescr *bd = allocBlock();
    bd->gen_no = stp->gen_no;
    bd->step = stp;
    bd->link = NULL;

    // blocks in to-space in generations up to and including N
    // get the BF_EVACUATED flag.
    if (stp->gen_no <= N) {
	bd->flags = BF_EVACUATED;
    } else {
	bd->flags = 0;
    }

    // Start a new to-space block, chain it on after the previous one.
    if (stp->hp_bd == NULL) {
	stp->hp_bd = bd;
    } else {
	stp->hp_bd->free = stp->hp;
	stp->hp_bd->link = bd;
	stp->hp_bd = bd;
    }

    stp->hp    = bd->start;
    stp->hpLim = stp->hp + BLOCK_SIZE_W;

    stp->n_to_blocks++;
    new_blocks++;

    return bd;
}

/* -----------------------------------------------------------------------------
   GarbageCollect

   Rough outline of the algorithm: for garbage collecting generation N
   (and all younger generations):

     - follow all pointers in the root set.  the root set includes all 
       mutable objects in all generations (mutable_list).

     - for each pointer, evacuate the object it points to into either

       + to-space of the step given by step->to, which is the next
         highest step in this generation or the first step in the next
         generation if this is the last step.

       + to-space of generations[evac_gen]->steps[0], if evac_gen != 0.
         When we evacuate an object we attempt to evacuate
         everything it points to into the same generation - this is
         achieved by setting evac_gen to the desired generation.  If
         we can't do this, then an entry in the mut list has to
         be made for the cross-generation pointer.

       + if the object is already in a generation > N, then leave
         it alone.

     - repeatedly scavenge to-space from each step in each generation
       being collected until no more objects can be evacuated.
      
     - free from-space in each step, and set from-space = to-space.

   Locks held: sched_mutex

   -------------------------------------------------------------------------- */

void
GarbageCollect ( void (*get_roots)(evac_fn), rtsBool force_major_gc )
{
  bdescr *bd;
  step *stp;
  lnat live, allocated, collected = 0, copied = 0;
  lnat oldgen_saved_blocks = 0;
  nat g, s;

#ifdef PROFILING
  CostCentreStack *prev_CCS;
#endif

#if defined(DEBUG) && defined(GRAN)
  IF_DEBUG(gc, debugBelch("@@ Starting garbage collection at %ld (%lx)\n", 
		     Now, Now));
#endif

#if defined(RTS_USER_SIGNALS)
  // block signals
  blockUserSignals();
#endif

  // tell the STM to discard any cached closures its hoping to re-use
  stmPreGCHook();

  // tell the stats department that we've started a GC 
  stat_startGC();

  // Init stats and print par specific (timing) info 
  PAR_TICKY_PAR_START();

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

#ifdef RTS_GTK_FRONTPANEL
  if (RtsFlags.GcFlags.frontpanel) {
      updateFrontPanelBeforeGC(N);
  }
#endif

  // check stack sanity *before* GC (ToDo: check all threads) 
#if defined(GRAN)
  // ToDo!: check sanity  IF_DEBUG(sanity, checkTSOsSanity());
#endif
  IF_DEBUG(sanity, checkFreeListSanity());

  /* Initialise the static object lists
   */
  static_objects = END_OF_STATIC_LIST;
  scavenged_static_objects = END_OF_STATIC_LIST;

  /* Save the old to-space if we're doing a two-space collection
   */
  if (RtsFlags.GcFlags.generations == 1) {
    old_to_blocks = g0s0->to_blocks;
    g0s0->to_blocks = NULL;
    g0s0->n_to_blocks = 0;
  }

  /* Keep a count of how many new blocks we allocated during this GC
   * (used for resizing the allocation area, later).
   */
  new_blocks = 0;

  // Initialise to-space in all the generations/steps that we're
  // collecting.
  //
  for (g = 0; g <= N; g++) {

    // throw away the mutable list.  Invariant: the mutable list
    // always has at least one block; this means we can avoid a check for
    // NULL in recordMutable().
    if (g != 0) {
	freeChain(generations[g].mut_list);
	generations[g].mut_list = allocBlock();
    }

    for (s = 0; s < generations[g].n_steps; s++) {

      // generation 0, step 0 doesn't need to-space 
      if (g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1) { 
	continue; 
      }

      stp = &generations[g].steps[s];
      ASSERT(stp->gen_no == g);

      // start a new to-space for this step.
      stp->hp        = NULL;
      stp->hp_bd     = NULL;
      stp->to_blocks = NULL;

      // allocate the first to-space block; extra blocks will be
      // chained on as necessary.
      bd = gc_alloc_block(stp);
      stp->to_blocks   = bd;
      stp->scan        = bd->start;
      stp->scan_bd     = bd;

      // initialise the large object queues.
      stp->new_large_objects = NULL;
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

	  bitmap_size = stp->n_blocks * BLOCK_SIZE / (sizeof(W_)*BITS_PER_BYTE);

	  if (bitmap_size > 0) {
	      bitmap_bdescr = allocGroup((lnat)BLOCK_ROUND_UP(bitmap_size) 
					 / BLOCK_SIZE);
	      stp->bitmap = bitmap_bdescr;
	      bitmap = bitmap_bdescr->start;
	      
	      IF_DEBUG(gc, debugBelch("bitmap_size: %d, bitmap: %p",
				   bitmap_size, bitmap););
	      
	      // don't forget to fill it with zeros!
	      memset(bitmap, 0, bitmap_size);
	      
	      // For each block in this step, point to its bitmap from the
	      // block descriptor.
	      for (bd=stp->blocks; bd != NULL; bd = bd->link) {
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
  }

  /* make sure the older generations have at least one block to
   * allocate into (this makes things easier for copy(), see below).
   */
  for (g = N+1; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      stp = &generations[g].steps[s];
      if (stp->hp_bd == NULL) {
	  ASSERT(stp->blocks == NULL);
	  bd = gc_alloc_block(stp);
	  stp->blocks = bd;
	  stp->n_blocks = 1;
      }
      /* Set the scan pointer for older generations: remember we
       * still have to scavenge objects that have been promoted. */
      stp->scan = stp->hp;
      stp->scan_bd = stp->hp_bd;
      stp->to_blocks = NULL;
      stp->n_to_blocks = 0;
      stp->new_large_objects = NULL;
      stp->scavenged_large_objects = NULL;
      stp->n_scavenged_large_blocks = 0;
    }
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

  /* -----------------------------------------------------------------------
   * follow all the roots that we know about:
   *   - mutable lists from each generation > N
   * we want to *scavenge* these roots, not evacuate them: they're not
   * going to move in this GC.
   * Also: do them in reverse generation order.  This is because we
   * often want to promote objects that are pointed to by older
   * generations early, so we don't have to repeatedly copy them.
   * Doing the generations in reverse order ensures that we don't end
   * up in the situation where we want to evac an object to gen 3 and
   * it has already been evaced to gen 2.
   */
  { 
    int st;
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      generations[g].saved_mut_list = generations[g].mut_list;
      generations[g].mut_list = allocBlock(); 
        // mut_list always has at least one block.
    }

    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      IF_PAR_DEBUG(verbose, printMutableList(&generations[g]));
      scavenge_mutable_list(&generations[g]);
      evac_gen = g;
      for (st = generations[g].n_steps-1; st >= 0; st--) {
	scavenge(&generations[g].steps[st]);
      }
    }
  }

  /* follow roots from the CAF list (used by GHCi)
   */
  evac_gen = 0;
  markCAFs(mark_root);

  /* follow all the roots that the application knows about.
   */
  evac_gen = 0;
  get_roots(mark_root);

#if defined(PAR)
  /* And don't forget to mark the TSO if we got here direct from
   * Haskell! */
  /* Not needed in a seq version?
  if (CurrentTSO) {
    CurrentTSO = (StgTSO *)MarkRoot((StgClosure *)CurrentTSO);
  }
  */

  // Mark the entries in the GALA table of the parallel system 
  markLocalGAs(major_gc);
  // Mark all entries on the list of pending fetches 
  markPendingFetches(major_gc);
#endif

  /* Mark the weak pointer list, and prepare to detect dead weak
   * pointers.
   */
  mark_weak_ptr_list(&weak_ptr_list);
  old_weak_ptr_list = weak_ptr_list;
  weak_ptr_list = NULL;
  weak_stage = WeakPtrs;

  /* The all_threads list is like the weak_ptr_list.  
   * See traverse_weak_ptr_list() for the details.
   */
  old_all_threads = all_threads;
  all_threads = END_TSO_QUEUE;
  resurrected_threads = END_TSO_QUEUE;

  /* Mark the stable pointer table.
   */
  markStablePtrTable(mark_root);

  /* -------------------------------------------------------------------------
   * Repeatedly scavenge all the areas we know about until there's no
   * more scavenging to be done.
   */
  { 
    rtsBool flag;
  loop:
    flag = rtsFalse;

    // scavenge static objects 
    if (major_gc && static_objects != END_OF_STATIC_LIST) {
	IF_DEBUG(sanity, checkStaticObjects(static_objects));
	scavenge_static();
    }

    /* When scavenging the older generations:  Objects may have been
     * evacuated from generations <= N into older generations, and we
     * need to scavenge these objects.  We're going to try to ensure that
     * any evacuations that occur move the objects into at least the
     * same generation as the object being scavenged, otherwise we
     * have to create new entries on the mutable list for the older
     * generation.
     */

    // scavenge each step in generations 0..maxgen 
    { 
      long gen;
      int st; 

    loop2:
      // scavenge objects in compacted generation
      if (mark_stack_overflowed || oldgen_scan_bd != NULL ||
	  (mark_stack_bdescr != NULL && !mark_stack_empty())) {
	  scavenge_mark_stack();
	  flag = rtsTrue;
      }

      for (gen = RtsFlags.GcFlags.generations; --gen >= 0; ) {
	for (st = generations[gen].n_steps; --st >= 0; ) {
	  if (gen == 0 && st == 0 && RtsFlags.GcFlags.generations > 1) { 
	    continue; 
	  }
	  stp = &generations[gen].steps[st];
	  evac_gen = gen;
	  if (stp->hp_bd != stp->scan_bd || stp->scan < stp->hp) {
	    scavenge(stp);
	    flag = rtsTrue;
	    goto loop2;
	  }
	  if (stp->new_large_objects != NULL) {
	    scavenge_large(stp);
	    flag = rtsTrue;
	    goto loop2;
	  }
	}
      }
    }

    if (flag) { goto loop; }

    // must be last...  invariant is that everything is fully
    // scavenged at this point.
    if (traverse_weak_ptr_list()) { // returns rtsTrue if evaced something 
      goto loop;
    }
  }

  /* Update the pointers from the "main thread" list - these are
   * treated as weak pointers because we want to allow a main thread
   * to get a BlockedOnDeadMVar exception in the same way as any other
   * thread.  Note that the threads should all have been retained by
   * GC by virtue of being on the all_threads list, we're just
   * updating pointers here.
   */
  {
      StgMainThread *m;
      StgTSO *tso;
      for (m = main_threads; m != NULL; m = m->link) {
	  tso = (StgTSO *) isAlive((StgClosure *)m->tso);
	  if (tso == NULL) {
	      barf("main thread has been GC'd");
	  }
	  m->tso = tso;
      }
  }

#if defined(PAR)
  // Reconstruct the Global Address tables used in GUM 
  rebuildGAtables(major_gc);
  IF_DEBUG(sanity, checkLAGAtable(rtsTrue/*check closures, too*/));
#endif

  // Now see which stable names are still alive.
  gcStablePtrTable();

  // Tidy the end of the to-space chains 
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (s = 0; s < generations[g].n_steps; s++) {
	  stp = &generations[g].steps[s];
	  if (!(g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1)) {
	      ASSERT(Bdescr(stp->hp) == stp->hp_bd);
	      stp->hp_bd->free = stp->hp;
	  }
      }
  }

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
      oldgen_saved_blocks = oldest_gen->steps[0].n_blocks;
      compact(get_roots);
  }

  IF_DEBUG(sanity, checkGlobalTSOList(rtsFalse));

  /* run through all the generations/steps and tidy up 
   */
  copied = new_blocks * BLOCK_SIZE_W;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {

    if (g <= N) {
      generations[g].collections++; // for stats 
    }

    // Count the mutable list as bytes "copied" for the purposes of
    // stats.  Every mutable list is copied during every GC.
    if (g > 0) {
	for (bd = generations[g].mut_list; bd != NULL; bd = bd->link) {
	    copied += (bd->free - bd->start) * sizeof(StgWord);
	}
    }

    for (s = 0; s < generations[g].n_steps; s++) {
      bdescr *next;
      stp = &generations[g].steps[s];

      if (!(g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1)) {
	// stats information: how much we copied 
	if (g <= N) {
	  copied -= stp->hp_bd->start + BLOCK_SIZE_W -
	    stp->hp_bd->free;
	}
      }

      // for generations we collected... 
      if (g <= N) {

	  // rough calculation of garbage collected, for stats output
	  if (stp->is_compacted) {
	      collected += (oldgen_saved_blocks - stp->n_blocks) * BLOCK_SIZE_W;
	  } else {
	      collected += stp->n_blocks * BLOCK_SIZE_W;
	  }

	/* free old memory and shift to-space into from-space for all
	 * the collected steps (except the allocation area).  These
	 * freed blocks will probaby be quickly recycled.
	 */
	if (!(g == 0 && s == 0)) {
	    if (stp->is_compacted) {
		// for a compacted step, just shift the new to-space
		// onto the front of the now-compacted existing blocks.
		for (bd = stp->to_blocks; bd != NULL; bd = bd->link) {
		    bd->flags &= ~BF_EVACUATED;  // now from-space 
		}
		// tack the new blocks on the end of the existing blocks
		if (stp->blocks == NULL) {
		    stp->blocks = stp->to_blocks;
		} else {
		    for (bd = stp->blocks; bd != NULL; bd = next) {
			next = bd->link;
			if (next == NULL) {
			    bd->link = stp->to_blocks;
			}
			// NB. this step might not be compacted next
			// time, so reset the BF_COMPACTED flags.
			// They are set before GC if we're going to
			// compact.  (search for BF_COMPACTED above).
			bd->flags &= ~BF_COMPACTED;
		    }
		}
		// add the new blocks to the block tally
		stp->n_blocks += stp->n_to_blocks;
	    } else {
		freeChain(stp->blocks);
		stp->blocks = stp->to_blocks;
		stp->n_blocks = stp->n_to_blocks;
		for (bd = stp->blocks; bd != NULL; bd = bd->link) {
		    bd->flags &= ~BF_EVACUATED;	 // now from-space 
		}
	    }
	    stp->to_blocks = NULL;
	    stp->n_to_blocks = 0;
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

      } else {
	// for older generations... 
	
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
	stp->n_blocks += stp->n_to_blocks;
	stp->n_to_blocks = 0;
	stp->n_large_blocks += stp->n_scavenged_large_blocks;
      }
    }
  }

  /* Reset the sizes of the older generations when we do a major
   * collection.
   *
   * CURRENT STRATEGY: make all generations except zero the same size.
   * We have to stay within the maximum heap size, and leave a certain
   * percentage of the maximum heap size available to allocate into.
   */
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

  // Guess the amount of live data for stats.
  live = calcLive();

  /* Free the small objects allocated via allocate(), since this will
   * all have been copied into G0S1 now.  
   */
  if (small_alloc_list != NULL) {
    freeChain(small_alloc_list);
  }
  small_alloc_list = NULL;
  alloc_blocks = 0;
  alloc_Hp = NULL;
  alloc_HpLim = NULL;
  alloc_blocks_lim = RtsFlags.GcFlags.minAllocAreaSize;

  // Start a new pinned_object_block
  pinned_object_block = NULL;

  /* Free the mark stack.
   */
  if (mark_stack_bdescr != NULL) {
      freeGroup(mark_stack_bdescr);
  }

  /* Free any bitmaps.
   */
  for (g = 0; g <= N; g++) {
      for (s = 0; s < generations[g].n_steps; s++) {
	  stp = &generations[g].steps[s];
	  if (stp->is_compacted && stp->bitmap != NULL) {
	      freeGroup(stp->bitmap);
	  }
      }
  }

  /* Two-space collector:
   * Free the old to-space, and estimate the amount of live data.
   */
  if (RtsFlags.GcFlags.generations == 1) {
    nat blocks;
    
    if (old_to_blocks != NULL) {
      freeChain(old_to_blocks);
    }
    for (bd = g0s0->to_blocks; bd != NULL; bd = bd->link) {
      bd->flags = 0;	// now from-space 
    }

    /* For a two-space collector, we need to resize the nursery. */
    
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
    blocks = g0s0->n_to_blocks;

    if ( RtsFlags.GcFlags.maxHeapSize != 0 &&
	 blocks * RtsFlags.GcFlags.oldGenFactor * 2 > 
	   RtsFlags.GcFlags.maxHeapSize ) {
      long adjusted_blocks;  // signed on purpose 
      int pc_free; 
      
      adjusted_blocks = (RtsFlags.GcFlags.maxHeapSize - 2 * blocks);
      IF_DEBUG(gc, debugBelch("@@ Near maximum heap size of 0x%x blocks, blocks = %d, adjusted to %ld", RtsFlags.GcFlags.maxHeapSize, blocks, adjusted_blocks));
      pc_free = adjusted_blocks * 100 / RtsFlags.GcFlags.maxHeapSize;
      if (pc_free < RtsFlags.GcFlags.pcFreeHeap) /* might even be < 0 */ {
	heapOverflow();
      }
      blocks = adjusted_blocks;
      
    } else {
      blocks *= RtsFlags.GcFlags.oldGenFactor;
      if (blocks < RtsFlags.GcFlags.minAllocAreaSize) {
	blocks = RtsFlags.GcFlags.minAllocAreaSize;
      }
    }
    resizeNurseries(blocks);
    
  } else {
    /* Generational collector:
     * If the user has given us a suggested heap size, adjust our
     * allocation area to make best use of the memory available.
     */

    if (RtsFlags.GcFlags.heapSizeSuggestion) {
      long blocks;
      nat needed = calcNeeded(); 	// approx blocks needed at next GC 

      /* Guess how much will be live in generation 0 step 0 next time.
       * A good approximation is obtained by finding the
       * percentage of g0s0 that was live at the last minor GC.
       */
      if (N == 0) {
	g0s0_pcnt_kept = (new_blocks * 100) / countNurseryBlocks();
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

    } else {
      // we might have added extra large blocks to the nursery, so
      // resize back to minAllocAreaSize again.
      resizeNurseries(RtsFlags.GcFlags.minAllocAreaSize);
    }
  }

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

  RELEASE_LOCK(&sched_mutex);
  
  // start any pending finalizers 
  scheduleFinalizers(old_weak_ptr_list);
  
  // send exceptions to any threads which were about to die 
  resurrectThreads(resurrected_threads);
  
  ACQUIRE_LOCK(&sched_mutex);

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

  // check for memory leaks if sanity checking is on 
  IF_DEBUG(sanity, memInventory());

#ifdef RTS_GTK_FRONTPANEL
  if (RtsFlags.GcFlags.frontpanel) {
      updateFrontPanelAfterGC( N, live );
  }
#endif

  // ok, GC over: tell the stats department what happened. 
  stat_endGC(allocated, collected, live, copied, N);

#if defined(RTS_USER_SIGNALS)
  // unblock signals again
  unblockUserSignals();
#endif

  //PAR_TICKY_TP();
}


/* -----------------------------------------------------------------------------
   Weak Pointers

   traverse_weak_ptr_list is called possibly many times during garbage
   collection.  It returns a flag indicating whether it did any work
   (i.e. called evacuate on any live pointers).

   Invariant: traverse_weak_ptr_list is called when the heap is in an
   idempotent state.  That means that there are no pending
   evacuate/scavenge operations.  This invariant helps the weak
   pointer code decide which weak pointers are dead - if there are no
   new live weak pointers, then all the currently unreachable ones are
   dead.

   For generational GC: we just don't try to finalize weak pointers in
   older generations than the one we're collecting.  This could
   probably be optimised by keeping per-generation lists of weak
   pointers, but for a few weak pointers this scheme will work.

   There are three distinct stages to processing weak pointers:

   - weak_stage == WeakPtrs

     We process all the weak pointers whos keys are alive (evacuate
     their values and finalizers), and repeat until we can find no new
     live keys.  If no live keys are found in this pass, then we
     evacuate the finalizers of all the dead weak pointers in order to
     run them.

   - weak_stage == WeakThreads

     Now, we discover which *threads* are still alive.  Pointers to
     threads from the all_threads and main thread lists are the
     weakest of all: a pointers from the finalizer of a dead weak
     pointer can keep a thread alive.  Any threads found to be unreachable
     are evacuated and placed on the resurrected_threads list so we 
     can send them a signal later.

   - weak_stage == WeakDone

     No more evacuation is done.

   -------------------------------------------------------------------------- */

static rtsBool 
traverse_weak_ptr_list(void)
{
  StgWeak *w, **last_w, *next_w;
  StgClosure *new;
  rtsBool flag = rtsFalse;

  switch (weak_stage) {

  case WeakDone:
      return rtsFalse;

  case WeakPtrs:
      /* doesn't matter where we evacuate values/finalizers to, since
       * these pointers are treated as roots (iff the keys are alive).
       */
      evac_gen = 0;
      
      last_w = &old_weak_ptr_list;
      for (w = old_weak_ptr_list; w != NULL; w = next_w) {
	  
	  /* There might be a DEAD_WEAK on the list if finalizeWeak# was
	   * called on a live weak pointer object.  Just remove it.
	   */
	  if (w->header.info == &stg_DEAD_WEAK_info) {
	      next_w = ((StgDeadWeak *)w)->link;
	      *last_w = next_w;
	      continue;
	  }
	  
	  switch (get_itbl(w)->type) {

	  case EVACUATED:
	      next_w = (StgWeak *)((StgEvacuated *)w)->evacuee;
	      *last_w = next_w;
	      continue;

	  case WEAK:
	      /* Now, check whether the key is reachable.
	       */
	      new = isAlive(w->key);
	      if (new != NULL) {
		  w->key = new;
		  // evacuate the value and finalizer 
		  w->value = evacuate(w->value);
		  w->finalizer = evacuate(w->finalizer);
		  // remove this weak ptr from the old_weak_ptr list 
		  *last_w = w->link;
		  // and put it on the new weak ptr list 
		  next_w  = w->link;
		  w->link = weak_ptr_list;
		  weak_ptr_list = w;
		  flag = rtsTrue;
		  IF_DEBUG(weak, debugBelch("Weak pointer still alive at %p -> %p", 
				       w, w->key));
		  continue;
	      }
	      else {
		  last_w = &(w->link);
		  next_w = w->link;
		  continue;
	      }

	  default:
	      barf("traverse_weak_ptr_list: not WEAK");
	  }
      }
      
      /* If we didn't make any changes, then we can go round and kill all
       * the dead weak pointers.  The old_weak_ptr list is used as a list
       * of pending finalizers later on.
       */
      if (flag == rtsFalse) {
	  for (w = old_weak_ptr_list; w; w = w->link) {
	      w->finalizer = evacuate(w->finalizer);
	  }

	  // Next, move to the WeakThreads stage after fully
	  // scavenging the finalizers we've just evacuated.
	  weak_stage = WeakThreads;
      }

      return rtsTrue;

  case WeakThreads:
      /* Now deal with the all_threads list, which behaves somewhat like
       * the weak ptr list.  If we discover any threads that are about to
       * become garbage, we wake them up and administer an exception.
       */
      {
	  StgTSO *t, *tmp, *next, **prev;
	  
	  prev = &old_all_threads;
	  for (t = old_all_threads; t != END_TSO_QUEUE; t = next) {
	      
	      tmp = (StgTSO *)isAlive((StgClosure *)t);
	      
	      if (tmp != NULL) {
		  t = tmp;
	      }
	      
	      ASSERT(get_itbl(t)->type == TSO);
	      switch (t->what_next) {
	      case ThreadRelocated:
		  next = t->link;
		  *prev = next;
		  continue;
	      case ThreadKilled:
	      case ThreadComplete:
		  // finshed or died.  The thread might still be alive, but we
		  // don't keep it on the all_threads list.  Don't forget to
		  // stub out its global_link field.
		  next = t->global_link;
		  t->global_link = END_TSO_QUEUE;
		  *prev = next;
		  continue;
	      default:
		  ;
	      }
	      
	      if (tmp == NULL) {
		  // not alive (yet): leave this thread on the
		  // old_all_threads list.
		  prev = &(t->global_link);
		  next = t->global_link;
	      } 
	      else {
		  // alive: move this thread onto the all_threads list.
		  next = t->global_link;
		  t->global_link = all_threads;
		  all_threads  = t;
		  *prev = next;
	      }
	  }
      }
      
      /* And resurrect any threads which were about to become garbage.
       */
      {
	  StgTSO *t, *tmp, *next;
	  for (t = old_all_threads; t != END_TSO_QUEUE; t = next) {
	      next = t->global_link;
	      tmp = (StgTSO *)evacuate((StgClosure *)t);
	      tmp->global_link = resurrected_threads;
	      resurrected_threads = tmp;
	  }
      }
      
      weak_stage = WeakDone;  // *now* we're done,
      return rtsTrue;         // but one more round of scavenging, please

  default:
      barf("traverse_weak_ptr_list");
      return rtsTrue;
  }

}

/* -----------------------------------------------------------------------------
   After GC, the live weak pointer list may have forwarding pointers
   on it, because a weak pointer object was evacuated after being
   moved to the live weak pointer list.  We remove those forwarding
   pointers here.

   Also, we don't consider weak pointer objects to be reachable, but
   we must nevertheless consider them to be "live" and retain them.
   Therefore any weak pointer objects which haven't as yet been
   evacuated need to be evacuated now.
   -------------------------------------------------------------------------- */


static void
mark_weak_ptr_list ( StgWeak **list )
{
  StgWeak *w, **last_w;

  last_w = list;
  for (w = *list; w; w = w->link) {
      // w might be WEAK, EVACUATED, or DEAD_WEAK (actually CON_STATIC) here
      ASSERT(w->header.info == &stg_DEAD_WEAK_info 
	     || get_itbl(w)->type == WEAK || get_itbl(w)->type == EVACUATED);
      w = (StgWeak *)evacuate((StgClosure *)w);
      *last_w = w;
      last_w = &(w->link);
  }
}

/* -----------------------------------------------------------------------------
   isAlive determines whether the given closure is still alive (after
   a garbage collection) or not.  It returns the new address of the
   closure if it is alive, or NULL otherwise.

   NOTE: Use it before compaction only!
   -------------------------------------------------------------------------- */


StgClosure *
isAlive(StgClosure *p)
{
  const StgInfoTable *info;
  bdescr *bd;

  while (1) {

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl(p);

    // ignore static closures 
    //
    // ToDo: for static closures, check the static link field.
    // Problem here is that we sometimes don't set the link field, eg.
    // for static closures with an empty SRT or CONSTR_STATIC_NOCAFs.
    //
    if (!HEAP_ALLOCED(p)) {
	return p;
    }

    // ignore closures in generations that we're not collecting. 
    bd = Bdescr((P_)p);
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
    if ((bd->flags & BF_COMPACTED) && is_marked((P_)p,bd)) {
	return p;
    }

    switch (info->type) {

    case IND:
    case IND_STATIC:
    case IND_PERM:
    case IND_OLDGEN:		// rely on compatible layout with StgInd 
    case IND_OLDGEN_PERM:
      // follow indirections 
      p = ((StgInd *)p)->indirectee;
      continue;

    case EVACUATED:
      // alive! 
      return ((StgEvacuated *)p)->evacuee;

    case TSO:
      if (((StgTSO *)p)->what_next == ThreadRelocated) {
	p = (StgClosure *)((StgTSO *)p)->link;
	continue;
      } 
      return NULL;

    default:
      // dead. 
      return NULL;
    }
  }
}

static void
mark_root(StgClosure **root)
{
  *root = evacuate(*root);
}

STATIC_INLINE void 
upd_evacuee(StgClosure *p, StgClosure *dest)
{
    // not true: (ToDo: perhaps it should be)
    // ASSERT(Bdescr((P_)dest)->flags & BF_EVACUATED);
    SET_INFO(p, &stg_EVACUATED_info);
    ((StgEvacuated *)p)->evacuee = dest;
}


STATIC_INLINE StgClosure *
copy(StgClosure *src, nat size, step *stp)
{
  P_ to, from, dest;
#ifdef PROFILING
  // @LDV profiling
  nat size_org = size;
#endif

  TICK_GC_WORDS_COPIED(size);
  /* Find out where we're going, using the handy "to" pointer in 
   * the step of the source object.  If it turns out we need to
   * evacuate to an older generation, adjust it here (see comment
   * by evacuate()).
   */
  if (stp->gen_no < evac_gen) {
#ifdef NO_EAGER_PROMOTION    
    failed_to_evac = rtsTrue;
#else
    stp = &generations[evac_gen].steps[0];
#endif
  }

  /* chain a new block onto the to-space for the destination step if
   * necessary.
   */
  if (stp->hp + size >= stp->hpLim) {
    gc_alloc_block(stp);
  }

  for(to = stp->hp, from = (P_)src; size>0; --size) {
    *to++ = *from++;
  }

  dest = stp->hp;
  stp->hp = to;
  upd_evacuee(src,(StgClosure *)dest);
#ifdef PROFILING
  // We store the size of the just evacuated object in the LDV word so that
  // the profiler can guess the position of the next object later.
  SET_EVACUAEE_FOR_LDV(src, size_org);
#endif
  return (StgClosure *)dest;
}

/* Special version of copy() for when we only want to copy the info
 * pointer of an object, but reserve some padding after it.  This is
 * used to optimise evacuation of BLACKHOLEs.
 */


static StgClosure *
copyPart(StgClosure *src, nat size_to_reserve, nat size_to_copy, step *stp)
{
  P_ dest, to, from;
#ifdef PROFILING
  // @LDV profiling
  nat size_to_copy_org = size_to_copy;
#endif

  TICK_GC_WORDS_COPIED(size_to_copy);
  if (stp->gen_no < evac_gen) {
#ifdef NO_EAGER_PROMOTION    
    failed_to_evac = rtsTrue;
#else
    stp = &generations[evac_gen].steps[0];
#endif
  }

  if (stp->hp + size_to_reserve >= stp->hpLim) {
    gc_alloc_block(stp);
  }

  for(to = stp->hp, from = (P_)src; size_to_copy>0; --size_to_copy) {
    *to++ = *from++;
  }
  
  dest = stp->hp;
  stp->hp += size_to_reserve;
  upd_evacuee(src,(StgClosure *)dest);
#ifdef PROFILING
  // We store the size of the just evacuated object in the LDV word so that
  // the profiler can guess the position of the next object later.
  // size_to_copy_org is wrong because the closure already occupies size_to_reserve
  // words.
  SET_EVACUAEE_FOR_LDV(src, size_to_reserve);
  // fill the slop
  if (size_to_reserve - size_to_copy_org > 0)
    FILL_SLOP(stp->hp - 1, (int)(size_to_reserve - size_to_copy_org)); 
#endif
  return (StgClosure *)dest;
}


/* -----------------------------------------------------------------------------
   Evacuate a large object

   This just consists of removing the object from the (doubly-linked)
   step->large_objects list, and linking it on to the (singly-linked)
   step->new_large_objects list, from where it will be scavenged later.

   Convention: bd->flags has BF_EVACUATED set for a large object
   that has been evacuated, or unset otherwise.
   -------------------------------------------------------------------------- */


STATIC_INLINE void
evacuate_large(StgPtr p)
{
  bdescr *bd = Bdescr(p);
  step *stp;

  // object must be at the beginning of the block (or be a ByteArray)
  ASSERT(get_itbl((StgClosure *)p)->type == ARR_WORDS ||
	 (((W_)p & BLOCK_MASK) == 0));

  // already evacuated? 
  if (bd->flags & BF_EVACUATED) { 
    /* Don't forget to set the failed_to_evac flag if we didn't get
     * the desired destination (see comments in evacuate()).
     */
    if (bd->gen_no < evac_gen) {
      failed_to_evac = rtsTrue;
      TICK_GC_FAILED_PROMOTION();
    }
    return;
  }

  stp = bd->step;
  // remove from large_object list 
  if (bd->u.back) {
    bd->u.back->link = bd->link;
  } else { // first object in the list 
    stp->large_objects = bd->link;
  }
  if (bd->link) {
    bd->link->u.back = bd->u.back;
  }
  
  /* link it on to the evacuated large object list of the destination step
   */
  stp = bd->step->to;
  if (stp->gen_no < evac_gen) {
#ifdef NO_EAGER_PROMOTION    
    failed_to_evac = rtsTrue;
#else
    stp = &generations[evac_gen].steps[0];
#endif
  }

  bd->step = stp;
  bd->gen_no = stp->gen_no;
  bd->link = stp->new_large_objects;
  stp->new_large_objects = bd;
  bd->flags |= BF_EVACUATED;
}

/* -----------------------------------------------------------------------------
   Evacuate

   This is called (eventually) for every live object in the system.

   The caller to evacuate specifies a desired generation in the
   evac_gen global variable.  The following conditions apply to
   evacuating an object which resides in generation M when we're
   collecting up to generation N

   if  M >= evac_gen 
           if  M > N     do nothing
	   else          evac to step->to

   if  M < evac_gen      evac to evac_gen, step 0

   if the object is already evacuated, then we check which generation
   it now resides in.

   if  M >= evac_gen     do nothing
   if  M <  evac_gen     set failed_to_evac flag to indicate that we
                         didn't manage to evacuate this object into evac_gen.


   OPTIMISATION NOTES:

   evacuate() is the single most important function performance-wise
   in the GC.  Various things have been tried to speed it up, but as
   far as I can tell the code generated by gcc 3.2 with -O2 is about
   as good as it's going to get.  We pass the argument to evacuate()
   in a register using the 'regparm' attribute (see the prototype for
   evacuate() near the top of this file).

   Changing evacuate() to take an (StgClosure **) rather than
   returning the new pointer seems attractive, because we can avoid
   writing back the pointer when it hasn't changed (eg. for a static
   object, or an object in a generation > N).  However, I tried it and
   it doesn't help.  One reason is that the (StgClosure **) pointer
   gets spilled to the stack inside evacuate(), resulting in far more
   extra reads/writes than we save.
   -------------------------------------------------------------------------- */

REGPARM1 static StgClosure *
evacuate(StgClosure *q)
{
#if defined(PAR)
  StgClosure *to;
#endif
  bdescr *bd = NULL;
  step *stp;
  const StgInfoTable *info;

loop:
  if (HEAP_ALLOCED(q)) {
    bd = Bdescr((P_)q);

    if (bd->gen_no > N) {
	/* Can't evacuate this object, because it's in a generation
	 * older than the ones we're collecting.  Let's hope that it's
	 * in evac_gen or older, or we will have to arrange to track
	 * this pointer using the mutable list.
	 */
	if (bd->gen_no < evac_gen) {
	    // nope 
	    failed_to_evac = rtsTrue;
	    TICK_GC_FAILED_PROMOTION();
	}
	return q;
    }

    /* evacuate large objects by re-linking them onto a different list.
     */
    if (bd->flags & BF_LARGE) {
	info = get_itbl(q);
	if (info->type == TSO && 
	    ((StgTSO *)q)->what_next == ThreadRelocated) {
	    q = (StgClosure *)((StgTSO *)q)->link;
	    goto loop;
	}
	evacuate_large((P_)q);
	return q;
    }

    /* If the object is in a step that we're compacting, then we
     * need to use an alternative evacuate procedure.
     */
    if (bd->flags & BF_COMPACTED) {
	if (!is_marked((P_)q,bd)) {
	    mark((P_)q,bd);
	    if (mark_stack_full()) {
		mark_stack_overflowed = rtsTrue;
		reset_mark_stack();
	    }
	    push_mark_stack((P_)q);
	}
	return q;
    }

    /* Object is not already evacuated. */
    ASSERT((bd->flags & BF_EVACUATED) == 0);

    stp = bd->step->to;
  }
#ifdef DEBUG
  else stp = NULL; // make sure copy() will crash if HEAP_ALLOCED is wrong 
#endif

  // make sure the info pointer is into text space 
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));
  info = get_itbl(q);
  
  switch (info -> type) {

  case MUT_VAR:
  case MVAR:
      return copy(q,sizeW_fromITBL(info),stp);

  case CONSTR_0_1:
  { 
      StgWord w = (StgWord)q->payload[0];
      if (q->header.info == Czh_con_info &&
	  // unsigned, so always true:  (StgChar)w >= MIN_CHARLIKE &&  
	  (StgChar)w <= MAX_CHARLIKE) {
	  return (StgClosure *)CHARLIKE_CLOSURE((StgChar)w);
      }
      if (q->header.info == Izh_con_info &&
	  (StgInt)w >= MIN_INTLIKE && (StgInt)w <= MAX_INTLIKE) {
	  return (StgClosure *)INTLIKE_CLOSURE((StgInt)w);
      }
      // else, fall through ... 
  }

  case FUN_1_0:
  case FUN_0_1:
  case CONSTR_1_0:
  case THUNK_1_0:
  case THUNK_0_1:
    return copy(q,sizeofW(StgHeader)+1,stp);

  case THUNK_1_1:
  case THUNK_0_2:
  case THUNK_2_0:
#ifdef NO_PROMOTE_THUNKS
    if (bd->gen_no == 0 && 
	bd->step->no != 0 &&
	bd->step->no == generations[bd->gen_no].n_steps-1) {
      stp = bd->step;
    }
#endif
    return copy(q,sizeofW(StgHeader)+2,stp);

  case FUN_1_1:
  case FUN_0_2:
  case FUN_2_0:
  case CONSTR_1_1:
  case CONSTR_0_2:
  case CONSTR_2_0:
    return copy(q,sizeofW(StgHeader)+2,stp);

  case FUN:
  case THUNK:
  case CONSTR:
  case IND_PERM:
  case IND_OLDGEN_PERM:
  case WEAK:
  case FOREIGN:
  case STABLE_NAME:
    return copy(q,sizeW_fromITBL(info),stp);

  case BCO:
      return copy(q,bco_sizeW((StgBCO *)q),stp);

  case CAF_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case SE_BLACKHOLE:
  case BLACKHOLE:
    return copyPart(q,BLACKHOLE_sizeW(),sizeofW(StgHeader),stp);

  case THUNK_SELECTOR:
    {
	StgClosure *p;

	if (thunk_selector_depth > MAX_THUNK_SELECTOR_DEPTH) {
	    return copy(q,THUNK_SELECTOR_sizeW(),stp);
	}

	p = eval_thunk_selector(info->layout.selector_offset,
				(StgSelector *)q);

	if (p == NULL) {
	    return copy(q,THUNK_SELECTOR_sizeW(),stp);
	} else {
	    // q is still BLACKHOLE'd.
	    thunk_selector_depth++;
	    p = evacuate(p);
	    thunk_selector_depth--;
	    upd_evacuee(q,p);
#ifdef PROFILING
	    // We store the size of the just evacuated object in the
	    // LDV word so that the profiler can guess the position of
	    // the next object later.
	    SET_EVACUAEE_FOR_LDV(q, THUNK_SELECTOR_sizeW());
#endif
	    return p;
	}
    }

  case IND:
  case IND_OLDGEN:
    // follow chains of indirections, don't evacuate them 
    q = ((StgInd*)q)->indirectee;
    goto loop;

  case THUNK_STATIC:
    if (info->srt_bitmap != 0 && major_gc && 
	THUNK_STATIC_LINK((StgClosure *)q) == NULL) {
      THUNK_STATIC_LINK((StgClosure *)q) = static_objects;
      static_objects = (StgClosure *)q;
    }
    return q;

  case FUN_STATIC:
    if (info->srt_bitmap != 0 && major_gc && 
	FUN_STATIC_LINK((StgClosure *)q) == NULL) {
      FUN_STATIC_LINK((StgClosure *)q) = static_objects;
      static_objects = (StgClosure *)q;
    }
    return q;

  case IND_STATIC:
    /* If q->saved_info != NULL, then it's a revertible CAF - it'll be
     * on the CAF list, so don't do anything with it here (we'll
     * scavenge it later).
     */
    if (major_gc
	  && ((StgIndStatic *)q)->saved_info == NULL
 	  && IND_STATIC_LINK((StgClosure *)q) == NULL) {
	IND_STATIC_LINK((StgClosure *)q) = static_objects;
	static_objects = (StgClosure *)q;
    }
    return q;

  case CONSTR_STATIC:
    if (major_gc && STATIC_LINK(info,(StgClosure *)q) == NULL) {
      STATIC_LINK(info,(StgClosure *)q) = static_objects;
      static_objects = (StgClosure *)q;
    }
    return q;

  case CONSTR_INTLIKE:
  case CONSTR_CHARLIKE:
  case CONSTR_NOCAF_STATIC:
    /* no need to put these on the static linked list, they don't need
     * to be scavenged.
     */
    return q;

  case RET_BCO:
  case RET_SMALL:
  case RET_VEC_SMALL:
  case RET_BIG:
  case RET_VEC_BIG:
  case RET_DYN:
  case UPDATE_FRAME:
  case STOP_FRAME:
  case CATCH_FRAME:
  case CATCH_STM_FRAME:
  case CATCH_RETRY_FRAME:
  case ATOMICALLY_FRAME:
    // shouldn't see these 
    barf("evacuate: stack frame at %p\n", q);

  case PAP:
  case AP:
      return copy(q,pap_sizeW((StgPAP*)q),stp);

  case AP_STACK:
      return copy(q,ap_stack_sizeW((StgAP_STACK*)q),stp);

  case EVACUATED:
    /* Already evacuated, just return the forwarding address.
     * HOWEVER: if the requested destination generation (evac_gen) is
     * older than the actual generation (because the object was
     * already evacuated to a younger generation) then we have to
     * set the failed_to_evac flag to indicate that we couldn't 
     * manage to promote the object to the desired generation.
     */
    if (evac_gen > 0) {		// optimisation 
      StgClosure *p = ((StgEvacuated*)q)->evacuee;
      if (HEAP_ALLOCED(p) && Bdescr((P_)p)->gen_no < evac_gen) {
	failed_to_evac = rtsTrue;
	TICK_GC_FAILED_PROMOTION();
      }
    }
    return ((StgEvacuated*)q)->evacuee;

  case ARR_WORDS:
      // just copy the block 
      return copy(q,arr_words_sizeW((StgArrWords *)q),stp);

  case MUT_ARR_PTRS:
  case MUT_ARR_PTRS_FROZEN:
  case MUT_ARR_PTRS_FROZEN0:
      // just copy the block 
      return copy(q,mut_arr_ptrs_sizeW((StgMutArrPtrs *)q),stp);

  case TSO:
    {
      StgTSO *tso = (StgTSO *)q;

      /* Deal with redirected TSOs (a TSO that's had its stack enlarged).
       */
      if (tso->what_next == ThreadRelocated) {
	q = (StgClosure *)tso->link;
	goto loop;
      }

      /* To evacuate a small TSO, we need to relocate the update frame
       * list it contains.  
       */
      {
	  StgTSO *new_tso;
	  StgPtr p, q;

	  new_tso = (StgTSO *)copyPart((StgClosure *)tso,
				       tso_sizeW(tso),
				       sizeofW(StgTSO), stp);
	  move_TSO(tso, new_tso);
	  for (p = tso->sp, q = new_tso->sp;
	       p < tso->stack+tso->stack_size;) {
	      *q++ = *p++;
	  }
	  
	  return (StgClosure *)new_tso;
      }
    }

#if defined(PAR)
  case RBH:
    {
      //StgInfoTable *rip = get_closure_info(q, &size, &ptrs, &nonptrs, &vhs, str);
      to = copy(q,BLACKHOLE_sizeW(),stp); 
      //ToDo: derive size etc from reverted IP
      //to = copy(q,size,stp);
      IF_DEBUG(gc,
	       debugBelch("@@ evacuate: RBH %p (%s) to %p (%s)",
		     q, info_type(q), to, info_type(to)));
      return to;
    }

  case BLOCKED_FETCH:
    ASSERT(sizeofW(StgBlockedFetch) >= MIN_NONUPD_SIZE);
    to = copy(q,sizeofW(StgBlockedFetch),stp);
    IF_DEBUG(gc,
	     debugBelch("@@ evacuate: %p (%s) to %p (%s)",
		   q, info_type(q), to, info_type(to)));
    return to;

# ifdef DIST    
  case REMOTE_REF:
# endif
  case FETCH_ME:
    ASSERT(sizeofW(StgBlockedFetch) >= MIN_UPD_SIZE);
    to = copy(q,sizeofW(StgFetchMe),stp);
    IF_DEBUG(gc,
	     debugBelch("@@ evacuate: %p (%s) to %p (%s)",
		   q, info_type(q), to, info_type(to)));
    return to;

  case FETCH_ME_BQ:
    ASSERT(sizeofW(StgBlockedFetch) >= MIN_UPD_SIZE);
    to = copy(q,sizeofW(StgFetchMeBlockingQueue),stp);
    IF_DEBUG(gc,
	     debugBelch("@@ evacuate: %p (%s) to %p (%s)",
		   q, info_type(q), to, info_type(to)));
    return to;
#endif

  case TREC_HEADER: 
    return copy(q,sizeofW(StgTRecHeader),stp);

  case TVAR_WAIT_QUEUE:
    return copy(q,sizeofW(StgTVarWaitQueue),stp);

  case TVAR:
    return copy(q,sizeofW(StgTVar),stp);
    
  case TREC_CHUNK:
    return copy(q,sizeofW(StgTRecChunk),stp);

  default:
    barf("evacuate: strange closure type %d", (int)(info->type));
  }

  barf("evacuate");
}

/* -----------------------------------------------------------------------------
   Evaluate a THUNK_SELECTOR if possible.

   returns: NULL if we couldn't evaluate this THUNK_SELECTOR, or
   a closure pointer if we evaluated it and this is the result.  Note
   that "evaluating" the THUNK_SELECTOR doesn't necessarily mean
   reducing it to HNF, just that we have eliminated the selection.
   The result might be another thunk, or even another THUNK_SELECTOR.

   If the return value is non-NULL, the original selector thunk has
   been BLACKHOLE'd, and should be updated with an indirection or a
   forwarding pointer.  If the return value is NULL, then the selector
   thunk is unchanged.
   -------------------------------------------------------------------------- */

static inline rtsBool
is_to_space ( StgClosure *p )
{
    bdescr *bd;

    bd = Bdescr((StgPtr)p);
    if (HEAP_ALLOCED(p) &&
	((bd->flags & BF_EVACUATED) 
	 || ((bd->flags & BF_COMPACTED) &&
	     is_marked((P_)p,bd)))) {
	return rtsTrue;
    } else {
	return rtsFalse;
    }
}    

static StgClosure *
eval_thunk_selector( nat field, StgSelector * p )
{
    StgInfoTable *info;
    const StgInfoTable *info_ptr;
    StgClosure *selectee;
    
    selectee = p->selectee;

    // Save the real info pointer (NOTE: not the same as get_itbl()).
    info_ptr = p->header.info;

    // If the THUNK_SELECTOR is in a generation that we are not
    // collecting, then bail out early.  We won't be able to save any
    // space in any case, and updating with an indirection is trickier
    // in an old gen.
    if (Bdescr((StgPtr)p)->gen_no > N) {
	return NULL;
    }

    // BLACKHOLE the selector thunk, since it is now under evaluation.
    // This is important to stop us going into an infinite loop if
    // this selector thunk eventually refers to itself.
    SET_INFO(p,&stg_BLACKHOLE_info);

selector_loop:

    // We don't want to end up in to-space, because this causes
    // problems when the GC later tries to evacuate the result of
    // eval_thunk_selector().  There are various ways this could
    // happen:
    //
    // 1. following an IND_STATIC
    //
    // 2. when the old generation is compacted, the mark phase updates
    //    from-space pointers to be to-space pointers, and we can't
    //    reliably tell which we're following (eg. from an IND_STATIC).
    // 
    // 3. compacting GC again: if we're looking at a constructor in
    //    the compacted generation, it might point directly to objects
    //    in to-space.  We must bale out here, otherwise doing the selection
    //    will result in a to-space pointer being returned.
    //
    //  (1) is dealt with using a BF_EVACUATED test on the
    //  selectee. (2) and (3): we can tell if we're looking at an
    //  object in the compacted generation that might point to
    //  to-space objects by testing that (a) it is BF_COMPACTED, (b)
    //  the compacted generation is being collected, and (c) the
    //  object is marked.  Only a marked object may have pointers that
    //  point to to-space objects, because that happens when
    //  scavenging.
    //
    //  The to-space test is now embodied in the in_to_space() inline
    //  function, as it is re-used below.
    //
    if (is_to_space(selectee)) {
	goto bale_out;
    }

    info = get_itbl(selectee);
    switch (info->type) {
      case CONSTR:
      case CONSTR_1_0:
      case CONSTR_0_1:
      case CONSTR_2_0:
      case CONSTR_1_1:
      case CONSTR_0_2:
      case CONSTR_STATIC:
      case CONSTR_NOCAF_STATIC:
	  // check that the size is in range 
	  ASSERT(field <  (StgWord32)(info->layout.payload.ptrs + 
				      info->layout.payload.nptrs));
	  
	  // Select the right field from the constructor, and check
	  // that the result isn't in to-space.  It might be in
	  // to-space if, for example, this constructor contains
	  // pointers to younger-gen objects (and is on the mut-once
	  // list).
	  //
	  { 
	      StgClosure *q;
	      q = selectee->payload[field];
	      if (is_to_space(q)) {
		  goto bale_out;
	      } else {
		  return q;
	      }
	  }

      case IND:
      case IND_PERM:
      case IND_OLDGEN:
      case IND_OLDGEN_PERM:
      case IND_STATIC:
	  selectee = ((StgInd *)selectee)->indirectee;
	  goto selector_loop;

      case EVACUATED:
	  // We don't follow pointers into to-space; the constructor
	  // has already been evacuated, so we won't save any space
	  // leaks by evaluating this selector thunk anyhow.
	  break;

      case THUNK_SELECTOR:
      {
	  StgClosure *val;

	  // check that we don't recurse too much, re-using the
	  // depth bound also used in evacuate().
	  if (thunk_selector_depth >= MAX_THUNK_SELECTOR_DEPTH) {
	      break;
	  }
	  thunk_selector_depth++;

	  val = eval_thunk_selector(info->layout.selector_offset, 
				    (StgSelector *)selectee);

	  thunk_selector_depth--;

	  if (val == NULL) { 
	      break;
	  } else {
	      // We evaluated this selector thunk, so update it with
	      // an indirection.  NOTE: we don't use UPD_IND here,
	      // because we are guaranteed that p is in a generation
	      // that we are collecting, and we never want to put the
	      // indirection on a mutable list.
#ifdef PROFILING
	      // For the purposes of LDV profiling, we have destroyed
	      // the original selector thunk.
	      SET_INFO(p, info_ptr);
	      LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(selectee);
#endif
	      ((StgInd *)selectee)->indirectee = val;
	      SET_INFO(selectee,&stg_IND_info);

	      // For the purposes of LDV profiling, we have created an
	      // indirection.
	      LDV_RECORD_CREATE(selectee);

	      selectee = val;
	      goto selector_loop;
	  }
      }

      case AP:
      case AP_STACK:
      case THUNK:
      case THUNK_1_0:
      case THUNK_0_1:
      case THUNK_2_0:
      case THUNK_1_1:
      case THUNK_0_2:
      case THUNK_STATIC:
      case CAF_BLACKHOLE:
      case SE_CAF_BLACKHOLE:
      case SE_BLACKHOLE:
      case BLACKHOLE:
#if defined(PAR)
      case RBH:
      case BLOCKED_FETCH:
# ifdef DIST    
      case REMOTE_REF:
# endif
      case FETCH_ME:
      case FETCH_ME_BQ:
#endif
	  // not evaluated yet 
	  break;
    
      default:
	barf("eval_thunk_selector: strange selectee %d",
	     (int)(info->type));
    }

bale_out:
    // We didn't manage to evaluate this thunk; restore the old info pointer
    SET_INFO(p, info_ptr);
    return NULL;
}

/* -----------------------------------------------------------------------------
   move_TSO is called to update the TSO structure after it has been
   moved from one place to another.
   -------------------------------------------------------------------------- */

void
move_TSO (StgTSO *src, StgTSO *dest)
{
    ptrdiff_t diff;

    // relocate the stack pointer... 
    diff = (StgPtr)dest - (StgPtr)src; // In *words* 
    dest->sp = (StgPtr)dest->sp + diff;
}

/* Similar to scavenge_large_bitmap(), but we don't write back the
 * pointers we get back from evacuate().
 */
static void
scavenge_large_srt_bitmap( StgLargeSRT *large_srt )
{
    nat i, b, size;
    StgWord bitmap;
    StgClosure **p;
    
    b = 0;
    bitmap = large_srt->l.bitmap[b];
    size   = (nat)large_srt->l.size;
    p      = (StgClosure **)large_srt->srt;
    for (i = 0; i < size; ) {
	if ((bitmap & 1) != 0) {
	    evacuate(*p);
	}
	i++;
	p++;
	if (i % BITS_IN(W_) == 0) {
	    b++;
	    bitmap = large_srt->l.bitmap[b];
	} else {
	    bitmap = bitmap >> 1;
	}
    }
}

/* evacuate the SRT.  If srt_bitmap is zero, then there isn't an
 * srt field in the info table.  That's ok, because we'll
 * never dereference it.
 */
STATIC_INLINE void
scavenge_srt (StgClosure **srt, nat srt_bitmap)
{
  nat bitmap;
  StgClosure **p;

  bitmap = srt_bitmap;
  p = srt;

  if (bitmap == (StgHalfWord)(-1)) {  
      scavenge_large_srt_bitmap( (StgLargeSRT *)srt );
      return;
  }

  while (bitmap != 0) {
      if ((bitmap & 1) != 0) {
#ifdef ENABLE_WIN32_DLL_SUPPORT
	  // Special-case to handle references to closures hiding out in DLLs, since
	  // double indirections required to get at those. The code generator knows
	  // which is which when generating the SRT, so it stores the (indirect)
	  // reference to the DLL closure in the table by first adding one to it.
	  // We check for this here, and undo the addition before evacuating it.
	  // 
	  // If the SRT entry hasn't got bit 0 set, the SRT entry points to a
	  // closure that's fixed at link-time, and no extra magic is required.
	  if ( (unsigned long)(*srt) & 0x1 ) {
	      evacuate(*stgCast(StgClosure**,(stgCast(unsigned long, *srt) & ~0x1)));
	  } else {
	      evacuate(*p);
	  }
#else
	  evacuate(*p);
#endif
      }
      p++;
      bitmap = bitmap >> 1;
  }
}


STATIC_INLINE void
scavenge_thunk_srt(const StgInfoTable *info)
{
    StgThunkInfoTable *thunk_info;

    thunk_info = itbl_to_thunk_itbl(info);
    scavenge_srt((StgClosure **)GET_SRT(thunk_info), thunk_info->i.srt_bitmap);
}

STATIC_INLINE void
scavenge_fun_srt(const StgInfoTable *info)
{
    StgFunInfoTable *fun_info;

    fun_info = itbl_to_fun_itbl(info);
    scavenge_srt((StgClosure **)GET_FUN_SRT(fun_info), fun_info->i.srt_bitmap);
}

STATIC_INLINE void
scavenge_ret_srt(const StgInfoTable *info)
{
    StgRetInfoTable *ret_info;

    ret_info = itbl_to_ret_itbl(info);
    scavenge_srt((StgClosure **)GET_SRT(ret_info), ret_info->i.srt_bitmap);
}

/* -----------------------------------------------------------------------------
   Scavenge a TSO.
   -------------------------------------------------------------------------- */

static void
scavengeTSO (StgTSO *tso)
{
    // chase the link field for any TSOs on the same queue 
    tso->link = (StgTSO *)evacuate((StgClosure *)tso->link);
    if (   tso->why_blocked == BlockedOnMVar
	|| tso->why_blocked == BlockedOnBlackHole
	|| tso->why_blocked == BlockedOnException
#if defined(PAR)
	|| tso->why_blocked == BlockedOnGA
	|| tso->why_blocked == BlockedOnGA_NoSend
#endif
	) {
	tso->block_info.closure = evacuate(tso->block_info.closure);
    }
    if ( tso->blocked_exceptions != NULL ) {
	tso->blocked_exceptions = 
	    (StgTSO *)evacuate((StgClosure *)tso->blocked_exceptions);
    }
    
    // scavange current transaction record
    tso->trec = (StgTRecHeader *)evacuate((StgClosure *)tso->trec);
    
    // scavenge this thread's stack 
    scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
}

/* -----------------------------------------------------------------------------
   Blocks of function args occur on the stack (at the top) and
   in PAPs.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
scavenge_arg_block (StgFunInfoTable *fun_info, StgClosure **args)
{
    StgPtr p;
    StgWord bitmap;
    nat size;

    p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
	size = BITMAP_SIZE(fun_info->f.b.bitmap);
	goto small_bitmap;
    case ARG_GEN_BIG:
	size = GET_FUN_LARGE_BITMAP(fun_info)->size;
	scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
	size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

STATIC_INLINE StgPtr
scavenge_PAP (StgPAP *pap)
{
    StgPtr p;
    StgWord bitmap, size;
    StgFunInfoTable *fun_info;

    pap->fun = evacuate(pap->fun);
    fun_info = get_fun_itbl(pap->fun);
    ASSERT(fun_info->i.type != PAP);

    p = (StgPtr)pap->payload;
    size = pap->n_args;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
	goto small_bitmap;
    case ARG_GEN_BIG:
	scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    case ARG_BCO:
	scavenge_large_bitmap((StgPtr)pap->payload, BCO_BITMAP(pap->fun), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	size = pap->n_args;
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

/* -----------------------------------------------------------------------------
   Scavenge a given step until there are no more objects in this step
   to scavenge.

   evac_gen is set by the caller to be either zero (for a step in a
   generation < N) or G where G is the generation of the step being
   scavenged.  

   We sometimes temporarily change evac_gen back to zero if we're
   scavenging a mutable object where early promotion isn't such a good
   idea.  
   -------------------------------------------------------------------------- */

static void
scavenge(step *stp)
{
  StgPtr p, q;
  StgInfoTable *info;
  bdescr *bd;
  nat saved_evac_gen = evac_gen;

  p = stp->scan;
  bd = stp->scan_bd;

  failed_to_evac = rtsFalse;

  /* scavenge phase - standard breadth-first scavenging of the
   * evacuated objects 
   */

  while (bd != stp->hp_bd || p < stp->hp) {

    // If we're at the end of this block, move on to the next block 
    if (bd != stp->hp_bd && p == bd->free) {
      bd = bd->link;
      p = bd->start;
      continue;
    }

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);
    
    ASSERT(thunk_selector_depth == 0);

    q = p;
    switch (info->type) {

    case MVAR:
    { 
	StgMVar *mvar = ((StgMVar *)p);
	evac_gen = 0;
	mvar->head = (StgTSO *)evacuate((StgClosure *)mvar->head);
	mvar->tail = (StgTSO *)evacuate((StgClosure *)mvar->tail);
	mvar->value = evacuate((StgClosure *)mvar->value);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable.
	p += sizeofW(StgMVar);
	break;
    }

    case FUN_2_0:
	scavenge_fun_srt(info);
	((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;

    case THUNK_2_0:
	scavenge_thunk_srt(info);
    case CONSTR_2_0:
	((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case THUNK_1_0:
	scavenge_thunk_srt(info);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 1;
	break;
	
    case FUN_1_0:
	scavenge_fun_srt(info);
    case CONSTR_1_0:
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 1;
	break;
	
    case THUNK_0_1:
	scavenge_thunk_srt(info);
	p += sizeofW(StgHeader) + 1;
	break;
	
    case FUN_0_1:
	scavenge_fun_srt(info);
    case CONSTR_0_1:
	p += sizeofW(StgHeader) + 1;
	break;
	
    case THUNK_0_2:
	scavenge_thunk_srt(info);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case FUN_0_2:
	scavenge_fun_srt(info);
    case CONSTR_0_2:
	p += sizeofW(StgHeader) + 2;
	break;
	
    case THUNK_1_1:
	scavenge_thunk_srt(info);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;

    case FUN_1_1:
	scavenge_fun_srt(info);
    case CONSTR_1_1:
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case FUN:
	scavenge_fun_srt(info);
	goto gen_obj;

    case THUNK:
	scavenge_thunk_srt(info);
	// fall through 
	
    gen_obj:
    case CONSTR:
    case WEAK:
    case FOREIGN:
    case STABLE_NAME:
    {
	StgPtr end;

	end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	p += info->layout.payload.nptrs;
	break;
    }

    case BCO: {
	StgBCO *bco = (StgBCO *)p;
	bco->instrs = (StgArrWords *)evacuate((StgClosure *)bco->instrs);
	bco->literals = (StgArrWords *)evacuate((StgClosure *)bco->literals);
	bco->ptrs = (StgMutArrPtrs *)evacuate((StgClosure *)bco->ptrs);
	bco->itbls = (StgArrWords *)evacuate((StgClosure *)bco->itbls);
	p += bco_sizeW(bco);
	break;
    }

    case IND_PERM:
      if (stp->gen->no != 0) {
#ifdef PROFILING
        // @LDV profiling
        // No need to call LDV_recordDead_FILL_SLOP_DYNAMIC() because an 
        // IND_OLDGEN_PERM closure is larger than an IND_PERM closure.
        LDV_recordDead((StgClosure *)p, sizeofW(StgInd));
#endif        
        // 
        // Todo: maybe use SET_HDR() and remove LDV_RECORD_CREATE()?
        //
	SET_INFO(((StgClosure *)p), &stg_IND_OLDGEN_PERM_info);

        // We pretend that p has just been created.
        LDV_RECORD_CREATE((StgClosure *)p);
      }
	// fall through 
    case IND_OLDGEN_PERM:
	((StgInd *)p)->indirectee = evacuate(((StgInd *)p)->indirectee);
	p += sizeofW(StgInd);
	break;

    case MUT_VAR:
	evac_gen = 0;
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable anyhow
	p += sizeofW(StgMutVar);
	break;

    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
    case SE_BLACKHOLE:
    case BLACKHOLE:
	p += BLACKHOLE_sizeW();
	break;

    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	s->selectee = evacuate(s->selectee);
	p += THUNK_SELECTOR_sizeW();
	break;
    }

    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
	StgAP_STACK *ap = (StgAP_STACK *)p;

	ap->fun = evacuate(ap->fun);
	scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	p = (StgPtr)ap->payload + ap->size;
	break;
    }

    case PAP:
    case AP:
	p = scavenge_PAP((StgPAP *)p);
	break;

    case ARR_WORDS:
	// nothing to follow 
	p += arr_words_sizeW((StgArrWords *)p);
	break;

    case MUT_ARR_PTRS:
	// follow everything 
    {
	StgPtr next;

	evac_gen = 0;		// repeatedly mutable 
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable anyhow.
	break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
	// follow everything 
    {
	StgPtr next;

	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	// it's tempting to recordMutable() if failed_to_evac is
	// false, but that breaks some assumptions (eg. every
	// closure on the mutable list is supposed to have the MUT
	// flag set, and MUT_ARR_PTRS_FROZEN doesn't).
	break;
    }

    case TSO:
    { 
	StgTSO *tso = (StgTSO *)p;
	evac_gen = 0;
	scavengeTSO(tso);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable anyhow.
	p += tso_sizeW(tso);
	break;
    }

#if defined(PAR)
    case RBH:
    { 
#if 0
	nat size, ptrs, nonptrs, vhs;
	char str[80];
	StgInfoTable *rip = get_closure_info(p, &size, &ptrs, &nonptrs, &vhs, str);
#endif
	StgRBH *rbh = (StgRBH *)p;
	(StgClosure *)rbh->blocking_queue = 
	    evacuate((StgClosure *)rbh->blocking_queue);
	failed_to_evac = rtsTrue;  // mutable anyhow.
	IF_DEBUG(gc,
		 debugBelch("@@ scavenge: RBH %p (%s) (new blocking_queue link=%p)",
		       p, info_type(p), (StgClosure *)rbh->blocking_queue));
	// ToDo: use size of reverted closure here!
	p += BLACKHOLE_sizeW(); 
	break;
    }

    case BLOCKED_FETCH:
    { 
	StgBlockedFetch *bf = (StgBlockedFetch *)p;
	// follow the pointer to the node which is being demanded 
	(StgClosure *)bf->node = 
	    evacuate((StgClosure *)bf->node);
	// follow the link to the rest of the blocking queue 
	(StgClosure *)bf->link = 
	    evacuate((StgClosure *)bf->link);
	IF_DEBUG(gc,
		 debugBelch("@@ scavenge: %p (%s); node is now %p; exciting, isn't it",
		       bf, info_type((StgClosure *)bf), 
		       bf->node, info_type(bf->node)));
	p += sizeofW(StgBlockedFetch);
	break;
    }

#ifdef DIST
    case REMOTE_REF:
#endif
    case FETCH_ME:
	p += sizeofW(StgFetchMe);
	break; // nothing to do in this case

    case FETCH_ME_BQ:
    { 
	StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)p;
	(StgClosure *)fmbq->blocking_queue = 
	    evacuate((StgClosure *)fmbq->blocking_queue);
	IF_DEBUG(gc,
		 debugBelch("@@ scavenge: %p (%s) exciting, isn't it",
		       p, info_type((StgClosure *)p)));
	p += sizeofW(StgFetchMeBlockingQueue);
	break;
    }
#endif

    case TVAR_WAIT_QUEUE:
      {
	StgTVarWaitQueue *wq = ((StgTVarWaitQueue *) p);
	evac_gen = 0;
	wq->waiting_tso = (StgTSO *)evacuate((StgClosure*)wq->waiting_tso);
	wq->next_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)wq->next_queue_entry);
	wq->prev_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)wq->prev_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTVarWaitQueue);
	break;
      }

    case TVAR:
      {
	StgTVar *tvar = ((StgTVar *) p);
	evac_gen = 0;
	tvar->current_value = evacuate((StgClosure*)tvar->current_value);
	tvar->first_wait_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)tvar->first_wait_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTVar);
	break;
      }

    case TREC_HEADER:
      {
        StgTRecHeader *trec = ((StgTRecHeader *) p);
        evac_gen = 0;
	trec->enclosing_trec = (StgTRecHeader *)evacuate((StgClosure*)trec->enclosing_trec);
	trec->current_chunk = (StgTRecChunk *)evacuate((StgClosure*)trec->current_chunk);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTRecHeader);
        break;
      }

    case TREC_CHUNK:
      {
	StgWord i;
	StgTRecChunk *tc = ((StgTRecChunk *) p);
	TRecEntry *e = &(tc -> entries[0]);
	evac_gen = 0;
	tc->prev_chunk = (StgTRecChunk *)evacuate((StgClosure*)tc->prev_chunk);
	for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	  e->tvar = (StgTVar *)evacuate((StgClosure*)e->tvar);
	  e->expected_value = evacuate((StgClosure*)e->expected_value);
	  e->new_value = evacuate((StgClosure*)e->new_value);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	p += sizeofW(StgTRecChunk);
	break;
      }

    default:
	barf("scavenge: unimplemented/strange closure type %d @ %p", 
	     info->type, p);
    }

    /*
     * We need to record the current object on the mutable list if
     *  (a) It is actually mutable, or 
     *  (b) It contains pointers to a younger generation.
     * Case (b) arises if we didn't manage to promote everything that
     * the current object points to into the current generation.
     */
    if (failed_to_evac) {
	failed_to_evac = rtsFalse;
	recordMutableGen((StgClosure *)q, stp->gen);
    }
  }

  stp->scan_bd = bd;
  stp->scan = p;
}    

/* -----------------------------------------------------------------------------
   Scavenge everything on the mark stack.

   This is slightly different from scavenge():
      - we don't walk linearly through the objects, so the scavenger
        doesn't need to advance the pointer on to the next object.
   -------------------------------------------------------------------------- */

static void
scavenge_mark_stack(void)
{
    StgPtr p, q;
    StgInfoTable *info;
    nat saved_evac_gen;

    evac_gen = oldest_gen->no;
    saved_evac_gen = evac_gen;

linear_scan:
    while (!mark_stack_empty()) {
	p = pop_mark_stack();

	ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
	info = get_itbl((StgClosure *)p);
	
	q = p;
	switch (info->type) {
	    
	case MVAR:
	{
	    StgMVar *mvar = ((StgMVar *)p);
	    evac_gen = 0;
	    mvar->head = (StgTSO *)evacuate((StgClosure *)mvar->head);
	    mvar->tail = (StgTSO *)evacuate((StgClosure *)mvar->tail);
	    mvar->value = evacuate((StgClosure *)mvar->value);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable.
	    break;
	}

	case FUN_2_0:
	    scavenge_fun_srt(info);
	    ((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;

	case THUNK_2_0:
	    scavenge_thunk_srt(info);
	case CONSTR_2_0:
	    ((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;
	
	case FUN_1_0:
	case FUN_1_1:
	    scavenge_fun_srt(info);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;

	case THUNK_1_0:
	case THUNK_1_1:
	    scavenge_thunk_srt(info);
	case CONSTR_1_0:
	case CONSTR_1_1:
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;
	
	case FUN_0_1:
	case FUN_0_2:
	    scavenge_fun_srt(info);
	    break;

	case THUNK_0_1:
	case THUNK_0_2:
	    scavenge_thunk_srt(info);
	    break;

	case CONSTR_0_1:
	case CONSTR_0_2:
	    break;
	
	case FUN:
	    scavenge_fun_srt(info);
	    goto gen_obj;

	case THUNK:
	    scavenge_thunk_srt(info);
	    // fall through 
	
	gen_obj:
	case CONSTR:
	case WEAK:
	case FOREIGN:
	case STABLE_NAME:
	{
	    StgPtr end;
	    
	    end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	    for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    break;
	}

	case BCO: {
	    StgBCO *bco = (StgBCO *)p;
	    bco->instrs = (StgArrWords *)evacuate((StgClosure *)bco->instrs);
	    bco->literals = (StgArrWords *)evacuate((StgClosure *)bco->literals);
	    bco->ptrs = (StgMutArrPtrs *)evacuate((StgClosure *)bco->ptrs);
	    bco->itbls = (StgArrWords *)evacuate((StgClosure *)bco->itbls);
	    break;
	}

	case IND_PERM:
	    // don't need to do anything here: the only possible case
	    // is that we're in a 1-space compacting collector, with
	    // no "old" generation.
	    break;

	case IND_OLDGEN:
	case IND_OLDGEN_PERM:
	    ((StgInd *)p)->indirectee = 
		evacuate(((StgInd *)p)->indirectee);
	    break;

	case MUT_VAR:
	    evac_gen = 0;
	    ((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue;
	    break;

	case CAF_BLACKHOLE:
	case SE_CAF_BLACKHOLE:
	case SE_BLACKHOLE:
	case BLACKHOLE:
	case ARR_WORDS:
	    break;

	case THUNK_SELECTOR:
	{ 
	    StgSelector *s = (StgSelector *)p;
	    s->selectee = evacuate(s->selectee);
	    break;
	}

	// A chunk of stack saved in a heap object
	case AP_STACK:
	{
	    StgAP_STACK *ap = (StgAP_STACK *)p;
	    
	    ap->fun = evacuate(ap->fun);
	    scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	    break;
	}

	case PAP:
	case AP:
	    scavenge_PAP((StgPAP *)p);
	    break;
      
	case MUT_ARR_PTRS:
	    // follow everything 
	{
	    StgPtr next;
	    
	    evac_gen = 0;		// repeatedly mutable 
	    next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	    for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable anyhow.
	    break;
	}

	case MUT_ARR_PTRS_FROZEN:
	case MUT_ARR_PTRS_FROZEN0:
	    // follow everything 
	{
	    StgPtr next;
	    
	    next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	    for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    }
	    break;
	}

	case TSO:
	{ 
	    StgTSO *tso = (StgTSO *)p;
	    evac_gen = 0;
	    scavengeTSO(tso);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue;
	    break;
	}

#if defined(PAR)
	case RBH:
	{ 
#if 0
	    nat size, ptrs, nonptrs, vhs;
	    char str[80];
	    StgInfoTable *rip = get_closure_info(p, &size, &ptrs, &nonptrs, &vhs, str);
#endif
	    StgRBH *rbh = (StgRBH *)p;
	    bh->blocking_queue = 
		(StgTSO *)evacuate((StgClosure *)bh->blocking_queue);
	    failed_to_evac = rtsTrue;  // mutable anyhow.
	    IF_DEBUG(gc,
		     debugBelch("@@ scavenge: RBH %p (%s) (new blocking_queue link=%p)",
			   p, info_type(p), (StgClosure *)rbh->blocking_queue));
	    break;
	}
	
	case BLOCKED_FETCH:
	{ 
	    StgBlockedFetch *bf = (StgBlockedFetch *)p;
	    // follow the pointer to the node which is being demanded 
	    (StgClosure *)bf->node = 
		evacuate((StgClosure *)bf->node);
	    // follow the link to the rest of the blocking queue 
	    (StgClosure *)bf->link = 
		evacuate((StgClosure *)bf->link);
	    IF_DEBUG(gc,
		     debugBelch("@@ scavenge: %p (%s); node is now %p; exciting, isn't it",
			   bf, info_type((StgClosure *)bf), 
			   bf->node, info_type(bf->node)));
	    break;
	}

#ifdef DIST
	case REMOTE_REF:
#endif
	case FETCH_ME:
	    break; // nothing to do in this case

	case FETCH_ME_BQ:
	{ 
	    StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)p;
	    (StgClosure *)fmbq->blocking_queue = 
		evacuate((StgClosure *)fmbq->blocking_queue);
	    IF_DEBUG(gc,
		     debugBelch("@@ scavenge: %p (%s) exciting, isn't it",
			   p, info_type((StgClosure *)p)));
	    break;
	}
#endif /* PAR */

	case TVAR_WAIT_QUEUE:
	  {
	    StgTVarWaitQueue *wq = ((StgTVarWaitQueue *) p);
	    evac_gen = 0;
	    wq->waiting_tso = (StgTSO *)evacuate((StgClosure*)wq->waiting_tso);
	    wq->next_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)wq->next_queue_entry);
	    wq->prev_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)wq->prev_queue_entry);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }
	  
	case TVAR:
	  {
	    StgTVar *tvar = ((StgTVar *) p);
	    evac_gen = 0;
	    tvar->current_value = evacuate((StgClosure*)tvar->current_value);
	    tvar->first_wait_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)tvar->first_wait_queue_entry);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }
	  
	case TREC_CHUNK:
	  {
	    StgWord i;
	    StgTRecChunk *tc = ((StgTRecChunk *) p);
	    TRecEntry *e = &(tc -> entries[0]);
	    evac_gen = 0;
	    tc->prev_chunk = (StgTRecChunk *)evacuate((StgClosure*)tc->prev_chunk);
	    for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	      e->tvar = (StgTVar *)evacuate((StgClosure*)e->tvar);
	      e->expected_value = evacuate((StgClosure*)e->expected_value);
	      e->new_value = evacuate((StgClosure*)e->new_value);
	    }
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }

	case TREC_HEADER:
	  {
	    StgTRecHeader *trec = ((StgTRecHeader *) p);
	    evac_gen = 0;
	    trec->enclosing_trec = (StgTRecHeader *)evacuate((StgClosure*)trec->enclosing_trec);
	    trec->current_chunk = (StgTRecChunk *)evacuate((StgClosure*)trec->current_chunk);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsTrue; // mutable
	    break;
	  }

	default:
	    barf("scavenge_mark_stack: unimplemented/strange closure type %d @ %p", 
		 info->type, p);
	}

	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    recordMutableGen((StgClosure *)q, &generations[evac_gen]);
	}
	
	// mark the next bit to indicate "scavenged"
	mark(q+1, Bdescr(q));

    } // while (!mark_stack_empty())

    // start a new linear scan if the mark stack overflowed at some point
    if (mark_stack_overflowed && oldgen_scan_bd == NULL) {
	IF_DEBUG(gc, debugBelch("scavenge_mark_stack: starting linear scan"));
	mark_stack_overflowed = rtsFalse;
	oldgen_scan_bd = oldest_gen->steps[0].blocks;
	oldgen_scan = oldgen_scan_bd->start;
    }

    if (oldgen_scan_bd) {
	// push a new thing on the mark stack
    loop:
	// find a closure that is marked but not scavenged, and start
	// from there.
	while (oldgen_scan < oldgen_scan_bd->free 
	       && !is_marked(oldgen_scan,oldgen_scan_bd)) {
	    oldgen_scan++;
	}

	if (oldgen_scan < oldgen_scan_bd->free) {

	    // already scavenged?
	    if (is_marked(oldgen_scan+1,oldgen_scan_bd)) {
		oldgen_scan += sizeofW(StgHeader) + MIN_NONUPD_SIZE;
		goto loop;
	    }
	    push_mark_stack(oldgen_scan);
	    // ToDo: bump the linear scan by the actual size of the object
	    oldgen_scan += sizeofW(StgHeader) + MIN_NONUPD_SIZE;
	    goto linear_scan;
	}

	oldgen_scan_bd = oldgen_scan_bd->link;
	if (oldgen_scan_bd != NULL) {
	    oldgen_scan = oldgen_scan_bd->start;
	    goto loop;
	}
    }
}

/* -----------------------------------------------------------------------------
   Scavenge one object.

   This is used for objects that are temporarily marked as mutable
   because they contain old-to-new generation pointers.  Only certain
   objects can have this property.
   -------------------------------------------------------------------------- */

static rtsBool
scavenge_one(StgPtr p)
{
    const StgInfoTable *info;
    nat saved_evac_gen = evac_gen;
    rtsBool no_luck;
    
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);
    
    switch (info->type) {
	
    case MVAR:
    { 
	StgMVar *mvar = ((StgMVar *)p);
	evac_gen = 0;
	mvar->head = (StgTSO *)evacuate((StgClosure *)mvar->head);
	mvar->tail = (StgTSO *)evacuate((StgClosure *)mvar->tail);
	mvar->value = evacuate((StgClosure *)mvar->value);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable.
	break;
    }

    case FUN:
    case FUN_1_0:			// hardly worth specialising these guys
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case WEAK:
    case FOREIGN:
    case IND_PERM:
    {
	StgPtr q, end;
	
	end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (q = (StgPtr)((StgClosure *)p)->payload; q < end; q++) {
	    *q = (StgWord)(StgPtr)evacuate((StgClosure *)*q);
	}
	break;
    }
    
    case MUT_VAR:
	evac_gen = 0;
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable anyhow
	break;

    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
    case SE_BLACKHOLE:
    case BLACKHOLE:
	break;
	
    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	s->selectee = evacuate(s->selectee);
	break;
    }
    
    case AP_STACK:
    {
	StgAP_STACK *ap = (StgAP_STACK *)p;

	ap->fun = evacuate(ap->fun);
	scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	p = (StgPtr)ap->payload + ap->size;
	break;
    }

    case PAP:
    case AP:
	p = scavenge_PAP((StgPAP *)p);
	break;

    case ARR_WORDS:
	// nothing to follow 
	break;

    case MUT_ARR_PTRS:
    {
	// follow everything 
	StgPtr next;
      
	evac_gen = 0;		// repeatedly mutable 
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue;
	break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
    {
	// follow everything 
	StgPtr next;
      
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	break;
    }

    case TSO:
    {
	StgTSO *tso = (StgTSO *)p;
      
	evac_gen = 0;		// repeatedly mutable 
	scavengeTSO(tso);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue;
	break;
    }
  
#if defined(PAR)
    case RBH:
    { 
#if 0
	nat size, ptrs, nonptrs, vhs;
	char str[80];
	StgInfoTable *rip = get_closure_info(p, &size, &ptrs, &nonptrs, &vhs, str);
#endif
	StgRBH *rbh = (StgRBH *)p;
	(StgClosure *)rbh->blocking_queue = 
	    evacuate((StgClosure *)rbh->blocking_queue);
	failed_to_evac = rtsTrue;  // mutable anyhow.
	IF_DEBUG(gc,
		 debugBelch("@@ scavenge: RBH %p (%s) (new blocking_queue link=%p)",
		       p, info_type(p), (StgClosure *)rbh->blocking_queue));
	// ToDo: use size of reverted closure here!
	break;
    }

    case BLOCKED_FETCH:
    { 
	StgBlockedFetch *bf = (StgBlockedFetch *)p;
	// follow the pointer to the node which is being demanded 
	(StgClosure *)bf->node = 
	    evacuate((StgClosure *)bf->node);
	// follow the link to the rest of the blocking queue 
	(StgClosure *)bf->link = 
	    evacuate((StgClosure *)bf->link);
	IF_DEBUG(gc,
		 debugBelch("@@ scavenge: %p (%s); node is now %p; exciting, isn't it",
		       bf, info_type((StgClosure *)bf), 
		       bf->node, info_type(bf->node)));
	break;
    }

#ifdef DIST
    case REMOTE_REF:
#endif
    case FETCH_ME:
	break; // nothing to do in this case

    case FETCH_ME_BQ:
    { 
	StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)p;
	(StgClosure *)fmbq->blocking_queue = 
	    evacuate((StgClosure *)fmbq->blocking_queue);
	IF_DEBUG(gc,
		 debugBelch("@@ scavenge: %p (%s) exciting, isn't it",
		       p, info_type((StgClosure *)p)));
	break;
    }
#endif

    case TVAR_WAIT_QUEUE:
      {
	StgTVarWaitQueue *wq = ((StgTVarWaitQueue *) p);
	evac_gen = 0;
	wq->waiting_tso = (StgTSO *)evacuate((StgClosure*)wq->waiting_tso);
	wq->next_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)wq->next_queue_entry);
	wq->prev_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)wq->prev_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	break;
      }

    case TVAR:
      {
	StgTVar *tvar = ((StgTVar *) p);
	evac_gen = 0;
	tvar->current_value = evacuate((StgClosure*)tvar->current_value);
	tvar->first_wait_queue_entry = (StgTVarWaitQueue *)evacuate((StgClosure*)tvar->first_wait_queue_entry);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	break;
      }

    case TREC_HEADER:
      {
        StgTRecHeader *trec = ((StgTRecHeader *) p);
        evac_gen = 0;
	trec->enclosing_trec = (StgTRecHeader *)evacuate((StgClosure*)trec->enclosing_trec);
	trec->current_chunk = (StgTRecChunk *)evacuate((StgClosure*)trec->current_chunk);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
        break;
      }

    case TREC_CHUNK:
      {
	StgWord i;
	StgTRecChunk *tc = ((StgTRecChunk *) p);
	TRecEntry *e = &(tc -> entries[0]);
	evac_gen = 0;
	tc->prev_chunk = (StgTRecChunk *)evacuate((StgClosure*)tc->prev_chunk);
	for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
	  e->tvar = (StgTVar *)evacuate((StgClosure*)e->tvar);
	  e->expected_value = evacuate((StgClosure*)e->expected_value);
	  e->new_value = evacuate((StgClosure*)e->new_value);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsTrue; // mutable
	break;
      }

    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
    {
	/* Careful here: a THUNK can be on the mutable list because
	 * it contains pointers to young gen objects.  If such a thunk
	 * is updated, the IND_OLDGEN will be added to the mutable
	 * list again, and we'll scavenge it twice.  evacuate()
	 * doesn't check whether the object has already been
	 * evacuated, so we perform that check here.
	 */
	StgClosure *q = ((StgInd *)p)->indirectee;
	if (HEAP_ALLOCED(q) && Bdescr((StgPtr)q)->flags & BF_EVACUATED) {
	    break;
	}
	((StgInd *)p)->indirectee = evacuate(q);
    }

#if 0 && defined(DEBUG)
      if (RtsFlags.DebugFlags.gc) 
      /* Debugging code to print out the size of the thing we just
       * promoted 
       */
      { 
	StgPtr start = gen->steps[0].scan;
	bdescr *start_bd = gen->steps[0].scan_bd;
	nat size = 0;
	scavenge(&gen->steps[0]);
	if (start_bd != gen->steps[0].scan_bd) {
	  size += (P_)BLOCK_ROUND_UP(start) - start;
	  start_bd = start_bd->link;
	  while (start_bd != gen->steps[0].scan_bd) {
	    size += BLOCK_SIZE_W;
	    start_bd = start_bd->link;
	  }
	  size += gen->steps[0].scan -
	    (P_)BLOCK_ROUND_DOWN(gen->steps[0].scan);
	} else {
	  size = gen->steps[0].scan - start;
	}
	debugBelch("evac IND_OLDGEN: %ld bytes", size * sizeof(W_));
      }
#endif
      break;

    default:
	barf("scavenge_one: strange object %d", (int)(info->type));
    }    

    no_luck = failed_to_evac;
    failed_to_evac = rtsFalse;
    return (no_luck);
}

/* -----------------------------------------------------------------------------
   Scavenging mutable lists.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

static void
scavenge_mutable_list(generation *gen)
{
    bdescr *bd;
    StgPtr p, q;

    bd = gen->saved_mut_list;

    evac_gen = gen->no;
    for (; bd != NULL; bd = bd->link) {
	for (q = bd->start; q < bd->free; q++) {
	    p = (StgPtr)*q;
	    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
	    if (scavenge_one(p)) {
		/* didn't manage to promote everything, so put the
		 * object back on the list.
		 */
		recordMutableGen((StgClosure *)p,gen);
	    }
	}
    }

    // free the old mut_list
    freeChain(gen->saved_mut_list);
    gen->saved_mut_list = NULL;
}


static void
scavenge_static(void)
{
  StgClosure* p = static_objects;
  const StgInfoTable *info;

  /* Always evacuate straight to the oldest generation for static
   * objects */
  evac_gen = oldest_gen->no;

  /* keep going until we've scavenged all the objects on the linked
     list... */
  while (p != END_OF_STATIC_LIST) {

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl(p);
    /*
    if (info->type==RBH)
      info = REVERT_INFOPTR(info); // if it's an RBH, look at the orig closure
    */
    // make sure the info pointer is into text space 
    
    /* Take this object *off* the static_objects list,
     * and put it on the scavenged_static_objects list.
     */
    static_objects = STATIC_LINK(info,p);
    STATIC_LINK(info,p) = scavenged_static_objects;
    scavenged_static_objects = p;
    
    switch (info -> type) {
      
    case IND_STATIC:
      {
	StgInd *ind = (StgInd *)p;
	ind->indirectee = evacuate(ind->indirectee);

	/* might fail to evacuate it, in which case we have to pop it
	 * back on the mutable list of the oldest generation.  We
	 * leave it *on* the scavenged_static_objects list, though,
	 * in case we visit this object again.
	 */
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  recordMutableGen((StgClosure *)p,oldest_gen);
	}
	break;
      }
      
    case THUNK_STATIC:
      scavenge_thunk_srt(info);
      break;

    case FUN_STATIC:
      scavenge_fun_srt(info);
      break;
      
    case CONSTR_STATIC:
      {	
	StgPtr q, next;
	
	next = (P_)p->payload + info->layout.payload.ptrs;
	// evacuate the pointers 
	for (q = (P_)p->payload; q < next; q++) {
	    *q = (StgWord)(StgPtr)evacuate((StgClosure *)*q);
	}
	break;
      }
      
    default:
      barf("scavenge_static: strange closure %d", (int)(info->type));
    }

    ASSERT(failed_to_evac == rtsFalse);

    /* get the next static object from the list.  Remember, there might
     * be more stuff on this list now that we've done some evacuating!
     * (static_objects is a global)
     */
    p = static_objects;
  }
}

/* -----------------------------------------------------------------------------
   scavenge a chunk of memory described by a bitmap
   -------------------------------------------------------------------------- */

static void
scavenge_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, nat size )
{
    nat i, b;
    StgWord bitmap;
    
    b = 0;
    bitmap = large_bitmap->bitmap[b];
    for (i = 0; i < size; ) {
	if ((bitmap & 1) == 0) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	i++;
	p++;
	if (i % BITS_IN(W_) == 0) {
	    b++;
	    bitmap = large_bitmap->bitmap[b];
	} else {
	    bitmap = bitmap >> 1;
	}
    }
}

STATIC_INLINE StgPtr
scavenge_small_bitmap (StgPtr p, nat size, StgWord bitmap)
{
    while (size > 0) {
	if ((bitmap & 1) == 0) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	}
	p++;
	bitmap = bitmap >> 1;
	size--;
    }
    return p;
}

/* -----------------------------------------------------------------------------
   scavenge_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   AP_STACK_UPDs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */


static void
scavenge_stack(StgPtr p, StgPtr stack_end)
{
  const StgRetInfoTable* info;
  StgWord bitmap;
  nat size;

  //IF_DEBUG(sanity, debugBelch("  scavenging stack between %p and %p", p, stack_end));

  /* 
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with an activation record. 
   */

  while (p < stack_end) {
    info  = get_ret_itbl((StgClosure *)p);
      
    switch (info->i.type) {
	
    case UPDATE_FRAME:
	((StgUpdateFrame *)p)->updatee 
	    = evacuate(((StgUpdateFrame *)p)->updatee);
	p += sizeofW(StgUpdateFrame);
	continue;

      // small bitmap (< 32 entries, or 64 on a 64-bit machine) 
    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
    case STOP_FRAME:
    case CATCH_FRAME:
    case RET_SMALL:
    case RET_VEC_SMALL:
	bitmap = BITMAP_BITS(info->i.layout.bitmap);
	size   = BITMAP_SIZE(info->i.layout.bitmap);
	// NOTE: the payload starts immediately after the info-ptr, we
	// don't have an StgHeader in the same sense as a heap closure.
	p++;
	p = scavenge_small_bitmap(p, size, bitmap);

    follow_srt:
	scavenge_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap);
	continue;

    case RET_BCO: {
	StgBCO *bco;
	nat size;

	p++;
	*p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	bco = (StgBCO *)*p;
	p++;
	size = BCO_BITMAP_SIZE(bco);
	scavenge_large_bitmap(p, BCO_BITMAP(bco), size);
	p += size;
	continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine) 
    case RET_BIG:
    case RET_VEC_BIG:
    {
	nat size;

	size = GET_LARGE_BITMAP(&info->i)->size;
	p++;
	scavenge_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
	p += size;
	// and don't forget to follow the SRT 
	goto follow_srt;
    }

      // Dynamic bitmap: the mask is stored on the stack, and
      // there are a number of non-pointers followed by a number
      // of pointers above the bitmapped area.  (see StgMacros.h,
      // HEAP_CHK_GEN).
    case RET_DYN:
    {
	StgWord dyn;
	dyn = ((StgRetDyn *)p)->liveness;

	// traverse the bitmap first
	bitmap = RET_DYN_LIVENESS(dyn);
	p      = (P_)&((StgRetDyn *)p)->payload[0];
	size   = RET_DYN_BITMAP_SIZE;
	p = scavenge_small_bitmap(p, size, bitmap);

	// skip over the non-ptr words
	p += RET_DYN_NONPTRS(dyn) + RET_DYN_NONPTR_REGS_SIZE;
	
	// follow the ptr words
	for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
	    *p = (StgWord)(StgPtr)evacuate((StgClosure *)*p);
	    p++;
	}
	continue;
    }

    case RET_FUN:
    {
	StgRetFun *ret_fun = (StgRetFun *)p;
	StgFunInfoTable *fun_info;

	ret_fun->fun = evacuate(ret_fun->fun);
 	fun_info = get_fun_itbl(ret_fun->fun);
	p = scavenge_arg_block(fun_info, ret_fun->payload);
	goto follow_srt;
    }

    default:
	barf("scavenge_stack: weird activation record found on stack: %d", (int)(info->i.type));
    }
  }		     
}

/*-----------------------------------------------------------------------------
  scavenge the large object list.

  evac_gen set by caller; similar games played with evac_gen as with
  scavenge() - see comment at the top of scavenge().  Most large
  objects are (repeatedly) mutable, so most of the time evac_gen will
  be zero.
  --------------------------------------------------------------------------- */

static void
scavenge_large(step *stp)
{
  bdescr *bd;
  StgPtr p;

  bd = stp->new_large_objects;

  for (; bd != NULL; bd = stp->new_large_objects) {

    /* take this object *off* the large objects list and put it on
     * the scavenged large objects list.  This is so that we can
     * treat new_large_objects as a stack and push new objects on
     * the front when evacuating.
     */
    stp->new_large_objects = bd->link;
    dbl_link_onto(bd, &stp->scavenged_large_objects);

    // update the block count in this step.
    stp->n_scavenged_large_blocks += bd->blocks;

    p = bd->start;
    if (scavenge_one(p)) {
	recordMutableGen((StgClosure *)p, stp->gen);
    }
  }
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
    link = STATIC_LINK(info, p);
    STATIC_LINK(info,p) = NULL;
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
      IF_DEBUG(gccafs, debugBelch("CAF gc'd at 0x%04lx", (long)p));
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

  //  debugBelch("%d CAFs live", i); 
}
#endif


/* -----------------------------------------------------------------------------
   Lazy black holing.

   Whenever a thread returns to the scheduler after possibly doing
   some work, we have to run down the stack and black-hole all the
   closures referred to by update frames.
   -------------------------------------------------------------------------- */

static void
threadLazyBlackHole(StgTSO *tso)
{
    StgClosure *frame;
    StgRetInfoTable *info;
    StgClosure *bh;
    StgPtr stack_end;
    
    stack_end = &tso->stack[tso->stack_size];
    
    frame = (StgClosure *)tso->sp;

    while (1) {
	info = get_ret_itbl(frame);
	
	switch (info->i.type) {
	    
	case UPDATE_FRAME:
	    bh = ((StgUpdateFrame *)frame)->updatee;
	    
	    /* if the thunk is already blackholed, it means we've also
	     * already blackholed the rest of the thunks on this stack,
	     * so we can stop early.
	     *
	     * The blackhole made for a CAF is a CAF_BLACKHOLE, so they
	     * don't interfere with this optimisation.
	     */
	    if (bh->header.info == &stg_BLACKHOLE_info) {
		return;
	    }
	    
	    if (bh->header.info != &stg_CAF_BLACKHOLE_info) {
#if (!defined(LAZY_BLACKHOLING)) && defined(DEBUG)
		debugBelch("Unexpected lazy BHing required at 0x%04x\n",(int)bh);
#endif
#ifdef PROFILING
		// @LDV profiling
		// We pretend that bh is now dead.
		LDV_recordDead_FILL_SLOP_DYNAMIC((StgClosure *)bh);
#endif
		SET_INFO(bh,&stg_BLACKHOLE_info);

		// We pretend that bh has just been created.
		LDV_RECORD_CREATE(bh);
	    }
	    
	    frame = (StgClosure *) ((StgUpdateFrame *)frame + 1);
	    break;
	    
	case STOP_FRAME:
	    return;
	    
	    // normal stack frames; do nothing except advance the pointer
	default:
	    frame = (StgClosure *)((StgPtr)frame + stack_frame_sizeW(frame));
	}
    }
}


/* -----------------------------------------------------------------------------
 * Stack squeezing
 *
 * Code largely pinched from old RTS, then hacked to bits.  We also do
 * lazy black holing here.
 *
 * -------------------------------------------------------------------------- */

struct stack_gap { StgWord gap_size; struct stack_gap *next_gap; };

static void
threadSqueezeStack(StgTSO *tso)
{
    StgPtr frame;
    rtsBool prev_was_update_frame;
    StgClosure *updatee = NULL;
    StgPtr bottom;
    StgRetInfoTable *info;
    StgWord current_gap_size;
    struct stack_gap *gap;

    // Stage 1: 
    //    Traverse the stack upwards, replacing adjacent update frames
    //    with a single update frame and a "stack gap".  A stack gap
    //    contains two values: the size of the gap, and the distance
    //    to the next gap (or the stack top).

    bottom = &(tso->stack[tso->stack_size]);

    frame = tso->sp;

    ASSERT(frame < bottom);
    
    prev_was_update_frame = rtsFalse;
    current_gap_size = 0;
    gap = (struct stack_gap *) (tso->sp - sizeofW(StgUpdateFrame));

    while (frame < bottom) {
	
	info = get_ret_itbl((StgClosure *)frame);
	switch (info->i.type) {

	case UPDATE_FRAME:
	{ 
	    StgUpdateFrame *upd = (StgUpdateFrame *)frame;

	    if (upd->updatee->header.info == &stg_BLACKHOLE_info) {

		// found a BLACKHOLE'd update frame; we've been here
		// before, in a previous GC, so just break out.

		// Mark the end of the gap, if we're in one.
		if (current_gap_size != 0) {
		    gap = (struct stack_gap *)(frame-sizeofW(StgUpdateFrame));
		}
		
		frame += sizeofW(StgUpdateFrame);
		goto done_traversing;
	    }

	    if (prev_was_update_frame) {

		TICK_UPD_SQUEEZED();
		/* wasn't there something about update squeezing and ticky to be
		 * sorted out?  oh yes: we aren't counting each enter properly
		 * in this case.  See the log somewhere.  KSW 1999-04-21
		 *
		 * Check two things: that the two update frames don't point to
		 * the same object, and that the updatee_bypass isn't already an
		 * indirection.  Both of these cases only happen when we're in a
		 * block hole-style loop (and there are multiple update frames
		 * on the stack pointing to the same closure), but they can both
		 * screw us up if we don't check.
		 */
		if (upd->updatee != updatee && !closure_IND(upd->updatee)) {
		    UPD_IND_NOLOCK(upd->updatee, updatee);
		}

		// now mark this update frame as a stack gap.  The gap
		// marker resides in the bottom-most update frame of
		// the series of adjacent frames, and covers all the
		// frames in this series.
		current_gap_size += sizeofW(StgUpdateFrame);
		((struct stack_gap *)frame)->gap_size = current_gap_size;
		((struct stack_gap *)frame)->next_gap = gap;

		frame += sizeofW(StgUpdateFrame);
		continue;
	    } 

	    // single update frame, or the topmost update frame in a series
	    else {
		StgClosure *bh = upd->updatee;

		// Do lazy black-holing
		if (bh->header.info != &stg_BLACKHOLE_info &&
		    bh->header.info != &stg_CAF_BLACKHOLE_info) {
#if (!defined(LAZY_BLACKHOLING)) && defined(DEBUG)
		    debugBelch("Unexpected lazy BHing required at 0x%04x",(int)bh);
#endif
#ifdef DEBUG
		    /* zero out the slop so that the sanity checker can tell
		     * where the next closure is.
		     */
		    { 
			StgInfoTable *bh_info = get_itbl(bh);
			nat np = bh_info->layout.payload.ptrs, 
			    nw = bh_info->layout.payload.nptrs, i;
			/* don't zero out slop for a THUNK_SELECTOR,
			 * because its layout info is used for a
			 * different purpose, and it's exactly the
			 * same size as a BLACKHOLE in any case.
			 */
			if (bh_info->type != THUNK_SELECTOR) {
			    for (i = 0; i < np + nw; i++) {
				((StgClosure *)bh)->payload[i] = INVALID_OBJECT;
			    }
			}
		    }
#endif
#ifdef PROFILING
		    // We pretend that bh is now dead.
		    LDV_recordDead_FILL_SLOP_DYNAMIC((StgClosure *)bh);
#endif
		    // Todo: maybe use SET_HDR() and remove LDV_RECORD_CREATE()?
		    SET_INFO(bh,&stg_BLACKHOLE_info);

		    // We pretend that bh has just been created.
		    LDV_RECORD_CREATE(bh);
		}

		prev_was_update_frame = rtsTrue;
		updatee = upd->updatee;
		frame += sizeofW(StgUpdateFrame);
		continue;
	    }
	}
	    
	default:
	    prev_was_update_frame = rtsFalse;

	    // we're not in a gap... check whether this is the end of a gap
	    // (an update frame can't be the end of a gap).
	    if (current_gap_size != 0) {
		gap = (struct stack_gap *) (frame - sizeofW(StgUpdateFrame));
	    }
	    current_gap_size = 0;

	    frame += stack_frame_sizeW((StgClosure *)frame);
	    continue;
	}
    }

done_traversing:
	    
    // Now we have a stack with gaps in it, and we have to walk down
    // shoving the stack up to fill in the gaps.  A diagram might
    // help:
    //
    //    +| ********* |
    //     | ********* | <- sp
    //     |           |
    //     |           | <- gap_start
    //     | ......... |                |
    //     | stack_gap | <- gap         | chunk_size
    //     | ......... |                | 
    //     | ......... | <- gap_end     v
    //     | ********* | 
    //     | ********* | 
    //     | ********* | 
    //    -| ********* | 
    //
    // 'sp'  points the the current top-of-stack
    // 'gap' points to the stack_gap structure inside the gap
    // *****   indicates real stack data
    // .....   indicates gap
    // <empty> indicates unused
    //
    {
	void *sp;
	void *gap_start, *next_gap_start, *gap_end;
	nat chunk_size;

	next_gap_start = (void *)((unsigned char*)gap + sizeof(StgUpdateFrame));
	sp = next_gap_start;

	while ((StgPtr)gap > tso->sp) {

	    // we're working in *bytes* now...
	    gap_start = next_gap_start;
	    gap_end = (void*) ((unsigned char*)gap_start - gap->gap_size * sizeof(W_));

	    gap = gap->next_gap;
	    next_gap_start = (void *)((unsigned char*)gap + sizeof(StgUpdateFrame));

	    chunk_size = (unsigned char*)gap_end - (unsigned char*)next_gap_start;
	    sp -= chunk_size;
	    memmove(sp, next_gap_start, chunk_size);
	}

	tso->sp = (StgPtr)sp;
    }
}    

/* -----------------------------------------------------------------------------
 * Pausing a thread
 * 
 * We have to prepare for GC - this means doing lazy black holing
 * here.  We also take the opportunity to do stack squeezing if it's
 * turned on.
 * -------------------------------------------------------------------------- */
void
threadPaused(StgTSO *tso)
{
  if ( RtsFlags.GcFlags.squeezeUpdFrames == rtsTrue )
    threadSqueezeStack(tso);	// does black holing too 
  else
    threadLazyBlackHole(tso);
}

/* -----------------------------------------------------------------------------
 * Debugging
 * -------------------------------------------------------------------------- */

#if DEBUG
void
printMutableList(generation *gen)
{
    bdescr *bd;
    StgPtr p;

    debugBelch("@@ Mutable list %p: ", gen->mut_list);

    for (bd = gen->mut_list; bd != NULL; bd = bd->link) {
	for (p = bd->start; p < bd->free; p++) {
	    debugBelch("%p (%s), ", (void *)*p, info_type((StgClosure *)*p));
	}
    }
    debugBelch("\n");
}

STATIC_INLINE rtsBool
maybeLarge(StgClosure *closure)
{
  StgInfoTable *info = get_itbl(closure);

  /* closure types that may be found on the new_large_objects list; 
     see scavenge_large */
  return (info->type == MUT_ARR_PTRS ||
	  info->type == MUT_ARR_PTRS_FROZEN ||
	  info->type == MUT_ARR_PTRS_FROZEN0 ||
	  info->type == TSO ||
	  info->type == ARR_WORDS);
}

  
#endif /* DEBUG */
