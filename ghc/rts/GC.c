/* -----------------------------------------------------------------------------
 * $Id: GC.c,v 1.117 2001/08/08 13:45:02 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Generational garbage collector
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "StoragePriv.h"
#include "Stats.h"
#include "Schedule.h"
#include "SchedAPI.h"		// for ReverCAFs prototype
#include "Sanity.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "Main.h"
#include "ProfHeap.h"
#include "SchedAPI.h"
#include "Weak.h"
#include "StablePriv.h"
#include "Prelude.h"
#include "ParTicky.h"		// ToDo: move into Rts.h
#include "GCCompact.h"
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
StgClosure* static_objects;	      // live static objects
StgClosure* scavenged_static_objects; // static objects scavenged so far

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
static rtsBool weak_done;	   // all done for this pass

/* List of all threads during GC
 */
static StgTSO *old_all_threads;
static StgTSO *resurrected_threads;

/* Flag indicating failure to evacuate an object to the desired
 * generation.
 */
static rtsBool failed_to_evac;

/* Old to-space (used for two-space collector only)
 */
bdescr *old_to_blocks;

/* Data used for allocation area sizing.
 */
lnat new_blocks;		// blocks allocated during this GC 
lnat g0s0_pcnt_kept = 30;	// percentage of g0s0 live at last minor GC 

/* Used to avoid long recursion due to selector thunks
 */
lnat thunk_selector_depth = 0;
#define MAX_THUNK_SELECTOR_DEPTH 256

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static void         mark_root               ( StgClosure **root );
static StgClosure * evacuate                ( StgClosure *q );
static void         zero_static_object_list ( StgClosure* first_static );
static void         zero_mutable_list       ( StgMutClosure *first );

static rtsBool      traverse_weak_ptr_list  ( void );
static void         mark_weak_ptr_list      ( StgWeak **list );

static void         scavenge                ( step * );
static void         scavenge_mark_stack     ( void );
static void         scavenge_stack          ( StgPtr p, StgPtr stack_end );
static rtsBool      scavenge_one            ( StgPtr p );
static void         scavenge_large          ( step * );
static void         scavenge_static         ( void );
static void         scavenge_mutable_list   ( generation *g );
static void         scavenge_mut_once_list  ( generation *g );
static void         scavengeCAFs            ( void );

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

static inline rtsBool
mark_stack_empty(void)
{
    return mark_sp == mark_stack;
}

static inline rtsBool
mark_stack_full(void)
{
    return mark_sp >= mark_splim;
}

static inline void
reset_mark_stack(void)
{
    mark_sp = mark_stack;
}

static inline void
push_mark_stack(StgPtr p)
{
    *mark_sp++ = p;
}

static inline StgPtr
pop_mark_stack(void)
{
    return *--mark_sp;
}

/* -----------------------------------------------------------------------------
   GarbageCollect

   For garbage collecting generation N (and all younger generations):

     - follow all pointers in the root set.  the root set includes all 
       mutable objects in all steps in all generations.

     - for each pointer, evacuate the object it points to into either
       + to-space in the next higher step in that generation, if one exists,
       + if the object's generation == N, then evacuate it to the next
         generation if one exists, or else to-space in the current
	 generation.
       + if the object's generation < N, then evacuate it to to-space
         in the next generation.

     - repeatedly scavenge to-space from each step in each generation
       being collected until no more objects can be evacuated.
      
     - free from-space in each step, and set from-space = to-space.

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
  IF_DEBUG(gc, belch("@@ Starting garbage collection at %ld (%lx)\n", 
		     Now, Now));
#endif

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

  /* zero the mutable list for the oldest generation (see comment by
   * zero_mutable_list below).
   */
  if (major_gc) { 
    zero_mutable_list(generations[RtsFlags.GcFlags.generations-1].mut_once_list);
  }

  /* Save the old to-space if we're doing a two-space collection
   */
  if (RtsFlags.GcFlags.generations == 1) {
    old_to_blocks = g0s0->to_blocks;
    g0s0->to_blocks = NULL;
  }

  /* Keep a count of how many new blocks we allocated during this GC
   * (used for resizing the allocation area, later).
   */
  new_blocks = 0;

  /* Initialise to-space in all the generations/steps that we're
   * collecting.
   */
  for (g = 0; g <= N; g++) {
    generations[g].mut_once_list = END_MUT_LIST;
    generations[g].mut_list = END_MUT_LIST;

    for (s = 0; s < generations[g].n_steps; s++) {

      // generation 0, step 0 doesn't need to-space 
      if (g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1) { 
	continue; 
      }

      /* Get a free block for to-space.  Extra blocks will be chained on
       * as necessary.
       */
      bd = allocBlock();
      stp = &generations[g].steps[s];
      ASSERT(stp->gen_no == g);
      ASSERT(stp->hp ? Bdescr(stp->hp)->step == stp : rtsTrue);
      bd->gen_no = g;
      bd->step = stp;
      bd->link = NULL;
      bd->flags        = BF_EVACUATED;	// it's a to-space block 
      stp->hp          = bd->start;
      stp->hpLim       = stp->hp + BLOCK_SIZE_W;
      stp->hp_bd       = bd;
      stp->to_blocks   = bd;
      stp->n_to_blocks = 1;
      stp->scan        = bd->start;
      stp->scan_bd     = bd;
      stp->new_large_objects = NULL;
      stp->scavenged_large_objects = NULL;
      stp->n_scavenged_large_blocks = 0;
      new_blocks++;
      // mark the large objects as not evacuated yet 
      for (bd = stp->large_objects; bd; bd = bd->link) {
	bd->flags = BF_LARGE;
      }

      // for a compacted step, we need to allocate the bitmap
      if (stp->is_compacted) {
	  nat bitmap_size; // in bytes
	  bdescr *bitmap_bdescr;
	  StgWord *bitmap;

	  bitmap_size = stp->n_blocks * BLOCK_SIZE / (sizeof(W_)*BITS_PER_BYTE);

	  if (bitmap_size > 0) {
	      bitmap_bdescr = allocGroup((nat)BLOCK_ROUND_UP(bitmap_size) 
					 / BLOCK_SIZE);
	      stp->bitmap = bitmap_bdescr;
	      bitmap = bitmap_bdescr->start;
	      
	      IF_DEBUG(gc, belch("bitmap_size: %d, bitmap: %p",
				   bitmap_size, bitmap););
	      
	      // don't forget to fill it with zeros!
	      memset(bitmap, 0, bitmap_size);
	      
	      // for each block in this step, point to its bitmap from the
	      // block descriptor.
	      for (bd=stp->blocks; bd != NULL; bd = bd->link) {
		  bd->u.bitmap = bitmap;
		  bitmap += BLOCK_SIZE_W / (sizeof(W_)*BITS_PER_BYTE);
	      }
	  }
      }
    }
  }

  /* make sure the older generations have at least one block to
   * allocate into (this makes things easier for copy(), see below.
   */
  for (g = N+1; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      stp = &generations[g].steps[s];
      if (stp->hp_bd == NULL) {
	  ASSERT(stp->blocks == NULL);
	  bd = allocBlock();
	  bd->gen_no = g;
	  bd->step = stp;
	  bd->link = NULL;
	  bd->flags = 0;	// *not* a to-space block or a large object
	  stp->hp = bd->start;
	  stp->hpLim = stp->hp + BLOCK_SIZE_W;
	  stp->hp_bd = bd;
	  stp->blocks = bd;
	  stp->n_blocks = 1;
	  new_blocks++;
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
      generations[g].mut_list = END_MUT_LIST;
    }

    // Do the mut-once lists first 
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      IF_PAR_DEBUG(verbose,
		   printMutOnceList(&generations[g]));
      scavenge_mut_once_list(&generations[g]);
      evac_gen = g;
      for (st = generations[g].n_steps-1; st >= 0; st--) {
	scavenge(&generations[g].steps[st]);
      }
    }

    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      IF_PAR_DEBUG(verbose,
		   printMutableList(&generations[g]));
      scavenge_mutable_list(&generations[g]);
      evac_gen = g;
      for (st = generations[g].n_steps-1; st >= 0; st--) {
	scavenge(&generations[g].steps[st]);
      }
    }
  }

  scavengeCAFs();

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
  weak_done = rtsFalse;

  /* The all_threads list is like the weak_ptr_list.  
   * See traverse_weak_ptr_list() for the details.
   */
  old_all_threads = all_threads;
  all_threads = END_TSO_QUEUE;
  resurrected_threads = END_TSO_QUEUE;

  /* Mark the stable pointer table.
   */
  markStablePtrTable(mark_root);

#ifdef INTERPRETER
  { 
      /* ToDo: To fix the caf leak, we need to make the commented out
       * parts of this code do something sensible - as described in 
       * the CAF document.
       */
      extern void markHugsObjects(void);
      markHugsObjects();
  }
#endif

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

    // must be last... 
    if (traverse_weak_ptr_list()) { // returns rtsTrue if evaced something 
      goto loop;
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
	      stp->hp_bd->free = stp->hp;
	      stp->hp_bd->link = NULL;
	  }
      }
  }

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
		    bd->flags &= ~BF_EVACUATED;	// now from-space 
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
		    }
		}
		// add the new blocks to the block tally
		stp->n_blocks += stp->n_to_blocks;
	    } else {
		freeChain(stp->blocks);
		stp->blocks = stp->to_blocks;
		stp->n_blocks = stp->n_to_blocks;
		for (bd = stp->blocks; bd != NULL; bd = bd->link) {
		    bd->flags &= ~BF_EVACUATED;	// now from-space 
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
      min_alloc = (RtsFlags.GcFlags.pcFreeHeap * max) / 200;

      // if we're going to go over the maximum heap size, reduce the
      // size of the generations accordingly.  The calculation is
      // different if compaction is turned on, because we don't need
      // to double the space required to collect the old generation.
      if (max != 0) {
	  if (RtsFlags.GcFlags.compact) {
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
      fprintf(stderr,"live: %d, min_alloc: %d, size : %d, max = %d\n", live,
	      min_alloc, size, max);
#endif

      for (g = 0; g < gens; g++) {
	  generations[g].max_blocks = size;
      }

      // Auto-enable compaction when the residency reaches a
      // certain percentage of the maximum heap size (default: 30%).
      if (RtsFlags.GcFlags.compact &&
	  max > 0 && 
	  oldest_gen->steps[0].n_blocks > 
	    (RtsFlags.GcFlags.compactThreshold * max) / 100) {
	  oldest_gen->steps[0].is_compacted = 1;
//	  fprintf(stderr,"compaction: on\n", live);
      } else {
	  oldest_gen->steps[0].is_compacted = 0;
//	  fprintf(stderr,"compaction: off\n", live);
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

    if ( blocks * RtsFlags.GcFlags.oldGenFactor * 2 > 
	 RtsFlags.GcFlags.maxHeapSize ) {
      long adjusted_blocks;  // signed on purpose 
      int pc_free; 
      
      adjusted_blocks = (RtsFlags.GcFlags.maxHeapSize - 2 * blocks);
      IF_DEBUG(gc, belch("@@ Near maximum heap size of 0x%x blocks, blocks = %d, adjusted to %ld", RtsFlags.GcFlags.maxHeapSize, blocks, adjusted_blocks));
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
    resizeNursery(blocks);
    
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
	g0s0_pcnt_kept = (new_blocks * 100) / g0s0->n_blocks;
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
      
      resizeNursery((nat)blocks);
    }
  }

 // mark the garbage collected CAFs as dead 
#if 0 && defined(DEBUG) // doesn't work at the moment 
  if (major_gc) { gcCAFs(); }
#endif
  
  // zero the scavenged static object list 
  if (major_gc) {
    zero_static_object_list(scavenged_static_objects);
  }

  // Reset the nursery
  resetNurseries();

  // start any pending finalizers 
  scheduleFinalizers(old_weak_ptr_list);
  
  // send exceptions to any threads which were about to die 
  resurrectThreads(resurrected_threads);

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
  heapCensus();
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
   -------------------------------------------------------------------------- */

static rtsBool 
traverse_weak_ptr_list(void)
{
  StgWeak *w, **last_w, *next_w;
  StgClosure *new;
  rtsBool flag = rtsFalse;

  if (weak_done) { return rtsFalse; }

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

    ASSERT(get_itbl(w)->type == WEAK);

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
      IF_DEBUG(weak, belch("Weak pointer still alive at %p -> %p", w, w->key));
      continue;
    }
    else {
      last_w = &(w->link);
      next_w = w->link;
      continue;
    }
  }

  /* Now deal with the all_threads list, which behaves somewhat like
   * the weak ptr list.  If we discover any threads that are about to
   * become garbage, we wake them up and administer an exception.
   */
  {
    StgTSO *t, *tmp, *next, **prev;

    prev = &old_all_threads;
    for (t = old_all_threads; t != END_TSO_QUEUE; t = next) {

      (StgClosure *)tmp = isAlive((StgClosure *)t);
      
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
	  // not alive (yet): leave this thread on the old_all_threads list.
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

  /* If we didn't make any changes, then we can go round and kill all
   * the dead weak pointers.  The old_weak_ptr list is used as a list
   * of pending finalizers later on.
   */
  if (flag == rtsFalse) {
    for (w = old_weak_ptr_list; w; w = w->link) {
      w->finalizer = evacuate(w->finalizer);
    }

    /* And resurrect any threads which were about to become garbage.
     */
    {
      StgTSO *t, *tmp, *next;
      for (t = old_all_threads; t != END_TSO_QUEUE; t = next) {
	next = t->global_link;
	(StgClosure *)tmp = evacuate((StgClosure *)t);
	tmp->global_link = resurrected_threads;
	resurrected_threads = tmp;
      }
    }

    weak_done = rtsTrue;
  }

  return rtsTrue;
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
      (StgClosure *)w = evacuate((StgClosure *)w);
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

    info = get_itbl(p);

    /* ToDo: for static closures, check the static link field.
     * Problem here is that we sometimes don't set the link field, eg.
     * for static closures with an empty SRT or CONSTR_STATIC_NOCAFs.
     */

  loop:
    bd = Bdescr((P_)p);
    // ignore closures in generations that we're not collecting. 
    if (LOOKS_LIKE_STATIC(p) || bd->gen_no > N) {
	return p;
    }
    // large objects have an evacuated flag
    if (bd->flags & BF_LARGE) {
	if (bd->flags & BF_EVACUATED) {
	    return p;
	} else {
	    return NULL;
	}
    }
    // check the mark bit for compacted steps
    if (bd->step->is_compacted && is_marked((P_)p,bd)) {
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
	goto loop;
      }

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

static void
addBlock(step *stp)
{
  bdescr *bd = allocBlock();
  bd->gen_no = stp->gen_no;
  bd->step = stp;

  if (stp->gen_no <= N) {
    bd->flags = BF_EVACUATED;
  } else {
    bd->flags = 0;
  }

  stp->hp_bd->free = stp->hp;
  stp->hp_bd->link = bd;
  stp->hp = bd->start;
  stp->hpLim = stp->hp + BLOCK_SIZE_W;
  stp->hp_bd = bd;
  stp->n_to_blocks++;
  new_blocks++;
}


static __inline__ void 
upd_evacuee(StgClosure *p, StgClosure *dest)
{
  p->header.info = &stg_EVACUATED_info;
  ((StgEvacuated *)p)->evacuee = dest;
}


static __inline__ StgClosure *
copy(StgClosure *src, nat size, step *stp)
{
  P_ to, from, dest;

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
    addBlock(stp);
  }

  for(to = stp->hp, from = (P_)src; size>0; --size) {
    *to++ = *from++;
  }

  dest = stp->hp;
  stp->hp = to;
  upd_evacuee(src,(StgClosure *)dest);
  return (StgClosure *)dest;
}

/* Special version of copy() for when we only want to copy the info
 * pointer of an object, but reserve some padding after it.  This is
 * used to optimise evacuation of BLACKHOLEs.
 */


static __inline__ StgClosure *
copyPart(StgClosure *src, nat size_to_reserve, nat size_to_copy, step *stp)
{
  P_ dest, to, from;

  TICK_GC_WORDS_COPIED(size_to_copy);
  if (stp->gen_no < evac_gen) {
#ifdef NO_EAGER_PROMOTION    
    failed_to_evac = rtsTrue;
#else
    stp = &generations[evac_gen].steps[0];
#endif
  }

  if (stp->hp + size_to_reserve >= stp->hpLim) {
    addBlock(stp);
  }

  for(to = stp->hp, from = (P_)src; size_to_copy>0; --size_to_copy) {
    *to++ = *from++;
  }
  
  dest = stp->hp;
  stp->hp += size_to_reserve;
  upd_evacuee(src,(StgClosure *)dest);
  return (StgClosure *)dest;
}


/* -----------------------------------------------------------------------------
   Evacuate a large object

   This just consists of removing the object from the (doubly-linked)
   large_alloc_list, and linking it on to the (singly-linked)
   new_large_objects list, from where it will be scavenged later.

   Convention: bd->flags has BF_EVACUATED set for a large object
   that has been evacuated, or unset otherwise.
   -------------------------------------------------------------------------- */


static inline void
evacuate_large(StgPtr p)
{
  bdescr *bd = Bdescr(p);
  step *stp;

  // should point to the beginning of the block 
  ASSERT(((W_)p & BLOCK_MASK) == 0);
  
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
   Adding a MUT_CONS to an older generation.

   This is necessary from time to time when we end up with an
   old-to-new generation pointer in a non-mutable object.  We defer
   the promotion until the next GC.
   -------------------------------------------------------------------------- */


static StgClosure *
mkMutCons(StgClosure *ptr, generation *gen)
{
  StgMutVar *q;
  step *stp;

  stp = &gen->steps[0];

  /* chain a new block onto the to-space for the destination step if
   * necessary.
   */
  if (stp->hp + sizeofW(StgIndOldGen) >= stp->hpLim) {
    addBlock(stp);
  }

  q = (StgMutVar *)stp->hp;
  stp->hp += sizeofW(StgMutVar);

  SET_HDR(q,&stg_MUT_CONS_info,CCS_GC);
  q->var = ptr;
  recordOldToNewPtrs((StgMutClosure *)q);

  return (StgClosure *)q;
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

   -------------------------------------------------------------------------- */

static StgClosure *
evacuate(StgClosure *q)
{
  StgClosure *to;
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
    if (bd->step->is_compacted) {
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

    stp = bd->step->to;
  }
#ifdef DEBUG
  else stp = NULL; // make sure copy() will crash if HEAP_ALLOCED is wrong 
#endif

  // make sure the info pointer is into text space 
  ASSERT(q && (LOOKS_LIKE_GHC_INFO(GET_INFO(q))
	       || IS_HUGS_CONSTR_INFO(GET_INFO(q))));
  info = get_itbl(q);
  
  switch (info -> type) {

  case MUT_VAR:
  case MVAR:
      to = copy(q,sizeW_fromITBL(info),stp);
      return to;

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
    return copy(q,sizeofW(StgHeader)+1,stp);

  case THUNK_1_0:		// here because of MIN_UPD_SIZE 
  case THUNK_0_1:
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
  case BCO:
    return copy(q,sizeW_fromITBL(info),stp);

  case CAF_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case SE_BLACKHOLE:
  case BLACKHOLE:
    return copyPart(q,BLACKHOLE_sizeW(),sizeofW(StgHeader),stp);

  case BLACKHOLE_BQ:
    to = copy(q,BLACKHOLE_sizeW(),stp); 
    return to;

  case THUNK_SELECTOR:
    {
      const StgInfoTable* selectee_info;
      StgClosure* selectee = ((StgSelector*)q)->selectee;

    selector_loop:
      selectee_info = get_itbl(selectee);
      switch (selectee_info->type) {
      case CONSTR:
      case CONSTR_1_0:
      case CONSTR_0_1:
      case CONSTR_2_0:
      case CONSTR_1_1:
      case CONSTR_0_2:
      case CONSTR_STATIC:
	{ 
	  StgWord offset = info->layout.selector_offset;

	  // check that the size is in range 
	  ASSERT(offset < 
		 (StgWord32)(selectee_info->layout.payload.ptrs + 
		            selectee_info->layout.payload.nptrs));

	  // perform the selection! 
	  q = selectee->payload[offset];

	  /* if we're already in to-space, there's no need to continue
	   * with the evacuation, just update the source address with
	   * a pointer to the (evacuated) constructor field.
	   */
	  if (HEAP_ALLOCED(q)) {
	    bdescr *bd = Bdescr((P_)q);
	    if (bd->flags & BF_EVACUATED) {
	      if (bd->gen_no < evac_gen) {
		failed_to_evac = rtsTrue;
		TICK_GC_FAILED_PROMOTION();
	      }
	      return q;
	    }
	  }

	  /* otherwise, carry on and evacuate this constructor field,
	   * (but not the constructor itself)
	   */
	  goto loop;
	}

      case IND:
      case IND_STATIC:
      case IND_PERM:
      case IND_OLDGEN:
      case IND_OLDGEN_PERM:
	selectee = ((StgInd *)selectee)->indirectee;
	goto selector_loop;

      case EVACUATED:
	selectee = ((StgEvacuated *)selectee)->evacuee;
	goto selector_loop;

      case THUNK_SELECTOR:
#         if 0
          /* Disabled 03 April 2001 by JRS; it seems to cause the GC (or
             something) to go into an infinite loop when the nightly
             stage2 compiles PrelTup.lhs. */

	  /* we can't recurse indefinitely in evacuate(), so set a
	   * limit on the number of times we can go around this
	   * loop.
	   */
	  if (thunk_selector_depth < MAX_THUNK_SELECTOR_DEPTH) {
	      bdescr *bd;
	      bd = Bdescr((P_)selectee);
	      if (!bd->flags & BF_EVACUATED) {
		  thunk_selector_depth++;
		  selectee = evacuate(selectee);
		  thunk_selector_depth--;
		  goto selector_loop;
	      }
	  }
	  // otherwise, fall through... 
#         endif

      case AP_UPD:
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
      case BLACKHOLE_BQ:
	// not evaluated yet 
	break;

#if defined(PAR)
	// a copy of the top-level cases below 
      case RBH: // cf. BLACKHOLE_BQ
  	{
  	  //StgInfoTable *rip = get_closure_info(q, &size, &ptrs, &nonptrs, &vhs, str);
  	  to = copy(q,BLACKHOLE_sizeW(),stp); 
  	  //ToDo: derive size etc from reverted IP
  	  //to = copy(q,size,stp);
  	  // recordMutable((StgMutClosure *)to);
  	  return to;
  	}
    
      case BLOCKED_FETCH:
  	ASSERT(sizeofW(StgBlockedFetch) >= MIN_NONUPD_SIZE);
  	to = copy(q,sizeofW(StgBlockedFetch),stp);
  	return to;

# ifdef DIST    
      case REMOTE_REF:
# endif
      case FETCH_ME:
  	ASSERT(sizeofW(StgBlockedFetch) >= MIN_UPD_SIZE);
  	to = copy(q,sizeofW(StgFetchMe),stp);
  	return to;
    
      case FETCH_ME_BQ:
  	ASSERT(sizeofW(StgBlockedFetch) >= MIN_UPD_SIZE);
  	to = copy(q,sizeofW(StgFetchMeBlockingQueue),stp);
  	return to;
#endif

      default:
	barf("evacuate: THUNK_SELECTOR: strange selectee %d",
	     (int)(selectee_info->type));
      }
    }
    return copy(q,THUNK_SELECTOR_sizeW(),stp);

  case IND:
  case IND_OLDGEN:
    // follow chains of indirections, don't evacuate them 
    q = ((StgInd*)q)->indirectee;
    goto loop;

  case THUNK_STATIC:
    if (info->srt_len > 0 && major_gc && 
	THUNK_STATIC_LINK((StgClosure *)q) == NULL) {
      THUNK_STATIC_LINK((StgClosure *)q) = static_objects;
      static_objects = (StgClosure *)q;
    }
    return q;

  case FUN_STATIC:
    if (info->srt_len > 0 && major_gc && 
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
  case SEQ_FRAME:
    // shouldn't see these 
    barf("evacuate: stack frame at %p\n", q);

  case AP_UPD:
  case PAP:
    /* PAPs and AP_UPDs are special - the payload is a copy of a chunk
     * of stack, tagging and all.
     */
      return copy(q,pap_sizeW((StgPAP*)q),stp);

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
      if (Bdescr((P_)p)->gen_no < evac_gen) {
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
	  StgTSO *new_tso = (StgTSO *)copy((StgClosure *)tso,tso_sizeW(tso),stp);
	  move_TSO(tso, new_tso);
	  return (StgClosure *)new_tso;
      }
    }

#if defined(PAR)
  case RBH: // cf. BLACKHOLE_BQ
    {
      //StgInfoTable *rip = get_closure_info(q, &size, &ptrs, &nonptrs, &vhs, str);
      to = copy(q,BLACKHOLE_sizeW(),stp); 
      //ToDo: derive size etc from reverted IP
      //to = copy(q,size,stp);
      IF_DEBUG(gc,
	       belch("@@ evacuate: RBH %p (%s) to %p (%s)",
		     q, info_type(q), to, info_type(to)));
      return to;
    }

  case BLOCKED_FETCH:
    ASSERT(sizeofW(StgBlockedFetch) >= MIN_NONUPD_SIZE);
    to = copy(q,sizeofW(StgBlockedFetch),stp);
    IF_DEBUG(gc,
	     belch("@@ evacuate: %p (%s) to %p (%s)",
		   q, info_type(q), to, info_type(to)));
    return to;

# ifdef DIST    
  case REMOTE_REF:
# endif
  case FETCH_ME:
    ASSERT(sizeofW(StgBlockedFetch) >= MIN_UPD_SIZE);
    to = copy(q,sizeofW(StgFetchMe),stp);
    IF_DEBUG(gc,
	     belch("@@ evacuate: %p (%s) to %p (%s)",
		   q, info_type(q), to, info_type(to)));
    return to;

  case FETCH_ME_BQ:
    ASSERT(sizeofW(StgBlockedFetch) >= MIN_UPD_SIZE);
    to = copy(q,sizeofW(StgFetchMeBlockingQueue),stp);
    IF_DEBUG(gc,
	     belch("@@ evacuate: %p (%s) to %p (%s)",
		   q, info_type(q), to, info_type(to)));
    return to;
#endif

  default:
    barf("evacuate: strange closure type %d", (int)(info->type));
  }

  barf("evacuate");
}

/* -----------------------------------------------------------------------------
   move_TSO is called to update the TSO structure after it has been
   moved from one place to another.
   -------------------------------------------------------------------------- */

void
move_TSO(StgTSO *src, StgTSO *dest)
{
    ptrdiff_t diff;

    // relocate the stack pointers... 
    diff = (StgPtr)dest - (StgPtr)src; // In *words* 
    dest->sp = (StgPtr)dest->sp + diff;
    dest->su = (StgUpdateFrame *) ((P_)dest->su + diff);

    relocate_stack(dest, diff);
}

/* -----------------------------------------------------------------------------
   relocate_stack is called to update the linkage between
   UPDATE_FRAMEs (and SEQ_FRAMEs etc.) when a stack is moved from one
   place to another.
   -------------------------------------------------------------------------- */

StgTSO *
relocate_stack(StgTSO *dest, ptrdiff_t diff)
{
  StgUpdateFrame *su;
  StgCatchFrame  *cf;
  StgSeqFrame    *sf;

  su = dest->su;

  while ((P_)su < dest->stack + dest->stack_size) {
    switch (get_itbl(su)->type) {
   
      // GCC actually manages to common up these three cases! 

    case UPDATE_FRAME:
      su->link = (StgUpdateFrame *) ((StgPtr)su->link + diff);
      su = su->link;
      continue;

    case CATCH_FRAME:
      cf = (StgCatchFrame *)su;
      cf->link = (StgUpdateFrame *) ((StgPtr)cf->link + diff);
      su = cf->link;
      continue;

    case SEQ_FRAME:
      sf = (StgSeqFrame *)su;
      sf->link = (StgUpdateFrame *) ((StgPtr)sf->link + diff);
      su = sf->link;
      continue;

    case STOP_FRAME:
      // all done! 
      break;

    default:
      barf("relocate_stack %d", (int)(get_itbl(su)->type));
    }
    break;
  }

  return dest;
}



static inline void
scavenge_srt(const StgInfoTable *info)
{
  StgClosure **srt, **srt_end;

  /* evacuate the SRT.  If srt_len is zero, then there isn't an
   * srt field in the info table.  That's ok, because we'll
   * never dereference it.
   */
  srt = (StgClosure **)(info->srt);
  srt_end = srt + info->srt_len;
  for (; srt < srt_end; srt++) {
    /* Special-case to handle references to closures hiding out in DLLs, since
       double indirections required to get at those. The code generator knows
       which is which when generating the SRT, so it stores the (indirect)
       reference to the DLL closure in the table by first adding one to it.
       We check for this here, and undo the addition before evacuating it.

       If the SRT entry hasn't got bit 0 set, the SRT entry points to a
       closure that's fixed at link-time, and no extra magic is required.
    */
#ifdef ENABLE_WIN32_DLL_SUPPORT
    if ( (unsigned long)(*srt) & 0x1 ) {
       evacuate(*stgCast(StgClosure**,(stgCast(unsigned long, *srt) & ~0x1)));
    } else {
       evacuate(*srt);
    }
#else
       evacuate(*srt);
#endif
  }
}

/* -----------------------------------------------------------------------------
   Scavenge a TSO.
   -------------------------------------------------------------------------- */

static void
scavengeTSO (StgTSO *tso)
{
  // chase the link field for any TSOs on the same queue 
  (StgClosure *)tso->link = evacuate((StgClosure *)tso->link);
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
  // scavenge this thread's stack 
  scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
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

    info = get_itbl((StgClosure *)p);
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(info) || IS_HUGS_CONSTR_INFO(info)));
    
    q = p;
    switch (info->type) {
	
    case MVAR:
	/* treat MVars specially, because we don't want to evacuate the
	 * mut_link field in the middle of the closure.
	 */
    { 
	StgMVar *mvar = ((StgMVar *)p);
	evac_gen = 0;
	(StgClosure *)mvar->head = evacuate((StgClosure *)mvar->head);
	(StgClosure *)mvar->tail = evacuate((StgClosure *)mvar->tail);
	(StgClosure *)mvar->value = evacuate((StgClosure *)mvar->value);
	evac_gen = saved_evac_gen;
	recordMutable((StgMutClosure *)mvar);
	failed_to_evac = rtsFalse; // mutable.
	p += sizeofW(StgMVar);
	break;
    }

    case THUNK_2_0:
    case FUN_2_0:
	scavenge_srt(info);
    case CONSTR_2_0:
	((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case THUNK_1_0:
	scavenge_srt(info);
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2; // MIN_UPD_SIZE 
	break;
	
    case FUN_1_0:
	scavenge_srt(info);
    case CONSTR_1_0:
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 1;
	break;
	
    case THUNK_0_1:
	scavenge_srt(info);
	p += sizeofW(StgHeader) + 2; // MIN_UPD_SIZE 
	break;
	
    case FUN_0_1:
	scavenge_srt(info);
    case CONSTR_0_1:
	p += sizeofW(StgHeader) + 1;
	break;
	
    case THUNK_0_2:
    case FUN_0_2:
	scavenge_srt(info);
    case CONSTR_0_2:
	p += sizeofW(StgHeader) + 2;
	break;
	
    case THUNK_1_1:
    case FUN_1_1:
	scavenge_srt(info);
    case CONSTR_1_1:
	((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	p += sizeofW(StgHeader) + 2;
	break;
	
    case FUN:
    case THUNK:
	scavenge_srt(info);
	// fall through 
	
    case CONSTR:
    case WEAK:
    case FOREIGN:
    case STABLE_NAME:
    case BCO:
    {
	StgPtr end;

	end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
	    (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	p += info->layout.payload.nptrs;
	break;
    }

    case IND_PERM:
	if (stp->gen_no != 0) {
	    SET_INFO(((StgClosure *)p), &stg_IND_OLDGEN_PERM_info);
	}
	// fall through 
    case IND_OLDGEN_PERM:
	((StgIndOldGen *)p)->indirectee = 
	    evacuate(((StgIndOldGen *)p)->indirectee);
	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    recordOldToNewPtrs((StgMutClosure *)p);
	}
	p += sizeofW(StgIndOldGen);
	break;

    case MUT_VAR:
	evac_gen = 0;
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	evac_gen = saved_evac_gen;
	recordMutable((StgMutClosure *)p);
	failed_to_evac = rtsFalse; // mutable anyhow
	p += sizeofW(StgMutVar);
	break;

    case MUT_CONS:
	// ignore these
	failed_to_evac = rtsFalse; // mutable anyhow
	p += sizeofW(StgMutVar);
	break;

    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
    case SE_BLACKHOLE:
    case BLACKHOLE:
	p += BLACKHOLE_sizeW();
	break;

    case BLACKHOLE_BQ:
    { 
	StgBlockingQueue *bh = (StgBlockingQueue *)p;
	(StgClosure *)bh->blocking_queue = 
	    evacuate((StgClosure *)bh->blocking_queue);
	recordMutable((StgMutClosure *)bh);
	failed_to_evac = rtsFalse;
	p += BLACKHOLE_sizeW();
	break;
    }

    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	s->selectee = evacuate(s->selectee);
	p += THUNK_SELECTOR_sizeW();
	break;
    }

    case AP_UPD: // same as PAPs 
    case PAP:
	/* Treat a PAP just like a section of stack, not forgetting to
	 * evacuate the function pointer too...
	 */
    { 
	StgPAP* pap = (StgPAP *)p;

	pap->fun = evacuate(pap->fun);
	scavenge_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
	p += pap_sizeW(pap);
	break;
    }
      
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
	    (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	evac_gen = saved_evac_gen;
	recordMutable((StgMutClosure *)q);
	failed_to_evac = rtsFalse; // mutable anyhow.
	break;
    }

    case MUT_ARR_PTRS_FROZEN:
	// follow everything 
    {
	StgPtr next;

	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    (StgClosure *)*p = evacuate((StgClosure *)*p);
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
	recordMutable((StgMutClosure *)tso);
	failed_to_evac = rtsFalse; // mutable anyhow.
	p += tso_sizeW(tso);
	break;
    }

#if defined(PAR)
    case RBH: // cf. BLACKHOLE_BQ
    { 
#if 0
	nat size, ptrs, nonptrs, vhs;
	char str[80];
	StgInfoTable *rip = get_closure_info(p, &size, &ptrs, &nonptrs, &vhs, str);
#endif
	StgRBH *rbh = (StgRBH *)p;
	(StgClosure *)rbh->blocking_queue = 
	    evacuate((StgClosure *)rbh->blocking_queue);
	recordMutable((StgMutClosure *)to);
	failed_to_evac = rtsFalse;  // mutable anyhow.
	IF_DEBUG(gc,
		 belch("@@ scavenge: RBH %p (%s) (new blocking_queue link=%p)",
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
	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    recordMutable((StgMutClosure *)bf);
	}
	IF_DEBUG(gc,
		 belch("@@ scavenge: %p (%s); node is now %p; exciting, isn't it",
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

    case FETCH_ME_BQ: // cf. BLACKHOLE_BQ
    { 
	StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)p;
	(StgClosure *)fmbq->blocking_queue = 
	    evacuate((StgClosure *)fmbq->blocking_queue);
	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    recordMutable((StgMutClosure *)fmbq);
	}
	IF_DEBUG(gc,
		 belch("@@ scavenge: %p (%s) exciting, isn't it",
		       p, info_type((StgClosure *)p)));
	p += sizeofW(StgFetchMeBlockingQueue);
	break;
    }
#endif

    default:
	barf("scavenge: unimplemented/strange closure type %d @ %p", 
	     info->type, p);
    }

    /* If we didn't manage to promote all the objects pointed to by
     * the current object, then we have to designate this object as
     * mutable (because it contains old-to-new generation pointers).
     */
    if (failed_to_evac) {
	failed_to_evac = rtsFalse;
	mkMutCons((StgClosure *)q, &generations[evac_gen]);
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

	info = get_itbl((StgClosure *)p);
	ASSERT(p && (LOOKS_LIKE_GHC_INFO(info) || IS_HUGS_CONSTR_INFO(info)));
	
	q = p;
	switch (info->type) {
	    
	case MVAR:
	    /* treat MVars specially, because we don't want to evacuate the
	     * mut_link field in the middle of the closure.
	     */
	{
	    StgMVar *mvar = ((StgMVar *)p);
	    evac_gen = 0;
	    (StgClosure *)mvar->head = evacuate((StgClosure *)mvar->head);
	    (StgClosure *)mvar->tail = evacuate((StgClosure *)mvar->tail);
	    (StgClosure *)mvar->value = evacuate((StgClosure *)mvar->value);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsFalse; // mutable.
	    break;
	}

	case FUN_2_0:
	case THUNK_2_0:
	    scavenge_srt(info);
	case CONSTR_2_0:
	    ((StgClosure *)p)->payload[1] = evacuate(((StgClosure *)p)->payload[1]);
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;
	
	case FUN_1_0:
	case FUN_1_1:
	case THUNK_1_0:
	case THUNK_1_1:
	    scavenge_srt(info);
	case CONSTR_1_0:
	case CONSTR_1_1:
	    ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
	    break;
	
	case FUN_0_1:
	case FUN_0_2:
	case THUNK_0_1:
	case THUNK_0_2:
	    scavenge_srt(info);
	case CONSTR_0_1:
	case CONSTR_0_2:
	    break;
	
	case FUN:
	case THUNK:
	    scavenge_srt(info);
	    // fall through 
	
	case CONSTR:
	case WEAK:
	case FOREIGN:
	case STABLE_NAME:
	case BCO:
	{
	    StgPtr end;
	    
	    end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	    for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
		(StgClosure *)*p = evacuate((StgClosure *)*p);
	    }
	    break;
	}

	case IND_PERM:
	    // don't need to do anything here: the only possible case
	    // is that we're in a 1-space compacting collector, with
	    // no "old" generation.
	    break;

	case IND_OLDGEN:
	case IND_OLDGEN_PERM:
	    ((StgIndOldGen *)p)->indirectee = 
		evacuate(((StgIndOldGen *)p)->indirectee);
	    if (failed_to_evac) {
		recordOldToNewPtrs((StgMutClosure *)p);
	    }
	    failed_to_evac = rtsFalse;
	    break;

	case MUT_VAR:
	    evac_gen = 0;
	    ((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsFalse;
	    break;

	case MUT_CONS:
	    // ignore these
	    failed_to_evac = rtsFalse;
	    break;

	case CAF_BLACKHOLE:
	case SE_CAF_BLACKHOLE:
	case SE_BLACKHOLE:
	case BLACKHOLE:
	case ARR_WORDS:
	    break;

	case BLACKHOLE_BQ:
	{ 
	    StgBlockingQueue *bh = (StgBlockingQueue *)p;
	    (StgClosure *)bh->blocking_queue = 
		evacuate((StgClosure *)bh->blocking_queue);
	    failed_to_evac = rtsFalse;
	    break;
	}

	case THUNK_SELECTOR:
	{ 
	    StgSelector *s = (StgSelector *)p;
	    s->selectee = evacuate(s->selectee);
	    break;
	}

	case AP_UPD: // same as PAPs 
	case PAP:
	    /* Treat a PAP just like a section of stack, not forgetting to
	     * evacuate the function pointer too...
	     */
	{ 
	    StgPAP* pap = (StgPAP *)p;
	    
	    pap->fun = evacuate(pap->fun);
	    scavenge_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
	    break;
	}
      
	case MUT_ARR_PTRS:
	    // follow everything 
	{
	    StgPtr next;
	    
	    evac_gen = 0;		// repeatedly mutable 
	    next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	    for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		(StgClosure *)*p = evacuate((StgClosure *)*p);
	    }
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsFalse; // mutable anyhow.
	    break;
	}

	case MUT_ARR_PTRS_FROZEN:
	    // follow everything 
	{
	    StgPtr next;
	    
	    next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	    for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
		(StgClosure *)*p = evacuate((StgClosure *)*p);
	    }
	    break;
	}

	case TSO:
	{ 
	    StgTSO *tso = (StgTSO *)p;
	    evac_gen = 0;
	    scavengeTSO(tso);
	    evac_gen = saved_evac_gen;
	    failed_to_evac = rtsFalse;
	    break;
	}

#if defined(PAR)
	case RBH: // cf. BLACKHOLE_BQ
	{ 
#if 0
	    nat size, ptrs, nonptrs, vhs;
	    char str[80];
	    StgInfoTable *rip = get_closure_info(p, &size, &ptrs, &nonptrs, &vhs, str);
#endif
	    StgRBH *rbh = (StgRBH *)p;
	    (StgClosure *)rbh->blocking_queue = 
		evacuate((StgClosure *)rbh->blocking_queue);
	    recordMutable((StgMutClosure *)rbh);
	    failed_to_evac = rtsFalse;  // mutable anyhow.
	    IF_DEBUG(gc,
		     belch("@@ scavenge: RBH %p (%s) (new blocking_queue link=%p)",
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
	    if (failed_to_evac) {
		failed_to_evac = rtsFalse;
		recordMutable((StgMutClosure *)bf);
	    }
	    IF_DEBUG(gc,
		     belch("@@ scavenge: %p (%s); node is now %p; exciting, isn't it",
			   bf, info_type((StgClosure *)bf), 
			   bf->node, info_type(bf->node)));
	    break;
	}

#ifdef DIST
	case REMOTE_REF:
#endif
	case FETCH_ME:
	    break; // nothing to do in this case

	case FETCH_ME_BQ: // cf. BLACKHOLE_BQ
	{ 
	    StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)p;
	    (StgClosure *)fmbq->blocking_queue = 
		evacuate((StgClosure *)fmbq->blocking_queue);
	    if (failed_to_evac) {
		failed_to_evac = rtsFalse;
		recordMutable((StgMutClosure *)fmbq);
	    }
	    IF_DEBUG(gc,
		     belch("@@ scavenge: %p (%s) exciting, isn't it",
			   p, info_type((StgClosure *)p)));
	    break;
	}
#endif // PAR

	default:
	    barf("scavenge_mark_stack: unimplemented/strange closure type %d @ %p", 
		 info->type, p);
	}

	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    mkMutCons((StgClosure *)q, &generations[evac_gen]);
	}
	
	// mark the next bit to indicate "scavenged"
	mark(q+1, Bdescr(q));

    } // while (!mark_stack_empty())

    // start a new linear scan if the mark stack overflowed at some point
    if (mark_stack_overflowed && oldgen_scan_bd == NULL) {
	IF_DEBUG(gc, belch("scavenge_mark_stack: starting linear scan"));
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
    
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO((StgClosure *)p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO((StgClosure *)p))));
    
    info = get_itbl((StgClosure *)p);
    
    switch (info->type) {
	
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
    case IND_OLDGEN_PERM:
    {
	StgPtr q, end;
	
	end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (q = (StgPtr)((StgClosure *)p)->payload; q < end; q++) {
	    (StgClosure *)*q = evacuate((StgClosure *)*q);
	}
	break;
    }
    
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
    
    case ARR_WORDS:
	// nothing to follow 
	break;
      
    case MUT_ARR_PTRS:
    {
	// follow everything 
	StgPtr next;
      
	evac_gen = 0;		// repeatedly mutable 
	recordMutable((StgMutClosure *)p);
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsFalse;
	break;
    }

    case MUT_ARR_PTRS_FROZEN:
    {
	// follow everything 
	StgPtr next;
      
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	    (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	break;
    }

    case TSO:
    {
	StgTSO *tso = (StgTSO *)p;
      
	evac_gen = 0;		// repeatedly mutable 
	scavengeTSO(tso);
	recordMutable((StgMutClosure *)tso);
	evac_gen = saved_evac_gen;
	failed_to_evac = rtsFalse;
	break;
    }
  
    case AP_UPD:
    case PAP:
    { 
	StgPAP* pap = (StgPAP *)p;
	pap->fun = evacuate(pap->fun);
	scavenge_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
	break;
    }

    case IND_OLDGEN:
	// This might happen if for instance a MUT_CONS was pointing to a
	// THUNK which has since been updated.  The IND_OLDGEN will
	// be on the mutable list anyway, so we don't need to do anything
	// here.
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
scavenge_mut_once_list(generation *gen)
{
  const StgInfoTable *info;
  StgMutClosure *p, *next, *new_list;

  p = gen->mut_once_list;
  new_list = END_MUT_LIST;
  next = p->mut_link;

  evac_gen = gen->no;
  failed_to_evac = rtsFalse;

  for (; p != END_MUT_LIST; p = next, next = p->mut_link) {

    // make sure the info pointer is into text space 
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO(p))));
    
    info = get_itbl(p);
    /*
    if (info->type==RBH)
      info = REVERT_INFOPTR(info); // if it's an RBH, look at the orig closure
    */
    switch(info->type) {
      
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
      /* Try to pull the indirectee into this generation, so we can
       * remove the indirection from the mutable list.  
       */
      ((StgIndOldGen *)p)->indirectee = 
        evacuate(((StgIndOldGen *)p)->indirectee);
      
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
	belch("evac IND_OLDGEN: %ld bytes", size * sizeof(W_));
      }
#endif

      /* failed_to_evac might happen if we've got more than two
       * generations, we're collecting only generation 0, the
       * indirection resides in generation 2 and the indirectee is
       * in generation 1.
       */
      if (failed_to_evac) {
	failed_to_evac = rtsFalse;
	p->mut_link = new_list;
	new_list = p;
      } else {
	/* the mut_link field of an IND_STATIC is overloaded as the
	 * static link field too (it just so happens that we don't need
	 * both at the same time), so we need to NULL it out when
	 * removing this object from the mutable list because the static
	 * link fields are all assumed to be NULL before doing a major
	 * collection. 
	 */
	p->mut_link = NULL;
      }
      continue;

    case MUT_CONS:
	/* MUT_CONS is a kind of MUT_VAR, except it that we try to remove
	 * it from the mutable list if possible by promoting whatever it
	 * points to.
	 */
	if (scavenge_one((StgPtr)((StgMutVar *)p)->var)) {
	    /* didn't manage to promote everything, so put the
	     * MUT_CONS back on the list.
	     */
	    p->mut_link = new_list;
	    new_list = p;
	}
	continue;

    default:
      // shouldn't have anything else on the mutables list 
      barf("scavenge_mut_once_list: strange object? %d", (int)(info->type));
    }
  }

  gen->mut_once_list = new_list;
}


static void
scavenge_mutable_list(generation *gen)
{
  const StgInfoTable *info;
  StgMutClosure *p, *next;

  p = gen->saved_mut_list;
  next = p->mut_link;

  evac_gen = 0;
  failed_to_evac = rtsFalse;

  for (; p != END_MUT_LIST; p = next, next = p->mut_link) {

    // make sure the info pointer is into text space 
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO(p))));
    
    info = get_itbl(p);
    /*
    if (info->type==RBH)
      info = REVERT_INFOPTR(info); // if it's an RBH, look at the orig closure
    */
    switch(info->type) {
      
    case MUT_ARR_PTRS:
      // follow everything 
      p->mut_link = gen->mut_list;
      gen->mut_list = p;
      {
	StgPtr end, q;
	
	end = (P_)p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (q = (P_)((StgMutArrPtrs *)p)->payload; q < end; q++) {
	  (StgClosure *)*q = evacuate((StgClosure *)*q);
	}
	continue;
      }
      
      // Happens if a MUT_ARR_PTRS in the old generation is frozen
    case MUT_ARR_PTRS_FROZEN:
      {
	StgPtr end, q;
	
	evac_gen = gen->no;
	end = (P_)p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (q = (P_)((StgMutArrPtrs *)p)->payload; q < end; q++) {
	  (StgClosure *)*q = evacuate((StgClosure *)*q);
	}
	evac_gen = 0;
	p->mut_link = NULL;
	if (failed_to_evac) {
	    failed_to_evac = rtsFalse;
	    mkMutCons((StgClosure *)p, gen);
	}
	continue;
      }
	
    case MUT_VAR:
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	p->mut_link = gen->mut_list;
	gen->mut_list = p;
	continue;

    case MVAR:
      {
	StgMVar *mvar = (StgMVar *)p;
	(StgClosure *)mvar->head = evacuate((StgClosure *)mvar->head);
	(StgClosure *)mvar->tail = evacuate((StgClosure *)mvar->tail);
	(StgClosure *)mvar->value = evacuate((StgClosure *)mvar->value);
	p->mut_link = gen->mut_list;
	gen->mut_list = p;
	continue;
      }

    case TSO:
      { 
	StgTSO *tso = (StgTSO *)p;

	scavengeTSO(tso);

	/* Don't take this TSO off the mutable list - it might still
	 * point to some younger objects (because we set evac_gen to 0
	 * above). 
	 */
	tso->mut_link = gen->mut_list;
	gen->mut_list = (StgMutClosure *)tso;
	continue;
      }
      
    case BLACKHOLE_BQ:
      { 
	StgBlockingQueue *bh = (StgBlockingQueue *)p;
	(StgClosure *)bh->blocking_queue = 
	  evacuate((StgClosure *)bh->blocking_queue);
	p->mut_link = gen->mut_list;
	gen->mut_list = p;
	continue;
      }

      /* Happens if a BLACKHOLE_BQ in the old generation is updated: 
       */
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
      /* Try to pull the indirectee into this generation, so we can
       * remove the indirection from the mutable list.  
       */
      evac_gen = gen->no;
      ((StgIndOldGen *)p)->indirectee = 
        evacuate(((StgIndOldGen *)p)->indirectee);
      evac_gen = 0;

      if (failed_to_evac) {
	failed_to_evac = rtsFalse;
	p->mut_link = gen->mut_once_list;
	gen->mut_once_list = p;
      } else {
	p->mut_link = NULL;
      }
      continue;

#if defined(PAR)
    // HWL: check whether all of these are necessary

    case RBH: // cf. BLACKHOLE_BQ
      { 
	// nat size, ptrs, nonptrs, vhs;
	// char str[80];
	// StgInfoTable *rip = get_closure_info(p, &size, &ptrs, &nonptrs, &vhs, str);
	StgRBH *rbh = (StgRBH *)p;
	(StgClosure *)rbh->blocking_queue = 
	  evacuate((StgClosure *)rbh->blocking_queue);
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  recordMutable((StgMutClosure *)rbh);
	}
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
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  recordMutable((StgMutClosure *)bf);
	}
	p += sizeofW(StgBlockedFetch);
	break;
      }

#ifdef DIST
    case REMOTE_REF:
      barf("scavenge_mutable_list: REMOTE_REF %d", (int)(info->type));
#endif
    case FETCH_ME:
      p += sizeofW(StgFetchMe);
      break; // nothing to do in this case

    case FETCH_ME_BQ: // cf. BLACKHOLE_BQ
      { 
	StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)p;
	(StgClosure *)fmbq->blocking_queue = 
	  evacuate((StgClosure *)fmbq->blocking_queue);
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  recordMutable((StgMutClosure *)fmbq);
	}
	p += sizeofW(StgFetchMeBlockingQueue);
	break;
      }
#endif

    default:
      // shouldn't have anything else on the mutables list 
      barf("scavenge_mutable_list: strange object? %d", (int)(info->type));
    }
  }
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

    info = get_itbl(p);
    /*
    if (info->type==RBH)
      info = REVERT_INFOPTR(info); // if it's an RBH, look at the orig closure
    */
    // make sure the info pointer is into text space 
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO(p))));
    
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
	 * back on the mutable list (and take it off the
	 * scavenged_static list because the static link and mut link
	 * pointers are one and the same).
	 */
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  scavenged_static_objects = IND_STATIC_LINK(p);
	  ((StgMutClosure *)ind)->mut_link = oldest_gen->mut_once_list;
	  oldest_gen->mut_once_list = (StgMutClosure *)ind;
	}
	break;
      }
      
    case THUNK_STATIC:
    case FUN_STATIC:
      scavenge_srt(info);
      break;
      
    case CONSTR_STATIC:
      {	
	StgPtr q, next;
	
	next = (P_)p->payload + info->layout.payload.ptrs;
	// evacuate the pointers 
	for (q = (P_)p->payload; q < next; q++) {
	  (StgClosure *)*q = evacuate((StgClosure *)*q);
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
   scavenge_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   PAPs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */

static void
scavenge_stack(StgPtr p, StgPtr stack_end)
{
  StgPtr q;
  const StgInfoTable* info;
  StgWord bitmap;

  //IF_DEBUG(sanity, belch("  scavenging stack between %p and %p", p, stack_end));

  /* 
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with either a pending argument section or an 
   * activation record. 
   */

  while (p < stack_end) {
    q = *(P_ *)p;

    // If we've got a tag, skip over that many words on the stack 
    if (IS_ARG_TAG((W_)q)) {
      p += ARG_SIZE(q);
      p++; continue;
    }
     
    /* Is q a pointer to a closure?
     */
    if (! LOOKS_LIKE_GHC_INFO(q) ) {
#ifdef DEBUG
      if ( 0 && LOOKS_LIKE_STATIC_CLOSURE(q) ) {  // Is it a static closure? 
	ASSERT(closure_STATIC((StgClosure *)q));
      }
      // otherwise, must be a pointer into the allocation space. 
#endif

      (StgClosure *)*p = evacuate((StgClosure *)q);
      p++; 
      continue;
    }
      
    /* 
     * Otherwise, q must be the info pointer of an activation
     * record.  All activation records have 'bitmap' style layout
     * info.
     */
    info  = get_itbl((StgClosure *)p);
      
    switch (info->type) {
	
      // Dynamic bitmap: the mask is stored on the stack 
    case RET_DYN:
      bitmap = ((StgRetDyn *)p)->liveness;
      p      = (P_)&((StgRetDyn *)p)->payload[0];
      goto small_bitmap;

      // probably a slow-entry point return address: 
    case FUN:
    case FUN_STATIC:
      {
#if 0	
	StgPtr old_p = p;
	p++; p++; 
	IF_DEBUG(sanity, 
		 belch("HWL: scavenge_stack: FUN(_STATIC) adjusting p from %p to %p (instead of %p)",
		       old_p, p, old_p+1));
#else
      p++; // what if FHS!=1 !? -- HWL 
#endif
      goto follow_srt;
      }

      /* Specialised code for update frames, since they're so common.
       * We *know* the updatee points to a BLACKHOLE, CAF_BLACKHOLE,
       * or BLACKHOLE_BQ, so just inline the code to evacuate it here.  
       */
    case UPDATE_FRAME:
      {
	StgUpdateFrame *frame = (StgUpdateFrame *)p;

	p += sizeofW(StgUpdateFrame);

#ifndef not_yet
	frame->updatee = evacuate(frame->updatee);
	continue;
#else // specialised code for update frames, not sure if it's worth it.
	StgClosure *to;
	nat type = get_itbl(frame->updatee)->type;

	if (type == EVACUATED) {
	  frame->updatee = evacuate(frame->updatee);
	  continue;
	} else {
	  bdescr *bd = Bdescr((P_)frame->updatee);
	  step *stp;
	  if (bd->gen_no > N) { 
	    if (bd->gen_no < evac_gen) {
	      failed_to_evac = rtsTrue;
	    }
	    continue;
	  }

	  // Don't promote blackholes 
	  stp = bd->step;
	  if (!(stp->gen_no == 0 && 
		stp->no != 0 &&
		stp->no == stp->gen->n_steps-1)) {
	    stp = stp->to;
	  }

	  switch (type) {
	  case BLACKHOLE:
	  case CAF_BLACKHOLE:
	    to = copyPart(frame->updatee, BLACKHOLE_sizeW(), 
			  sizeofW(StgHeader), stp);
	    frame->updatee = to;
	    continue;
	  case BLACKHOLE_BQ:
	    to = copy(frame->updatee, BLACKHOLE_sizeW(), stp);
	    frame->updatee = to;
	    recordMutable((StgMutClosure *)to);
	    continue;
	  default:
            /* will never be SE_{,CAF_}BLACKHOLE, since we
               don't push an update frame for single-entry thunks.  KSW 1999-01. */
	    barf("scavenge_stack: UPDATE_FRAME updatee");
	  }
	}
#endif
      }

      // small bitmap (< 32 entries, or 64 on a 64-bit machine) 
    case STOP_FRAME:
    case CATCH_FRAME:
    case SEQ_FRAME:
    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
      bitmap = info->layout.bitmap;
      p++;
      // this assumes that the payload starts immediately after the info-ptr 
    small_bitmap:
      while (bitmap != 0) {
	if ((bitmap & 1) == 0) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	p++;
	bitmap = bitmap >> 1;
      }
      
    follow_srt:
      scavenge_srt(info);
      continue;

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine) 
    case RET_BIG:
    case RET_VEC_BIG:
      {
	StgPtr q;
	StgLargeBitmap *large_bitmap;
	nat i;

	large_bitmap = info->layout.large_bitmap;
	p++;

	for (i=0; i<large_bitmap->size; i++) {
	  bitmap = large_bitmap->bitmap[i];
	  q = p + BITS_IN(W_);
	  while (bitmap != 0) {
	    if ((bitmap & 1) == 0) {
	      (StgClosure *)*p = evacuate((StgClosure *)*p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	  }
	  if (i+1 < large_bitmap->size) {
	    while (p < q) {
	      (StgClosure *)*p = evacuate((StgClosure *)*p);
	      p++;
	    }
	  }
	}

	// and don't forget to follow the SRT 
	goto follow_srt;
      }

    default:
      barf("scavenge_stack: weird activation record found on stack: %d", (int)(info->type));
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
	mkMutCons((StgClosure *)p, stp->gen);
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

/* This function is only needed because we share the mutable link
 * field with the static link field in an IND_STATIC, so we have to
 * zero the mut_link field before doing a major GC, which needs the
 * static link field.  
 *
 * It doesn't do any harm to zero all the mutable link fields on the
 * mutable list.
 */

static void
zero_mutable_list( StgMutClosure *first )
{
  StgMutClosure *next, *c;

  for (c = first; c != END_MUT_LIST; c = next) {
    next = c->mut_link;
    c->mut_link = NULL;
  }
}

/* -----------------------------------------------------------------------------
   Reverting CAFs
   -------------------------------------------------------------------------- */

void
revertCAFs( void )
{
    StgIndStatic *c;

    for (c = (StgIndStatic *)caf_list; c != NULL; 
	 c = (StgIndStatic *)c->static_link) 
    {
	c->header.info = c->saved_info;
	c->saved_info = NULL;
	// could, but not necessary: c->static_link = NULL; 
    }
    caf_list = NULL;
}

void
scavengeCAFs( void )
{
    StgIndStatic *c;

    evac_gen = 0;
    for (c = (StgIndStatic *)caf_list; c != NULL; 
	 c = (StgIndStatic *)c->static_link) 
    {
	c->indirectee = evacuate(c->indirectee);
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
      IF_DEBUG(gccafs, belch("CAF gc'd at 0x%04lx", (long)p));
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

  //  belch("%d CAFs live", i); 
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
  StgUpdateFrame *update_frame;
  StgBlockingQueue *bh;
  StgPtr stack_end;

  stack_end = &tso->stack[tso->stack_size];
  update_frame = tso->su;

  while (1) {
    switch (get_itbl(update_frame)->type) {

    case CATCH_FRAME:
      update_frame = ((StgCatchFrame *)update_frame)->link;
      break;

    case UPDATE_FRAME:
      bh = (StgBlockingQueue *)update_frame->updatee;

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

      if (bh->header.info != &stg_BLACKHOLE_BQ_info &&
	  bh->header.info != &stg_CAF_BLACKHOLE_info) {
#if (!defined(LAZY_BLACKHOLING)) && defined(DEBUG)
        belch("Unexpected lazy BHing required at 0x%04x",(int)bh);
#endif
	SET_INFO(bh,&stg_BLACKHOLE_info);
      }

      update_frame = update_frame->link;
      break;

    case SEQ_FRAME:
      update_frame = ((StgSeqFrame *)update_frame)->link;
      break;

    case STOP_FRAME:
      return;
    default:
      barf("threadPaused");
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

static void
threadSqueezeStack(StgTSO *tso)
{
  lnat displacement = 0;
  StgUpdateFrame *frame;
  StgUpdateFrame *next_frame;		        // Temporally next 
  StgUpdateFrame *prev_frame;			// Temporally previous 
  StgPtr bottom;
  rtsBool prev_was_update_frame;
#if DEBUG
  StgUpdateFrame *top_frame;
  nat upd_frames=0, stop_frames=0, catch_frames=0, seq_frames=0,
      bhs=0, squeezes=0;
  void printObj( StgClosure *obj ); // from Printer.c

  top_frame  = tso->su;
#endif
  
  bottom = &(tso->stack[tso->stack_size]);
  frame  = tso->su;

  /* There must be at least one frame, namely the STOP_FRAME.
   */
  ASSERT((P_)frame < bottom);

  /* Walk down the stack, reversing the links between frames so that
   * we can walk back up as we squeeze from the bottom.  Note that
   * next_frame and prev_frame refer to next and previous as they were
   * added to the stack, rather than the way we see them in this
   * walk. (It makes the next loop less confusing.)  
   *
   * Stop if we find an update frame pointing to a black hole 
   * (see comment in threadLazyBlackHole()).
   */
  
  next_frame = NULL;
  // bottom - sizeof(StgStopFrame) is the STOP_FRAME 
  while ((P_)frame < bottom - sizeofW(StgStopFrame)) {  
    prev_frame = frame->link;
    frame->link = next_frame;
    next_frame = frame;
    frame = prev_frame;
#if DEBUG
    IF_DEBUG(sanity,
	     if (!(frame>=top_frame && frame<=(StgUpdateFrame *)bottom)) {
	       printObj((StgClosure *)prev_frame);
	       barf("threadSqueezeStack: current frame is rubbish %p; previous was %p\n", 
		    frame, prev_frame);
	     })
    switch (get_itbl(frame)->type) {
    case UPDATE_FRAME:
	upd_frames++;
	if (frame->updatee->header.info == &stg_BLACKHOLE_info)
	    bhs++;
	break;
    case STOP_FRAME:
	stop_frames++;
	break;
    case CATCH_FRAME:
	catch_frames++;
	break;
    case SEQ_FRAME:
	seq_frames++;
	break;
    default:
      barf("Found non-frame during stack squeezing at %p (prev frame was %p)\n",
	   frame, prev_frame);
      printObj((StgClosure *)prev_frame);
    }
#endif
    if (get_itbl(frame)->type == UPDATE_FRAME
	&& frame->updatee->header.info == &stg_BLACKHOLE_info) {
        break;
    }
  }

  /* Now, we're at the bottom.  Frame points to the lowest update
   * frame on the stack, and its link actually points to the frame
   * above. We have to walk back up the stack, squeezing out empty
   * update frames and turning the pointers back around on the way
   * back up.
   *
   * The bottom-most frame (the STOP_FRAME) has not been altered, and
   * we never want to eliminate it anyway.  Just walk one step up
   * before starting to squeeze. When you get to the topmost frame,
   * remember that there are still some words above it that might have
   * to be moved.  
   */
  
  prev_frame = frame;
  frame = next_frame;

  prev_was_update_frame = (get_itbl(prev_frame)->type == UPDATE_FRAME);

  /*
   * Loop through all of the frames (everything except the very
   * bottom).  Things are complicated by the fact that we have 
   * CATCH_FRAMEs and SEQ_FRAMEs interspersed with the update frames.
   * We can only squeeze when there are two consecutive UPDATE_FRAMEs.
   */
  while (frame != NULL) {
    StgPtr sp;
    StgPtr frame_bottom = (P_)frame + sizeofW(StgUpdateFrame);
    rtsBool is_update_frame;
    
    next_frame = frame->link;
    is_update_frame = (get_itbl(frame)->type == UPDATE_FRAME);

    /* Check to see if 
     *   1. both the previous and current frame are update frames
     *   2. the current frame is empty
     */
    if (prev_was_update_frame && is_update_frame &&
	(P_)prev_frame == frame_bottom + displacement) {
      
      // Now squeeze out the current frame 
      StgClosure *updatee_keep   = prev_frame->updatee;
      StgClosure *updatee_bypass = frame->updatee;
      
#if DEBUG
      IF_DEBUG(gc, belch("@@ squeezing frame at %p", frame));
      squeezes++;
#endif

      /* Deal with blocking queues.  If both updatees have blocked
       * threads, then we should merge the queues into the update
       * frame that we're keeping.
       *
       * Alternatively, we could just wake them up: they'll just go
       * straight to sleep on the proper blackhole!  This is less code
       * and probably less bug prone, although it's probably much
       * slower --SDM
       */
#if 0 // do it properly... 
#  if (!defined(LAZY_BLACKHOLING)) && defined(DEBUG)
#    error Unimplemented lazy BH warning.  (KSW 1999-01)
#  endif
      if (GET_INFO(updatee_bypass) == stg_BLACKHOLE_BQ_info
	  || GET_INFO(updatee_bypass) == stg_CAF_BLACKHOLE_info
	  ) {
	// Sigh.  It has one.  Don't lose those threads! 
	  if (GET_INFO(updatee_keep) == stg_BLACKHOLE_BQ_info) {
	  // Urgh.  Two queues.  Merge them. 
	  P_ keep_tso = ((StgBlockingQueue *)updatee_keep)->blocking_queue;
	  
	  while (keep_tso->link != END_TSO_QUEUE) {
	    keep_tso = keep_tso->link;
	  }
	  keep_tso->link = ((StgBlockingQueue *)updatee_bypass)->blocking_queue;

	} else {
	  // For simplicity, just swap the BQ for the BH 
	  P_ temp = updatee_keep;
	  
	  updatee_keep = updatee_bypass;
	  updatee_bypass = temp;
	  
	  // Record the swap in the kept frame (below) 
	  prev_frame->updatee = updatee_keep;
	}
      }
#endif

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
      if (updatee_bypass != updatee_keep && !closure_IND(updatee_bypass)) {
	  // this wakes the threads up 
	  UPD_IND_NOLOCK(updatee_bypass, updatee_keep);
      }
      
      sp = (P_)frame - 1;	// sp = stuff to slide 
      displacement += sizeofW(StgUpdateFrame);
      
    } else {
      // No squeeze for this frame 
      sp = frame_bottom - 1;	// Keep the current frame 
      
      /* Do lazy black-holing.
       */
      if (is_update_frame) {
	StgBlockingQueue *bh = (StgBlockingQueue *)frame->updatee;
	if (bh->header.info != &stg_BLACKHOLE_info &&
	    bh->header.info != &stg_BLACKHOLE_BQ_info &&
	    bh->header.info != &stg_CAF_BLACKHOLE_info) {
#if (!defined(LAZY_BLACKHOLING)) && defined(DEBUG)
          belch("Unexpected lazy BHing required at 0x%04x",(int)bh);
#endif
#ifdef DEBUG
	  /* zero out the slop so that the sanity checker can tell
	   * where the next closure is.
	   */
	  { 
	      StgInfoTable *info = get_itbl(bh);
	      nat np = info->layout.payload.ptrs, nw = info->layout.payload.nptrs, i;
	      /* don't zero out slop for a THUNK_SELECTOR, because its layout
	       * info is used for a different purpose, and it's exactly the
	       * same size as a BLACKHOLE in any case.
	       */
	      if (info->type != THUNK_SELECTOR) {
		for (i = np; i < np + nw; i++) {
		  ((StgClosure *)bh)->payload[i] = 0;
		}
	      }
	  }
#endif
	  SET_INFO(bh,&stg_BLACKHOLE_info);
	}
      }

      // Fix the link in the current frame (should point to the frame below) 
      frame->link = prev_frame;
      prev_was_update_frame = is_update_frame;
    }
    
    // Now slide all words from sp up to the next frame 
    
    if (displacement > 0) {
      P_ next_frame_bottom;

      if (next_frame != NULL)
	next_frame_bottom = (P_)next_frame + sizeofW(StgUpdateFrame);
      else
	next_frame_bottom = tso->sp - 1;
      
#if 0
      IF_DEBUG(gc,
	       belch("sliding [%p, %p] by %ld", sp, next_frame_bottom,
		     displacement))
#endif
      
      while (sp >= next_frame_bottom) {
	sp[displacement] = *sp;
	sp -= 1;
      }
    }
    (P_)prev_frame = (P_)frame + displacement;
    frame = next_frame;
  }

  tso->sp += displacement;
  tso->su = prev_frame;
#if 0
  IF_DEBUG(gc,
	   belch("@@ threadSqueezeStack: squeezed %d update-frames; found %d BHs; found %d update-, %d stop-, %d catch, %d seq-frames",
		   squeezes, bhs, upd_frames, stop_frames, catch_frames, seq_frames))
#endif
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
printMutOnceList(generation *gen)
{
  StgMutClosure *p, *next;

  p = gen->mut_once_list;
  next = p->mut_link;

  fprintf(stderr, "@@ Mut once list %p: ", gen->mut_once_list);
  for (; p != END_MUT_LIST; p = next, next = p->mut_link) {
    fprintf(stderr, "%p (%s), ", 
	    p, info_type((StgClosure *)p));
  }
  fputc('\n', stderr);
}

void
printMutableList(generation *gen)
{
  StgMutClosure *p, *next;

  p = gen->mut_list;
  next = p->mut_link;

  fprintf(stderr, "@@ Mutable list %p: ", gen->mut_list);
  for (; p != END_MUT_LIST; p = next, next = p->mut_link) {
    fprintf(stderr, "%p (%s), ",
	    p, info_type((StgClosure *)p));
  }
  fputc('\n', stderr);
}

static inline rtsBool
maybeLarge(StgClosure *closure)
{
  StgInfoTable *info = get_itbl(closure);

  /* closure types that may be found on the new_large_objects list; 
     see scavenge_large */
  return (info->type == MUT_ARR_PTRS ||
	  info->type == MUT_ARR_PTRS_FROZEN ||
	  info->type == TSO ||
	  info->type == ARR_WORDS);
}

  
#endif // DEBUG
