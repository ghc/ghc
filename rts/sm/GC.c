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
#include "Stable.h"
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
#include "Trace.h"
#include "RetainerProfile.h"
#include "RaiseAsync.h"

#include "GC.h"
#include "Compact.h"
#include "Evac.h"
#include "Scav.h"
#include "GCUtils.h"
#include "MarkWeak.h"

#include <string.h> // for memset()

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

/* N is the oldest generation being collected, where the generations
 * are numbered starting at 0.  A major GC (indicated by the major_gc
 * flag) is when we're collecting all generations.  We only attempt to
 * deal with static objects and GC CAFs when doing a major GC.
 */
nat N;
rtsBool major_gc;

/* Youngest generation that objects should be evacuated to in
 * evacuate().  (Logically an argument to evacuate, but it's static
 * a lot of the time so we optimise it into a global variable).
 */
nat evac_gen;

/* Whether to do eager promotion or not.
 */
rtsBool eager_promotion;

/* Flag indicating failure to evacuate an object to the desired
 * generation.
 */
rtsBool failed_to_evac;

/* Saved nursery (used for 2-space collector only)
 */
static bdescr *saved_nursery;
static nat saved_n_blocks;
  
/* Data used for allocation area sizing.
 */
lnat new_blocks;		 // blocks allocated during this GC 
lnat new_scavd_blocks;	 // ditto, but depth-first blocks
static lnat g0s0_pcnt_kept = 30; // percentage of g0s0 live at last minor GC 

/* Mut-list stats */
#ifdef DEBUG
nat mutlist_MUTVARS,
    mutlist_MUTARRS,
    mutlist_OTHERS;
#endif

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static void         mark_root               ( StgClosure **root );

static void         zero_static_object_list ( StgClosure* first_static );

#if 0 && defined(DEBUG)
static void         gcCAFs                  ( void );
#endif

/* -----------------------------------------------------------------------------
   inline functions etc. for dealing with the mark bitmap & stack.
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

   Locks held: all capabilities are held throughout GarbageCollect().

   -------------------------------------------------------------------------- */

void
GarbageCollect ( rtsBool force_major_gc )
{
  bdescr *bd;
  step *stp;
  lnat live, allocated, copied = 0, scavd_copied = 0;
  lnat oldgen_saved_blocks = 0;
  nat g, s, i;

  ACQUIRE_SM_LOCK;

#ifdef PROFILING
  CostCentreStack *prev_CCS;
#endif

  debugTrace(DEBUG_gc, "starting GC");

#if defined(RTS_USER_SIGNALS)
  // block signals
  blockUserSignals();
#endif

  // tell the STM to discard any cached closures its hoping to re-use
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

  /* Save the nursery if we're doing a two-space collection.
   * g0s0->blocks will be used for to-space, so we need to get the
   * nursery out of the way.
   */
  if (RtsFlags.GcFlags.generations == 1) {
      saved_nursery = g0s0->blocks;
      saved_n_blocks = g0s0->n_blocks;
      g0s0->blocks = NULL;
      g0s0->n_blocks = 0;
  }

  /* Keep a count of how many new blocks we allocated during this GC
   * (used for resizing the allocation area, later).
   */
  new_blocks = 0;
  new_scavd_blocks = 0;

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

      // start a new to-space for this step.
      stp->old_blocks   = stp->blocks;
      stp->n_old_blocks = stp->n_blocks;

      // allocate the first to-space block; extra blocks will be
      // chained on as necessary.
      stp->hp_bd     = NULL;
      bd = gc_alloc_block(stp);
      stp->blocks      = bd;
      stp->n_blocks    = 1;
      stp->scan        = bd->start;
      stp->scan_bd     = bd;

      // allocate a block for "already scavenged" objects.  This goes
      // on the front of the stp->blocks list, so it won't be
      // traversed by the scavenging sweep.
      gc_alloc_scavd_block(stp);

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
      if (stp->scavd_hp == NULL) {
	  gc_alloc_scavd_block(stp);
	  stp->n_blocks++;
      }
      /* Set the scan pointer for older generations: remember we
       * still have to scavenge objects that have been promoted. */
      stp->scan = stp->hp;
      stp->scan_bd = stp->hp_bd;
      stp->new_large_objects = NULL;
      stp->scavenged_large_objects = NULL;
      stp->n_scavenged_large_blocks = 0;
    }

    /* Move the private mutable lists from each capability onto the
     * main mutable list for the generation.
     */
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

  eager_promotion = rtsTrue; // for now

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
  GetRoots(mark_root);

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
  markWeakPtrList();
  initWeakForGC();

  /* Mark the stable pointer table.
   */
  markStablePtrTable(mark_root);

  /* Mark the root pointer table.
   */
  markRootPtrTable(mark_root);

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

  /* Update the pointers from the task list - these are
   * treated as weak pointers because we want to allow a main thread
   * to get a BlockedOnDeadMVar exception in the same way as any other
   * thread.  Note that the threads should all have been retained by
   * GC by virtue of being on the all_threads list, we're just
   * updating pointers here.
   */
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
	      Bdescr(stp->scavd_hp)->free = stp->scavd_hp;
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
      oldgen_saved_blocks = oldest_gen->steps[0].n_old_blocks;
      compact();
  }

  IF_DEBUG(sanity, checkGlobalTSOList(rtsFalse));

  /* run through all the generations/steps and tidy up 
   */
  copied = new_blocks * BLOCK_SIZE_W;
  scavd_copied =  new_scavd_blocks * BLOCK_SIZE_W;
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
		   "mut_list_size: %lu (%d vars, %d arrays, %d others)",
		   (unsigned long)(mut_list_size * sizeof(W_)),
		   mutlist_MUTVARS, mutlist_MUTARRS, mutlist_OTHERS);
    }

    for (s = 0; s < generations[g].n_steps; s++) {
      bdescr *next;
      stp = &generations[g].steps[s];

      if (!(g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1)) {
	// stats information: how much we copied 
	if (g <= N) {
	  copied -= stp->hp_bd->start + BLOCK_SIZE_W -
	    stp->hp_bd->free;
	  scavd_copied -= (P_)(BLOCK_ROUND_UP(stp->scavd_hp)) - stp->scavd_hp;
	}
      }

      // for generations we collected... 
      if (g <= N) {

	/* free old memory and shift to-space into from-space for all
	 * the collected steps (except the allocation area).  These
	 * freed blocks will probaby be quickly recycled.
	 */
	if (!(g == 0 && s == 0)) {
	    if (stp->is_compacted) {
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
	    } else {
		freeChain(stp->old_blocks);
		for (bd = stp->blocks; bd != NULL; bd = bd->link) {
		    bd->flags &= ~BF_EVACUATED;	 // now from-space 
		}
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
	  if (stp->bitmap != NULL) {
	      freeGroup(stp->bitmap);
	      stp->bitmap = NULL;
	  }
      }
  }

  /* Two-space collector:
   * Free the old to-space, and estimate the amount of live data.
   */
  if (RtsFlags.GcFlags.generations == 1) {
    nat blocks;
    
    if (g0s0->old_blocks != NULL) {
      freeChain(g0s0->old_blocks);
    }
    for (bd = g0s0->blocks; bd != NULL; bd = bd->link) {
      bd->flags = 0;	// now from-space 
    }
    g0s0->old_blocks = g0s0->blocks;
    g0s0->n_old_blocks = g0s0->n_blocks;
    g0s0->blocks = saved_nursery;
    g0s0->n_blocks = saved_n_blocks;

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
    blocks = g0s0->n_old_blocks;

    if ( RtsFlags.GcFlags.maxHeapSize != 0 &&
	 blocks * RtsFlags.GcFlags.oldGenFactor * 2 > 
	   RtsFlags.GcFlags.maxHeapSize ) {
      long adjusted_blocks;  // signed on purpose 
      int pc_free; 
      
      adjusted_blocks = (RtsFlags.GcFlags.maxHeapSize - 2 * blocks);

      debugTrace(DEBUG_gc, "near maximum heap size of 0x%x blocks, blocks = %d, adjusted to %ld", 
		 RtsFlags.GcFlags.maxHeapSize, blocks, adjusted_blocks);

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
      resizeNurseriesFixed(RtsFlags.GcFlags.minAllocAreaSize);
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
  // unblock signals again
  unblockUserSignals();
#endif

  RELEASE_SM_LOCK;

  //PAR_TICKY_TP();
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

/* -----------------------------------------------------------------------------
 * Debugging
 * -------------------------------------------------------------------------- */

#if DEBUG
void
printMutableList(generation *gen)
{
    bdescr *bd;
    StgPtr p;

    debugBelch("mutable list %p: ", gen->mut_list);

    for (bd = gen->mut_list; bd != NULL; bd = bd->link) {
	for (p = bd->start; p < bd->free; p++) {
	    debugBelch("%p (%s), ", (void *)*p, info_type((StgClosure *)*p));
	}
    }
    debugBelch("\n");
}
#endif /* DEBUG */
