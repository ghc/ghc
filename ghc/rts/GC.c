/* -----------------------------------------------------------------------------
 * $Id: GC.c,v 1.27 1999/02/05 16:02:41 simonm Exp $
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
#include "SchedAPI.h" /* for ReverCAFs prototype */
#include "Sanity.h"
#include "GC.h"
#include "BlockAlloc.h"
#include "Main.h"
#include "DebugProf.h"
#include "SchedAPI.h"
#include "Weak.h"
#include "StablePriv.h"

StgCAF* enteredCAFs;

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
StgClosure* static_objects;	      /* live static objects */
StgClosure* scavenged_static_objects; /* static objects scavenged so far */

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

/* WEAK POINTERS
 */
static StgWeak *old_weak_ptr_list; /* also pending finaliser list */
static rtsBool weak_done;	/* all done for this pass */

/* Flag indicating failure to evacuate an object to the desired
 * generation.
 */
static rtsBool failed_to_evac;

/* Old to-space (used for two-space collector only)
 */
bdescr *old_to_space;

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static StgClosure *evacuate(StgClosure *q);
static void    zeroStaticObjectList(StgClosure* first_static);
static rtsBool traverse_weak_ptr_list(void);
static void    zeroMutableList(StgMutClosure *first);
static void    revertDeadCAFs(void);

static void           scavenge_stack(StgPtr p, StgPtr stack_end);
static void           scavenge_large(step *step);
static void           scavenge(step *step);
static void           scavenge_static(void);
static void           scavenge_mutable_list(generation *g);
static void           scavenge_mut_once_list(generation *g);

#ifdef DEBUG
static void gcCAFs(void);
#endif

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

void GarbageCollect(void (*get_roots)(void))
{
  bdescr *bd;
  step *step;
  lnat live, allocated, collected = 0;
  nat g, s;

#ifdef PROFILING
  CostCentreStack *prev_CCS;
#endif

  /* tell the stats department that we've started a GC */
  stat_startGC();

  /* attribute any costs to CCS_GC */
#ifdef PROFILING
  prev_CCS = CCCS;
  CCCS = CCS_GC;
#endif

  /* We might have been called from Haskell land by _ccall_GC, in
   * which case we need to call threadPaused() because the scheduler
   * won't have done it.
   */
  if (CurrentTSO) { threadPaused(CurrentTSO); }

  /* Approximate how much we allocated: number of blocks in the
   * nursery + blocks allocated via allocate() - unused nusery blocks.
   * This leaves a little slop at the end of each block, and doesn't
   * take into account large objects (ToDo).
   */
  allocated = (nursery_blocks * BLOCK_SIZE_W) + allocated_bytes();
  for ( bd = current_nursery->link; bd != NULL; bd = bd->link ) {
    allocated -= BLOCK_SIZE_W;
  }

  /* Figure out which generation to collect
   */
  N = 0;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    if (generations[g].steps[0].n_blocks >= generations[g].max_blocks) {
      N = g;
    }
  }
  major_gc = (N == RtsFlags.GcFlags.generations-1);

  /* check stack sanity *before* GC (ToDo: check all threads) */
  /*IF_DEBUG(sanity, checkTSO(MainTSO,0)); */
  IF_DEBUG(sanity, checkFreeListSanity());

  /* Initialise the static object lists
   */
  static_objects = END_OF_STATIC_LIST;
  scavenged_static_objects = END_OF_STATIC_LIST;

  /* zero the mutable list for the oldest generation (see comment by
   * zeroMutableList below).
   */
  if (major_gc) { 
    zeroMutableList(generations[RtsFlags.GcFlags.generations-1].mut_once_list);
  }

  /* Save the old to-space if we're doing a two-space collection
   */
  if (RtsFlags.GcFlags.generations == 1) {
    old_to_space = g0s0->to_space;
    g0s0->to_space = NULL;
  }

  /* Initialise to-space in all the generations/steps that we're
   * collecting.
   */
  for (g = 0; g <= N; g++) {
    generations[g].mut_once_list = END_MUT_LIST;
    generations[g].mut_list = END_MUT_LIST;

    for (s = 0; s < generations[g].n_steps; s++) {

      /* generation 0, step 0 doesn't need to-space */
      if (g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1) { 
	continue; 
      }

      /* Get a free block for to-space.  Extra blocks will be chained on
       * as necessary.
       */
      bd = allocBlock();
      step = &generations[g].steps[s];
      ASSERT(step->gen->no == g);
      ASSERT(step->hp ? Bdescr(step->hp)->step == step : rtsTrue);
      bd->gen  = &generations[g];
      bd->step = step;
      bd->link = NULL;
      bd->evacuated = 1;	/* it's a to-space block */
      step->hp        = bd->start;
      step->hpLim     = step->hp + BLOCK_SIZE_W;
      step->hp_bd     = bd;
      step->to_space  = bd;
      step->to_blocks = 1; /* ???? */
      step->scan      = bd->start;
      step->scan_bd   = bd;
      step->new_large_objects = NULL;
      step->scavenged_large_objects = NULL;
      /* mark the large objects as not evacuated yet */
      for (bd = step->large_objects; bd; bd = bd->link) {
	bd->evacuated = 0;
      }
    }
  }

  /* make sure the older generations have at least one block to
   * allocate into (this makes things easier for copy(), see below.
   */
  for (g = N+1; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      step = &generations[g].steps[s];
      if (step->hp_bd == NULL) {
	bd = allocBlock();
	bd->gen = &generations[g];
	bd->step = step;
	bd->link = NULL;
	bd->evacuated = 0;	/* *not* a to-space block */
	step->hp = bd->start;
	step->hpLim = step->hp + BLOCK_SIZE_W;
	step->hp_bd = bd;
	step->blocks = bd;
	step->n_blocks = 1;
      }
      /* Set the scan pointer for older generations: remember we
       * still have to scavenge objects that have been promoted. */
      step->scan = step->hp;
      step->scan_bd = step->hp_bd;
      step->to_space = NULL;
      step->to_blocks = 0;
      step->new_large_objects = NULL;
      step->scavenged_large_objects = NULL;
    }
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

    /* Do the mut-once lists first */
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      scavenge_mut_once_list(&generations[g]);
      evac_gen = g;
      for (st = generations[g].n_steps-1; st >= 0; st--) {
	scavenge(&generations[g].steps[st]);
      }
    }

    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
      scavenge_mutable_list(&generations[g]);
      evac_gen = g;
      for (st = generations[g].n_steps-1; st >= 0; st--) {
	scavenge(&generations[g].steps[st]);
      }
    }
  }

  /* follow all the roots that the application knows about.
   */
  evac_gen = 0;
  get_roots();

  /* And don't forget to mark the TSO if we got here direct from
   * Haskell! */
  if (CurrentTSO) {
    CurrentTSO = (StgTSO *)MarkRoot((StgClosure *)CurrentTSO);
  }

  /* Mark the weak pointer list, and prepare to detect dead weak
   * pointers.
   */
  markWeakList();
  old_weak_ptr_list = weak_ptr_list;
  weak_ptr_list = NULL;
  weak_done = rtsFalse;

  /* Mark the stable pointer table.
   */
  markStablePtrTable(major_gc);

#ifdef INTERPRETER
  { 
      /* ToDo: To fix the caf leak, we need to make the commented out
       * parts of this code do something sensible - as described in 
       * the CAF document.
       */
      extern void markHugsObjects(void);
#if 0
      /* ToDo: This (undefined) function should contain the scavenge
       * loop immediately below this block of code - but I'm not sure
       * enough of the details to do this myself.
       */
      scavengeEverything();
      /* revert dead CAFs and update enteredCAFs list */
      revertDeadCAFs();
#endif      
      markHugsObjects();
#if 0
      /* This will keep the CAFs and the attached BCOs alive 
       * but the values will have been reverted
       */
      scavengeEverything();
#endif
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

    /* scavenge static objects */
    if (major_gc && static_objects != END_OF_STATIC_LIST) {
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

    /* scavenge each step in generations 0..maxgen */
    { 
      int gen, st; 
    loop2:
      for (gen = RtsFlags.GcFlags.generations-1; gen >= 0; gen--) {
	for (st = generations[gen].n_steps-1; st >= 0 ; st--) {
	  step = &generations[gen].steps[st];
	  evac_gen = gen;
	  if (step->hp_bd != step->scan_bd || step->scan < step->hp) {
	    scavenge(step);
	    flag = rtsTrue;
	    goto loop2;
	  }
	  if (step->new_large_objects != NULL) {
	    scavenge_large(step);
	    flag = rtsTrue;
	    goto loop2;
	  }
	}
      }
    }
    if (flag) { goto loop; }

    /* must be last... */
    if (traverse_weak_ptr_list()) { /* returns rtsTrue if evaced something */
      goto loop;
    }
  }

  /* Now see which stable names are still alive
   */
  gcStablePtrTable(major_gc);

  /* Set the maximum blocks for the oldest generation, based on twice
   * the amount of live data now, adjusted to fit the maximum heap
   * size if necessary.  
   *
   * This is an approximation, since in the worst case we'll need
   * twice the amount of live data plus whatever space the other
   * generations need.
   */
  if (RtsFlags.GcFlags.generations > 1) {
    if (major_gc) {
      oldest_gen->max_blocks = 
	stg_max(oldest_gen->steps[0].to_blocks * RtsFlags.GcFlags.oldGenFactor,
		RtsFlags.GcFlags.minOldGenSize);
      if (oldest_gen->max_blocks > RtsFlags.GcFlags.maxHeapSize / 2) {
	oldest_gen->max_blocks = RtsFlags.GcFlags.maxHeapSize / 2;
	if (((int)oldest_gen->max_blocks - 
	     (int)oldest_gen->steps[0].to_blocks) < 
	    (RtsFlags.GcFlags.pcFreeHeap *
	     RtsFlags.GcFlags.maxHeapSize / 200)) {
	  heapOverflow();
	}
      }
    }
  }

  /* run through all the generations/steps and tidy up 
   */
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {

    if (g <= N) {
      generations[g].collections++; /* for stats */
    }

    for (s = 0; s < generations[g].n_steps; s++) {
      bdescr *next;
      step = &generations[g].steps[s];

      if (!(g == 0 && s == 0 && RtsFlags.GcFlags.generations > 1)) {
	/* Tidy the end of the to-space chains */
	step->hp_bd->free = step->hp;
	step->hp_bd->link = NULL;
      }

      /* for generations we collected... */
      if (g <= N) {

	collected += step->n_blocks * BLOCK_SIZE_W; /* for stats */

	/* free old memory and shift to-space into from-space for all
	 * the collected steps (except the allocation area).  These
	 * freed blocks will probaby be quickly recycled.
	 */
	if (!(g == 0 && s == 0)) {
	  freeChain(step->blocks);
	  step->blocks = step->to_space;
	  step->n_blocks = step->to_blocks;
	  step->to_space = NULL;
	  step->to_blocks = 0;
	  for (bd = step->blocks; bd != NULL; bd = bd->link) {
	    bd->evacuated = 0;	/* now from-space */
	  }
	}

	/* LARGE OBJECTS.  The current live large objects are chained on
	 * scavenged_large, having been moved during garbage
	 * collection from large_objects.  Any objects left on
	 * large_objects list are therefore dead, so we free them here.
	 */
	for (bd = step->large_objects; bd != NULL; bd = next) {
	  next = bd->link;
	  freeGroup(bd);
	  bd = next;
	}
	for (bd = step->scavenged_large_objects; bd != NULL; bd = bd->link) {
	  bd->evacuated = 0;
	}
	step->large_objects = step->scavenged_large_objects;

	/* Set the maximum blocks for this generation, interpolating
	 * between the maximum size of the oldest and youngest
	 * generations.
	 *
	 * max_blocks =    oldgen_max_blocks * G
	 *                 ----------------------
	 *                      oldest_gen
	 */
	if (g != 0) {
	  generations[g].max_blocks = (oldest_gen->max_blocks * g)
	       / (RtsFlags.GcFlags.generations-1);
	}

      /* for older generations... */
      } else {
	
	/* For older generations, we need to append the
	 * scavenged_large_object list (i.e. large objects that have been
	 * promoted during this GC) to the large_object list for that step.
	 */
	for (bd = step->scavenged_large_objects; bd; bd = next) {
	  next = bd->link;
	  bd->evacuated = 0;
	  dbl_link_onto(bd, &step->large_objects);
	}

	/* add the new blocks we promoted during this GC */
	step->n_blocks += step->to_blocks;
      }
    }
  }
  
  /* Guess the amount of live data for stats. */
  live = calcLive();

  /* Two-space collector:
   * Free the old to-space, and estimate the amount of live data.
   */
  if (RtsFlags.GcFlags.generations == 1) {
    nat blocks;
    
    if (old_to_space != NULL) {
      freeChain(old_to_space);
    }
    for (bd = g0s0->to_space; bd != NULL; bd = bd->link) {
      bd->evacuated = 0;	/* now from-space */
    }

    /* For a two-space collector, we need to resize the nursery. */
    
    /* set up a new nursery.  Allocate a nursery size based on a
     * function of the amount of live data (currently a factor of 2,
     * should be configurable (ToDo)).  Use the blocks from the old
     * nursery if possible, freeing up any left over blocks.
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

    if ( blocks * RtsFlags.GcFlags.oldGenFactor * 2 > 
	 RtsFlags.GcFlags.maxHeapSize ) {
      int adjusted_blocks;  /* signed on purpose */
      int pc_free; 
      
      adjusted_blocks = (RtsFlags.GcFlags.maxHeapSize - 2 * blocks);
      IF_DEBUG(gc, fprintf(stderr, "Near maximum heap size of 0x%x blocks, blocks = %d, adjusted to %d\n", RtsFlags.GcFlags.maxHeapSize, blocks, adjusted_blocks));
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
      int blocks;
      nat needed = calcNeeded(); 	/* approx blocks needed at next GC */

      /* Guess how much will be live in generation 0 step 0 next time.
       * A good approximation is the amount of data that was live this
       * time:  this assumes (1) that the size of G0S0 will be roughly
       * the same as last time, and (2) that the promotion rate will be
       * constant.
       *
       * If we don't know how much was live in G0S0 (because there's no
       * step 1), then assume 30% (which is usually an overestimate).
       */
      if (g0->n_steps == 1) {
	needed += (g0s0->n_blocks * 30) / 100;
      } else {
	needed += g0->steps[1].n_blocks;
      }

      /* Now we have a rough guess at the number of blocks needed for
       * the next GC, subtract this from the user's suggested heap size
       * and use the rest for the allocation area.
       */
      blocks = (int)RtsFlags.GcFlags.heapSizeSuggestion - (int)needed;
      
      if (blocks < (int)RtsFlags.GcFlags.minAllocAreaSize) {
	blocks = RtsFlags.GcFlags.minAllocAreaSize;
      }
      
      resizeNursery((nat)blocks);
    }
  }

  /* revert dead CAFs and update enteredCAFs list */
  revertDeadCAFs();
  
  /* mark the garbage collected CAFs as dead */
#ifdef DEBUG
  if (major_gc) { gcCAFs(); }
#endif
  
  /* zero the scavenged static object list */
  if (major_gc) {
    zeroStaticObjectList(scavenged_static_objects);
  }

  /* Reset the nursery
   */
  for (bd = g0s0->blocks; bd; bd = bd->link) {
    bd->free = bd->start;
    ASSERT(bd->gen == g0);
    ASSERT(bd->step == g0s0);
  }
  current_nursery = g0s0->blocks;

  /* Free the small objects allocated via allocate(), since this will
   * all have been copied into G0S1 now.  
   */
  if (small_alloc_list != NULL) {
    freeChain(small_alloc_list);
  }
  small_alloc_list = NULL;
  alloc_blocks = 0;
  alloc_blocks_lim = RtsFlags.GcFlags.minAllocAreaSize;

  /* start any pending finalisers */
  scheduleFinalisers(old_weak_ptr_list);
  
  /* check sanity after GC */
  IF_DEBUG(sanity, checkSanity(N));

  /* extra GC trace info */
  IF_DEBUG(gc, stat_describe_gens());

#ifdef DEBUG
  /* symbol-table based profiling */
  /*  heapCensus(to_space); */ /* ToDo */
#endif

  /* restore enclosing cost centre */
#ifdef PROFILING
  CCCS = prev_CCS;
#endif

  /* check for memory leaks if sanity checking is on */
  IF_DEBUG(sanity, memInventory());

  /* ok, GC over: tell the stats department what happened. */
  stat_endGC(allocated, collected, live, N);
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

   For generational GC: we just don't try to finalise weak pointers in
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

  /* doesn't matter where we evacuate values/finalisers to, since
   * these pointers are treated as roots (iff the keys are alive).
   */
  evac_gen = 0;

  last_w = &old_weak_ptr_list;
  for (w = old_weak_ptr_list; w; w = next_w) {

    if ((new = isAlive(w->key))) {
      w->key = new;
      /* evacuate the value and finaliser */
      w->value = evacuate(w->value);
      w->finaliser = evacuate(w->finaliser);
      /* remove this weak ptr from the old_weak_ptr list */
      *last_w = w->link;
      /* and put it on the new weak ptr list */
      next_w  = w->link;
      w->link = weak_ptr_list;
      weak_ptr_list = w;
      flag = rtsTrue;
      IF_DEBUG(weak, fprintf(stderr,"Weak pointer still alive at %p -> %p\n", w, w->key));
      continue;
    }
    else {
      last_w = &(w->link);
      next_w = w->link;
      continue;
    }
  }
  
  /* If we didn't make any changes, then we can go round and kill all
   * the dead weak pointers.  The old_weak_ptr list is used as a list
   * of pending finalisers later on.
   */
  if (flag == rtsFalse) {
    for (w = old_weak_ptr_list; w; w = w->link) {
      w->value = evacuate(w->value);
      w->finaliser = evacuate(w->finaliser);
    }
    weak_done = rtsTrue;
  }

  return rtsTrue;
}

/* -----------------------------------------------------------------------------
   isAlive determines whether the given closure is still alive (after
   a garbage collection) or not.  It returns the new address of the
   closure if it is alive, or NULL otherwise.
   -------------------------------------------------------------------------- */

StgClosure *
isAlive(StgClosure *p)
{
  StgInfoTable *info;

  while (1) {

    info = get_itbl(p);

    /* ToDo: for static closures, check the static link field.
     * Problem here is that we sometimes don't set the link field, eg.
     * for static closures with an empty SRT or CONSTR_STATIC_NOCAFs.
     */

    /* ignore closures in generations that we're not collecting. */
    if (LOOKS_LIKE_STATIC(p) || Bdescr((P_)p)->gen->no > N) {
      return p;
    }
    
    switch (info->type) {
      
    case IND:
    case IND_STATIC:
    case IND_PERM:
    case IND_OLDGEN:		/* rely on compatible layout with StgInd */
    case IND_OLDGEN_PERM:
      /* follow indirections */
      p = ((StgInd *)p)->indirectee;
      continue;
      
    case EVACUATED:
      /* alive! */
      return ((StgEvacuated *)p)->evacuee;

    default:
      /* dead. */
      return NULL;
    }
  }
}

StgClosure *
MarkRoot(StgClosure *root)
{
  return evacuate(root);
}

static void addBlock(step *step)
{
  bdescr *bd = allocBlock();
  bd->gen = step->gen;
  bd->step = step;

  if (step->gen->no <= N) {
    bd->evacuated = 1;
  } else {
    bd->evacuated = 0;
  }

  step->hp_bd->free = step->hp;
  step->hp_bd->link = bd;
  step->hp = bd->start;
  step->hpLim = step->hp + BLOCK_SIZE_W;
  step->hp_bd = bd;
  step->to_blocks++;
}

static __inline__ StgClosure *
copy(StgClosure *src, nat size, step *step)
{
  P_ to, from, dest;

  TICK_GC_WORDS_COPIED(size);
  /* Find out where we're going, using the handy "to" pointer in 
   * the step of the source object.  If it turns out we need to
   * evacuate to an older generation, adjust it here (see comment
   * by evacuate()).
   */
  if (step->gen->no < evac_gen) {
    step = &generations[evac_gen].steps[0];
  }

  /* chain a new block onto the to-space for the destination step if
   * necessary.
   */
  if (step->hp + size >= step->hpLim) {
    addBlock(step);
  }

  for(to = step->hp, from = (P_)src; size>0; --size) {
    *to++ = *from++;
  }

  dest = step->hp;
  step->hp = to;
  return (StgClosure *)dest;
}

/* Special version of copy() for when we only want to copy the info
 * pointer of an object, but reserve some padding after it.  This is
 * used to optimise evacuation of BLACKHOLEs.
 */

static __inline__ StgClosure *
copyPart(StgClosure *src, nat size_to_reserve, nat size_to_copy, step *step)
{
  P_ dest, to, from;

  TICK_GC_WORDS_COPIED(size_to_copy);
  if (step->gen->no < evac_gen) {
    step = &generations[evac_gen].steps[0];
  }

  if (step->hp + size_to_reserve >= step->hpLim) {
    addBlock(step);
  }

  for(to = step->hp, from = (P_)src; size_to_copy>0; --size_to_copy) {
    *to++ = *from++;
  }
  
  dest = step->hp;
  step->hp += size_to_reserve;
  return (StgClosure *)dest;
}

static __inline__ void 
upd_evacuee(StgClosure *p, StgClosure *dest)
{
  StgEvacuated *q = (StgEvacuated *)p;

  SET_INFO(q,&EVACUATED_info);
  q->evacuee = dest;
}

/* -----------------------------------------------------------------------------
   Evacuate a large object

   This just consists of removing the object from the (doubly-linked)
   large_alloc_list, and linking it on to the (singly-linked)
   new_large_objects list, from where it will be scavenged later.

   Convention: bd->evacuated is /= 0 for a large object that has been
   evacuated, or 0 otherwise.
   -------------------------------------------------------------------------- */

static inline void
evacuate_large(StgPtr p, rtsBool mutable)
{
  bdescr *bd = Bdescr(p);
  step *step;

  /* should point to the beginning of the block */
  ASSERT(((W_)p & BLOCK_MASK) == 0);
  
  /* already evacuated? */
  if (bd->evacuated) { 
    /* Don't forget to set the failed_to_evac flag if we didn't get
     * the desired destination (see comments in evacuate()).
     */
    if (bd->gen->no < evac_gen) {
      failed_to_evac = rtsTrue;
      TICK_GC_FAILED_PROMOTION();
    }
    return;
  }

  step = bd->step;
  /* remove from large_object list */
  if (bd->back) {
    bd->back->link = bd->link;
  } else { /* first object in the list */
    step->large_objects = bd->link;
  }
  if (bd->link) {
    bd->link->back = bd->back;
  }
  
  /* link it on to the evacuated large object list of the destination step
   */
  step = bd->step->to;
  if (step->gen->no < evac_gen) {
    step = &generations[evac_gen].steps[0];
  }

  bd->step = step;
  bd->gen = step->gen;
  bd->link = step->new_large_objects;
  step->new_large_objects = bd;
  bd->evacuated = 1;

  if (mutable) {
    recordMutable((StgMutClosure *)p);
  }
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
  step *step;

  step = &gen->steps[0];

  /* chain a new block onto the to-space for the destination step if
   * necessary.
   */
  if (step->hp + sizeofW(StgIndOldGen) >= step->hpLim) {
    addBlock(step);
  }

  q = (StgMutVar *)step->hp;
  step->hp += sizeofW(StgMutVar);

  SET_HDR(q,&MUT_CONS_info,CCS_GC);
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
  step *step;
  const StgInfoTable *info;

loop:
  if (!LOOKS_LIKE_STATIC(q)) {
    bd = Bdescr((P_)q);
    if (bd->gen->no > N) {
      /* Can't evacuate this object, because it's in a generation
       * older than the ones we're collecting.  Let's hope that it's
       * in evac_gen or older, or we will have to make an IND_OLDGEN object.
       */
      if (bd->gen->no < evac_gen) {
	/* nope */
	failed_to_evac = rtsTrue;
	TICK_GC_FAILED_PROMOTION();
      }
      return q;
    }
    step = bd->step->to;
  }

  /* make sure the info pointer is into text space */
  ASSERT(q && (LOOKS_LIKE_GHC_INFO(GET_INFO(q))
	       || IS_HUGS_CONSTR_INFO(GET_INFO(q))));

  info = get_itbl(q);
  switch (info -> type) {

  case BCO:
    to = copy(q,bco_sizeW(stgCast(StgBCO*,q)),step);
    upd_evacuee(q,to);
    return to;

  case MUT_VAR:
    ASSERT(q->header.info != &MUT_CONS_info);
  case MVAR:
    to = copy(q,sizeW_fromITBL(info),step);
    upd_evacuee(q,to);
    recordMutable((StgMutClosure *)to);
    return to;

  case STABLE_NAME:
    stable_ptr_table[((StgStableName *)q)->sn].keep = rtsTrue;
    to = copy(q,sizeofW(StgStableName),step);
    upd_evacuee(q,to);
    return to;

  case FUN_1_0:
  case FUN_0_1:
  case CONSTR_1_0:
  case CONSTR_0_1:
    to = copy(q,sizeofW(StgHeader)+1,step);
    upd_evacuee(q,to);
    return to;

  case THUNK_1_0:		/* here because of MIN_UPD_SIZE */
  case THUNK_0_1:
  case FUN_1_1:
  case FUN_0_2:
  case FUN_2_0:
  case THUNK_1_1:
  case THUNK_0_2:
  case THUNK_2_0:
  case CONSTR_1_1:
  case CONSTR_0_2:
  case CONSTR_2_0:
    to = copy(q,sizeofW(StgHeader)+2,step);
    upd_evacuee(q,to);
    return to;

  case FUN:
  case THUNK:
  case CONSTR:
  case IND_PERM:
  case IND_OLDGEN_PERM:
  case CAF_UNENTERED:
  case CAF_ENTERED:
  case WEAK:
  case FOREIGN:
    to = copy(q,sizeW_fromITBL(info),step);
    upd_evacuee(q,to);
    return to;

  case CAF_BLACKHOLE:
  case BLACKHOLE:
    to = copyPart(q,BLACKHOLE_sizeW(),sizeofW(StgHeader),step);
    upd_evacuee(q,to);
    return to;

  case BLACKHOLE_BQ:
    to = copy(q,BLACKHOLE_sizeW(),step); 
    upd_evacuee(q,to);
    recordMutable((StgMutClosure *)to);
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
	  StgNat32 offset = info->layout.selector_offset;

	  /* check that the size is in range */
	  ASSERT(offset < 
		 (StgNat32)(selectee_info->layout.payload.ptrs + 
		            selectee_info->layout.payload.nptrs));

	  /* perform the selection! */
	  q = selectee->payload[offset];

	  /* if we're already in to-space, there's no need to continue
	   * with the evacuation, just update the source address with
	   * a pointer to the (evacuated) constructor field.
	   */
	  if (IS_USER_PTR(q)) {
	    bdescr *bd = Bdescr((P_)q);
	    if (bd->evacuated) {
	      if (bd->gen->no < evac_gen) {
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
	selectee = stgCast(StgInd *,selectee)->indirectee;
	goto selector_loop;

      case CAF_ENTERED:
	selectee = stgCast(StgCAF *,selectee)->value;
	goto selector_loop;

      case EVACUATED:
	selectee = stgCast(StgEvacuated*,selectee)->evacuee;
	goto selector_loop;

      case THUNK:
      case THUNK_1_0:
      case THUNK_0_1:
      case THUNK_2_0:
      case THUNK_1_1:
      case THUNK_0_2:
      case THUNK_STATIC:
      case THUNK_SELECTOR:
	/* aargh - do recursively???? */
      case CAF_UNENTERED:
      case CAF_BLACKHOLE:
      case BLACKHOLE:
      case BLACKHOLE_BQ:
	/* not evaluated yet */
	break;

      default:
	barf("evacuate: THUNK_SELECTOR: strange selectee");
      }
    }
    to = copy(q,THUNK_SELECTOR_sizeW(),step);
    upd_evacuee(q,to);
    return to;

  case IND:
  case IND_OLDGEN:
    /* follow chains of indirections, don't evacuate them */
    q = ((StgInd*)q)->indirectee;
    goto loop;

    /* ToDo: optimise STATIC_LINK for known cases.
       - FUN_STATIC       : payload[0]
       - THUNK_STATIC     : payload[1]
       - IND_STATIC       : payload[1]
    */
  case THUNK_STATIC:
  case FUN_STATIC:
    if (info->srt_len == 0) {	/* small optimisation */
      return q;
    }
    /* fall through */
  case CONSTR_STATIC:
  case IND_STATIC:
    /* don't want to evacuate these, but we do want to follow pointers
     * from SRTs  - see scavenge_static.
     */

    /* put the object on the static list, if necessary.
     */
    if (major_gc && STATIC_LINK(info,(StgClosure *)q) == NULL) {
      STATIC_LINK(info,(StgClosure *)q) = static_objects;
      static_objects = (StgClosure *)q;
    }
    /* fall through */

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
    /* shouldn't see these */
    barf("evacuate: stack frame\n");

  case AP_UPD:
  case PAP:
    /* these are special - the payload is a copy of a chunk of stack,
       tagging and all. */
    to = copy(q,pap_sizeW(stgCast(StgPAP*,q)),step);
    upd_evacuee(q,to);
    return to;

  case EVACUATED:
    /* Already evacuated, just return the forwarding address.
     * HOWEVER: if the requested destination generation (evac_gen) is
     * older than the actual generation (because the object was
     * already evacuated to a younger generation) then we have to
     * set the failed_to_evac flag to indicate that we couldn't 
     * manage to promote the object to the desired generation.
     */
    if (evac_gen > 0) {		/* optimisation */
      StgClosure *p = ((StgEvacuated*)q)->evacuee;
      if (Bdescr((P_)p)->gen->no < evac_gen) {
	/*	fprintf(stderr,"evac failed!\n");*/
	failed_to_evac = rtsTrue;
	TICK_GC_FAILED_PROMOTION();
      }
    }
    return ((StgEvacuated*)q)->evacuee;

  case ARR_WORDS:
    {
      nat size = arr_words_sizeW(stgCast(StgArrWords*,q)); 

      if (size >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	evacuate_large((P_)q, rtsFalse);
	return q;
      } else {
	/* just copy the block */
	to = copy(q,size,step);
	upd_evacuee(q,to);
	return to;
      }
    }

  case MUT_ARR_PTRS:
  case MUT_ARR_PTRS_FROZEN:
    {
      nat size = mut_arr_ptrs_sizeW(stgCast(StgMutArrPtrs*,q)); 

      if (size >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	evacuate_large((P_)q, info->type == MUT_ARR_PTRS);
	to = q;
      } else {
	/* just copy the block */
	to = copy(q,size,step);
	upd_evacuee(q,to);
	if (info->type == MUT_ARR_PTRS) {
	  recordMutable((StgMutClosure *)to);
	}
      }
      return to;
    }

  case TSO:
    {
      StgTSO *tso = stgCast(StgTSO *,q);
      nat size = tso_sizeW(tso);
      int diff;

      /* Large TSOs don't get moved, so no relocation is required.
       */
      if (size >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	evacuate_large((P_)q, rtsTrue);
	return q;

      /* To evacuate a small TSO, we need to relocate the update frame
       * list it contains.  
       */
      } else {
	StgTSO *new_tso = (StgTSO *)copy((StgClosure *)tso,tso_sizeW(tso),step);

	diff = (StgPtr)new_tso - (StgPtr)tso; /* In *words* */

	/* relocate the stack pointers... */
	new_tso->su = (StgUpdateFrame *) ((StgPtr)new_tso->su + diff);
	new_tso->sp = (StgPtr)new_tso->sp + diff;
	new_tso->splim = (StgPtr)new_tso->splim + diff;
	
	relocate_TSO(tso, new_tso);
	upd_evacuee(q,(StgClosure *)new_tso);

	recordMutable((StgMutClosure *)new_tso);
	return (StgClosure *)new_tso;
      }
    }

  case BLOCKED_FETCH:
  case FETCH_ME:
    fprintf(stderr,"evacuate: unimplemented/strange closure type\n");
    return q;

  default:
    barf("evacuate: strange closure type");
  }

  barf("evacuate");
}

/* -----------------------------------------------------------------------------
   relocate_TSO is called just after a TSO has been copied from src to
   dest.  It adjusts the update frame list for the new location.
   -------------------------------------------------------------------------- */

StgTSO *
relocate_TSO(StgTSO *src, StgTSO *dest)
{
  StgUpdateFrame *su;
  StgCatchFrame  *cf;
  StgSeqFrame    *sf;
  int diff;

  diff = (StgPtr)dest->sp - (StgPtr)src->sp; /* In *words* */

  su = dest->su;

  while ((P_)su < dest->stack + dest->stack_size) {
    switch (get_itbl(su)->type) {
   
      /* GCC actually manages to common up these three cases! */

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
      /* all done! */
      break;

    default:
      barf("relocate_TSO");
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
  srt = stgCast(StgClosure **,info->srt);
  srt_end = srt + info->srt_len;
  for (; srt < srt_end; srt++) {
    evacuate(*srt);
  }
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
scavenge(step *step)
{
  StgPtr p, q;
  const StgInfoTable *info;
  bdescr *bd;
  nat saved_evac_gen = evac_gen; /* used for temporarily changing evac_gen */

  p = step->scan;
  bd = step->scan_bd;

  failed_to_evac = rtsFalse;

  /* scavenge phase - standard breadth-first scavenging of the
   * evacuated objects 
   */

  while (bd != step->hp_bd || p < step->hp) {

    /* If we're at the end of this block, move on to the next block */
    if (bd != step->hp_bd && p == bd->free) {
      bd = bd->link;
      p = bd->start;
      continue;
    }

    q = p;			/* save ptr to object */

    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO((StgClosure *)p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO((StgClosure *)p))));

    info = get_itbl((StgClosure *)p);
    switch (info -> type) {

    case BCO:
      {
	StgBCO* bco = stgCast(StgBCO*,p);
	nat i;
	for (i = 0; i < bco->n_ptrs; i++) {
	  bcoConstCPtr(bco,i) = evacuate(bcoConstCPtr(bco,i));
	}
	p += bco_sizeW(bco);
	break;
      }

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
	p += sizeofW(StgMVar);
	evac_gen = saved_evac_gen;
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
      p += sizeofW(StgHeader) + 2; /* MIN_UPD_SIZE */
      break;

    case FUN_1_0:
      scavenge_srt(info);
    case CONSTR_1_0:
      ((StgClosure *)p)->payload[0] = evacuate(((StgClosure *)p)->payload[0]);
      p += sizeofW(StgHeader) + 1;
      break;

    case THUNK_0_1:
      scavenge_srt(info);
      p += sizeofW(StgHeader) + 2; /* MIN_UPD_SIZE */
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
      /* fall through */

    case CONSTR:
    case WEAK:
    case FOREIGN:
    case STABLE_NAME:
    case IND_PERM:
    case IND_OLDGEN_PERM:
    case CAF_UNENTERED:
    case CAF_ENTERED:
      {
	StgPtr end;

	end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	p += info->layout.payload.nptrs;
	break;
      }

    case MUT_VAR:
      /* ignore MUT_CONSs */
      if (((StgMutVar *)p)->header.info != &MUT_CONS_info) {
	evac_gen = 0;
	((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
	evac_gen = saved_evac_gen;
      }
      p += sizeofW(StgMutVar);
      break;

    case CAF_BLACKHOLE:
    case BLACKHOLE:
	p += BLACKHOLE_sizeW();
	break;

    case BLACKHOLE_BQ:
      { 
	StgBlockingQueue *bh = (StgBlockingQueue *)p;
	(StgClosure *)bh->blocking_queue = 
	  evacuate((StgClosure *)bh->blocking_queue);
	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  recordMutable((StgMutClosure *)bh);
	}
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

    case IND:
    case IND_OLDGEN:
      barf("scavenge:IND???\n");

    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
    case THUNK_STATIC:
    case FUN_STATIC:
    case IND_STATIC:
      /* Shouldn't see a static object here. */
      barf("scavenge: STATIC object\n");

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
      /* Shouldn't see stack frames here. */
      barf("scavenge: stack frame\n");

    case AP_UPD: /* same as PAPs */
    case PAP:
      /* Treat a PAP just like a section of stack, not forgetting to
       * evacuate the function pointer too...
       */
      { 
	StgPAP* pap = stgCast(StgPAP*,p);

	pap->fun = evacuate(pap->fun);
	scavenge_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
	p += pap_sizeW(pap);
	break;
      }
      
    case ARR_WORDS:
      /* nothing to follow */
      p += arr_words_sizeW(stgCast(StgArrWords*,p));
      break;

    case MUT_ARR_PTRS:
      /* follow everything */
      {
	StgPtr next;

	evac_gen = 0;		/* repeatedly mutable */
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	evac_gen = saved_evac_gen;
	break;
      }

    case MUT_ARR_PTRS_FROZEN:
      /* follow everything */
      {
	StgPtr start = p, next;

	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	if (failed_to_evac) {
	  /* we can do this easier... */
	  recordMutable((StgMutClosure *)start);
	  failed_to_evac = rtsFalse;
	}
	break;
      }

    case TSO:
      { 
	StgTSO *tso;
	
	tso = (StgTSO *)p;
	evac_gen = 0;
	/* chase the link field for any TSOs on the same queue */
	(StgClosure *)tso->link = evacuate((StgClosure *)tso->link);
	/* scavenge this thread's stack */
	scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
	evac_gen = saved_evac_gen;
	p += tso_sizeW(tso);
	break;
      }

    case BLOCKED_FETCH:
    case FETCH_ME:
    case EVACUATED:
      barf("scavenge: unimplemented/strange closure type\n");

    default:
      barf("scavenge");
    }

    /* If we didn't manage to promote all the objects pointed to by
     * the current object, then we have to designate this object as
     * mutable (because it contains old-to-new generation pointers).
     */
    if (failed_to_evac) {
      mkMutCons((StgClosure *)q, &generations[evac_gen]);
      failed_to_evac = rtsFalse;
    }
  }

  step->scan_bd = bd;
  step->scan = p;
}    

/* -----------------------------------------------------------------------------
   Scavenge one object.

   This is used for objects that are temporarily marked as mutable
   because they contain old-to-new generation pointers.  Only certain
   objects can have this property.
   -------------------------------------------------------------------------- */
static rtsBool
scavenge_one(StgClosure *p)
{
  StgInfoTable *info;
  rtsBool no_luck;

  ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
	       || IS_HUGS_CONSTR_INFO(GET_INFO(p))));

  info = get_itbl(p);

  switch (info -> type) {

  case FUN:
  case FUN_1_0:			/* hardly worth specialising these guys */
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
  case CAF_UNENTERED:
  case CAF_ENTERED:
    {
      StgPtr q, end;
      
      end = (P_)p->payload + info->layout.payload.ptrs;
      for (q = (P_)p->payload; q < end; q++) {
	(StgClosure *)*q = evacuate((StgClosure *)*q);
      }
      break;
    }

  case CAF_BLACKHOLE:
  case BLACKHOLE:
      break;

  case THUNK_SELECTOR:
    { 
      StgSelector *s = (StgSelector *)p;
      s->selectee = evacuate(s->selectee);
      break;
    }
    
  case AP_UPD: /* same as PAPs */
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

  case IND_OLDGEN:
    /* This might happen if for instance a MUT_CONS was pointing to a
     * THUNK which has since been updated.  The IND_OLDGEN will
     * be on the mutable list anyway, so we don't need to do anything
     * here.
     */
    break;

  default:
    barf("scavenge_one: strange object");
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
  StgInfoTable *info;
  StgMutClosure *p, *next, *new_list;

  p = gen->mut_once_list;
  new_list = END_MUT_LIST;
  next = p->mut_link;

  evac_gen = gen->no;
  failed_to_evac = rtsFalse;

  for (; p != END_MUT_LIST; p = next, next = p->mut_link) {

    /* make sure the info pointer is into text space */
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO(p))));
    
    info = get_itbl(p);
    switch(info->type) {
      
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
      /* Try to pull the indirectee into this generation, so we can
       * remove the indirection from the mutable list.  
       */
      ((StgIndOldGen *)p)->indirectee = 
        evacuate(((StgIndOldGen *)p)->indirectee);
      
#if 0  
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
	fprintf(stderr,"evac IND_OLDGEN: %d bytes\n", size * sizeof(W_));
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
      
    case MUT_VAR:
      /* MUT_CONS is a kind of MUT_VAR, except it that we try to remove
       * it from the mutable list if possible by promoting whatever it
       * points to.
       */
      ASSERT(p->header.info == &MUT_CONS_info);
      if (scavenge_one(((StgMutVar *)p)->var) == rtsTrue) {
	/* didn't manage to promote everything, so put the
	 * MUT_CONS back on the list.
	 */
	p->mut_link = new_list;
	new_list = p;
      } 
      continue;
      
    default:
      /* shouldn't have anything else on the mutables list */
      barf("scavenge_mut_once_list: strange object?");
    }
  }

  gen->mut_once_list = new_list;
}


static void
scavenge_mutable_list(generation *gen)
{
  StgInfoTable *info;
  StgMutClosure *p, *next, *new_list;

  p = gen->saved_mut_list;
  new_list = END_MUT_LIST;
  next = p->mut_link;

  evac_gen = 0;
  failed_to_evac = rtsFalse;

  for (; p != END_MUT_LIST; p = next, next = p->mut_link) {

    /* make sure the info pointer is into text space */
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO(p))));
    
    info = get_itbl(p);
    switch(info->type) {
      
    case MUT_ARR_PTRS_FROZEN:
      /* remove this guy from the mutable list, but follow the ptrs
       * anyway (and make sure they get promoted to this gen).
       */
      {
	StgPtr end, q;
	
	end = (P_)p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	evac_gen = gen->no;
	for (q = (P_)((StgMutArrPtrs *)p)->payload; q < end; q++) {
	  (StgClosure *)*q = evacuate((StgClosure *)*q);
	}
	evac_gen = 0;

	if (failed_to_evac) {
	  failed_to_evac = rtsFalse;
	  p->mut_link = new_list;
	  new_list = p;
	} 
	continue;
      }

    case MUT_ARR_PTRS:
      /* follow everything */
      p->mut_link = new_list;
      new_list = p;
      {
	StgPtr end, q;
	
	end = (P_)p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (q = (P_)((StgMutArrPtrs *)p)->payload; q < end; q++) {
	  (StgClosure *)*q = evacuate((StgClosure *)*q);
	}
	continue;
      }
      
    case MUT_VAR:
      /* MUT_CONS is a kind of MUT_VAR, except that we try to remove
       * it from the mutable list if possible by promoting whatever it
       * points to.
       */
      ASSERT(p->header.info != &MUT_CONS_info);
      ((StgMutVar *)p)->var = evacuate(((StgMutVar *)p)->var);
      p->mut_link = new_list;
      new_list = p;
      continue;
      
    case MVAR:
      {
	StgMVar *mvar = (StgMVar *)p;
	(StgClosure *)mvar->head = evacuate((StgClosure *)mvar->head);
	(StgClosure *)mvar->tail = evacuate((StgClosure *)mvar->tail);
	(StgClosure *)mvar->value = evacuate((StgClosure *)mvar->value);
	p->mut_link = new_list;
	new_list = p;
	continue;
      }

    case TSO:
      /* follow ptrs and remove this from the mutable list */
      { 
	StgTSO *tso = (StgTSO *)p;

	/* Don't bother scavenging if this thread is dead 
	 */
	if (!(tso->whatNext == ThreadComplete ||
	      tso->whatNext == ThreadKilled)) {
	  /* Don't need to chase the link field for any TSOs on the
	   * same queue. Just scavenge this thread's stack 
	   */
	  scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
	}

	/* Don't take this TSO off the mutable list - it might still
	 * point to some younger objects (because we set evac_gen to 0
	 * above). 
	 */
	tso->mut_link = new_list;
	new_list = (StgMutClosure *)tso;
	continue;
      }
      
    case BLACKHOLE_BQ:
      { 
	StgBlockingQueue *bh = (StgBlockingQueue *)p;
	(StgClosure *)bh->blocking_queue = 
	  evacuate((StgClosure *)bh->blocking_queue);
	p->mut_link = new_list;
	new_list = p;
	continue;
      }

    default:
      /* shouldn't have anything else on the mutables list */
      barf("scavenge_mut_list: strange object?");
    }
  }

  gen->mut_list = new_list;
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

    /* make sure the info pointer is into text space */
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
	  scavenged_static_objects = STATIC_LINK(info,p);
	  ((StgMutClosure *)ind)->mut_link = oldest_gen->mut_once_list;
	  oldest_gen->mut_once_list = (StgMutClosure *)ind;
	}
	break;
      }
      
    case THUNK_STATIC:
    case FUN_STATIC:
      scavenge_srt(info);
      /* fall through */
      
    case CONSTR_STATIC:
      {	
	StgPtr q, next;
	
	next = (P_)p->payload + info->layout.payload.ptrs;
	/* evacuate the pointers */
	for (q = (P_)p->payload; q < next; q++) {
	  (StgClosure *)*q = evacuate((StgClosure *)*q);
	}
	break;
      }
      
    default:
      barf("scavenge_static");
    }

    ASSERT(failed_to_evac == rtsFalse);

    /* get the next static object from the list.  Remeber, there might
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
  StgNat32 bitmap;

  /* 
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with either a pending argument section or an 
   * activation record. 
   */

  while (p < stack_end) {
    q = *stgCast(StgPtr*,p);

    /* If we've got a tag, skip over that many words on the stack */
    if (IS_ARG_TAG(stgCast(StgWord,q))) {
      p += ARG_SIZE(q);
      p++; continue;
    }
     
    /* Is q a pointer to a closure?
     */
    if (! LOOKS_LIKE_GHC_INFO(q)) {

#ifdef DEBUG
      if (LOOKS_LIKE_STATIC(q)) { /* Is it a static closure? */
	ASSERT(closure_STATIC(stgCast(StgClosure*,q)));
      } 
      /* otherwise, must be a pointer into the allocation space.
       */
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
    info  = get_itbl(stgCast(StgClosure*,p));
      
    switch (info->type) {
	
      /* Dynamic bitmap: the mask is stored on the stack */
    case RET_DYN:
      bitmap = stgCast(StgRetDyn*,p)->liveness;
      p      = &payloadWord(stgCast(StgRetDyn*,p),0);
      goto small_bitmap;

      /* probably a slow-entry point return address: */
    case FUN:
    case FUN_STATIC:
      p++;
      goto follow_srt;

      /* Specialised code for update frames, since they're so common.
       * We *know* the updatee points to a BLACKHOLE, CAF_BLACKHOLE,
       * or BLACKHOLE_BQ, so just inline the code to evacuate it here.  
       */
    case UPDATE_FRAME:
      {
	StgUpdateFrame *frame = (StgUpdateFrame *)p;
	StgClosure *to;
	StgClosureType type = get_itbl(frame->updatee)->type;

	p += sizeofW(StgUpdateFrame);
	if (type == EVACUATED) {
	  frame->updatee = evacuate(frame->updatee);
	  continue;
	} else {
	  bdescr *bd = Bdescr((P_)frame->updatee);
	  step *step;
	  if (bd->gen->no > N) { 
	    if (bd->gen->no < evac_gen) {
	      failed_to_evac = rtsTrue;
	    }
	    continue;
	  }
	  step = bd->step->to;
	  switch (type) {
	  case BLACKHOLE:
	  case CAF_BLACKHOLE:
	    to = copyPart(frame->updatee, BLACKHOLE_sizeW(), 
			  sizeofW(StgHeader), step);
	    upd_evacuee(frame->updatee,to);
	    frame->updatee = to;
	    continue;
	  case BLACKHOLE_BQ:
	    to = copy(frame->updatee, BLACKHOLE_sizeW(), step);
	    upd_evacuee(frame->updatee,to);
	    frame->updatee = to;
	    recordMutable((StgMutClosure *)to);
	    continue;
	  default:
	    barf("scavenge_stack: UPDATE_FRAME updatee");
	  }
	}
      }

      /* small bitmap (< 32 entries, or 64 on a 64-bit machine) */
    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
    case STOP_FRAME:
    case CATCH_FRAME:
    case SEQ_FRAME:
      bitmap = info->layout.bitmap;
      p++;
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

      /* large bitmap (> 32 entries) */
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
	  q = p + sizeof(W_) * 8;
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

	/* and don't forget to follow the SRT */
	goto follow_srt;
      }

    default:
      barf("scavenge_stack: weird activation record found on stack.\n");
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
scavenge_large(step *step)
{
  bdescr *bd;
  StgPtr p;
  const StgInfoTable* info;
  nat saved_evac_gen = evac_gen; /* used for temporarily changing evac_gen */

  evac_gen = 0;			/* most objects are mutable */
  bd = step->new_large_objects;

  for (; bd != NULL; bd = step->new_large_objects) {

    /* take this object *off* the large objects list and put it on
     * the scavenged large objects list.  This is so that we can
     * treat new_large_objects as a stack and push new objects on
     * the front when evacuating.
     */
    step->new_large_objects = bd->link;
    dbl_link_onto(bd, &step->scavenged_large_objects);

    p = bd->start;
    info  = get_itbl(stgCast(StgClosure*,p));

    switch (info->type) {

    /* only certain objects can be "large"... */

    case ARR_WORDS:
      /* nothing to follow */
      continue;

    case MUT_ARR_PTRS:
      /* follow everything */
      {
	StgPtr next;

	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	continue;
      }

    case MUT_ARR_PTRS_FROZEN:
      /* follow everything */
      {
	StgPtr start = p, next;

	evac_gen = saved_evac_gen; /* not really mutable */
	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	evac_gen = 0;
	if (failed_to_evac) {
	  recordMutable((StgMutClosure *)start);
	}
	continue;
      }

    case BCO:
      {
	StgBCO* bco = stgCast(StgBCO*,p);
	nat i;
	evac_gen = saved_evac_gen;
	for (i = 0; i < bco->n_ptrs; i++) {
	  bcoConstCPtr(bco,i) = evacuate(bcoConstCPtr(bco,i));
	}
	evac_gen = 0;
	continue;
      }

    case TSO:
      { 
	StgTSO *tso;
	
	tso = (StgTSO *)p;
	/* chase the link field for any TSOs on the same queue */
	(StgClosure *)tso->link = evacuate((StgClosure *)tso->link);
	/* scavenge this thread's stack */
	scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
	continue;
      }

    default:
      barf("scavenge_large: unknown/strange object");
    }
  }
}

static void
zeroStaticObjectList(StgClosure* first_static)
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
zeroMutableList(StgMutClosure *first)
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

void RevertCAFs(void)
{
  while (enteredCAFs != END_CAF_LIST) {
    StgCAF* caf = enteredCAFs;
    
    enteredCAFs = caf->link;
    ASSERT(get_itbl(caf)->type == CAF_ENTERED);
    SET_INFO(caf,&CAF_UNENTERED_info);
    caf->value = stgCast(StgClosure*,0xdeadbeef);
    caf->link  = stgCast(StgCAF*,0xdeadbeef);
  }
}

void revertDeadCAFs(void)
{
    StgCAF* caf = enteredCAFs;
    enteredCAFs = END_CAF_LIST;
    while (caf != END_CAF_LIST) {
	StgCAF* next = caf->link;

	switch(GET_INFO(caf)->type) {
	case EVACUATED:
	    {
		/* This object has been evacuated, it must be live. */
		StgCAF* new = stgCast(StgCAF*,stgCast(StgEvacuated*,caf)->evacuee);
		new->link = enteredCAFs;
		enteredCAFs = new;
		break;
	    }
	case CAF_ENTERED:
	    {
		SET_INFO(caf,&CAF_UNENTERED_info);
		caf->value = stgCast(StgClosure*,0xdeadbeef);
		caf->link  = stgCast(StgCAF*,0xdeadbeef);
		break;
	    }
	default:
		barf("revertDeadCAFs: enteredCAFs list corrupted");
	} 
	caf = next;
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

#ifdef DEBUG
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
      IF_DEBUG(gccafs, fprintf(stderr, "CAF gc'd at 0x%04x\n", (int)p));
      /* black hole it */
      SET_INFO(p,&BLACKHOLE_info);
      p = STATIC_LINK2(info,p);
      *pp = p;
    }
    else {
      pp = &STATIC_LINK2(info,p);
      p = *pp;
      i++;
    }

  }

  /*  fprintf(stderr, "%d CAFs live\n", i); */
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
      update_frame = stgCast(StgCatchFrame*,update_frame)->link;
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
      if (bh->header.info == &BLACKHOLE_info) {
	return;
      }

      if (bh->header.info != &BLACKHOLE_BQ_info &&
	  bh->header.info != &CAF_BLACKHOLE_info) {
	SET_INFO(bh,&BLACKHOLE_info);
      }

      update_frame = update_frame->link;
      break;

    case SEQ_FRAME:
      update_frame = stgCast(StgSeqFrame*,update_frame)->link;
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
  StgUpdateFrame *next_frame;		        /* Temporally next */
  StgUpdateFrame *prev_frame;			/* Temporally previous */
  StgPtr bottom;
  rtsBool prev_was_update_frame;
  
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
  while ((P_)frame < bottom - 1) {  /* bottom - 1 is the STOP_FRAME */
    prev_frame = frame->link;
    frame->link = next_frame;
    next_frame = frame;
    frame = prev_frame;
    if (get_itbl(frame)->type == UPDATE_FRAME
	&& frame->updatee->header.info == &BLACKHOLE_info) {
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
      
      /* Now squeeze out the current frame */
      StgClosure *updatee_keep   = prev_frame->updatee;
      StgClosure *updatee_bypass = frame->updatee;
      
#if 0 /* DEBUG */
      fprintf(stderr, "squeezing frame at %p\n", frame);
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
#if 0 /* do it properly... */
      if (GET_INFO(updatee_bypass) == BLACKHOLE_BQ_info) {
	/* Sigh.  It has one.  Don't lose those threads! */
	  if (GET_INFO(updatee_keep) == BLACKHOLE_BQ_info) {
	  /* Urgh.  Two queues.  Merge them. */
	  P_ keep_tso = ((StgBlockingQueue *)updatee_keep)->blocking_queue;
	  
	  while (keep_tso->link != END_TSO_QUEUE) {
	    keep_tso = keep_tso->link;
	  }
	  keep_tso->link = ((StgBlockingQueue *)updatee_bypass)->blocking_queue;

	} else {
	  /* For simplicity, just swap the BQ for the BH */
	  P_ temp = updatee_keep;
	  
	  updatee_keep = updatee_bypass;
	  updatee_bypass = temp;
	  
	  /* Record the swap in the kept frame (below) */
	  prev_frame->updatee = updatee_keep;
	}
      }
#endif

      TICK_UPD_SQUEEZED();
      UPD_IND(updatee_bypass, updatee_keep); /* this wakes the threads up */
      
      sp = (P_)frame - 1;	/* sp = stuff to slide */
      displacement += sizeofW(StgUpdateFrame);
      
    } else {
      /* No squeeze for this frame */
      sp = frame_bottom - 1;	/* Keep the current frame */
      
      /* Do lazy black-holing.
       */
      if (is_update_frame) {
	StgBlockingQueue *bh = (StgBlockingQueue *)frame->updatee;
	if (bh->header.info != &BLACKHOLE_BQ_info &&
	    bh->header.info != &CAF_BLACKHOLE_info) {
	  SET_INFO(bh,&BLACKHOLE_info);
	}
      }

      /* Fix the link in the current frame (should point to the frame below) */
      frame->link = prev_frame;
      prev_was_update_frame = is_update_frame;
    }
    
    /* Now slide all words from sp up to the next frame */
    
    if (displacement > 0) {
      P_ next_frame_bottom;

      if (next_frame != NULL)
	next_frame_bottom = (P_)next_frame + sizeofW(StgUpdateFrame);
      else
	next_frame_bottom = tso->sp - 1;
      
#if 0 /* DEBUG */
      fprintf(stderr, "sliding [%p, %p] by %ld\n", sp, next_frame_bottom,
	      displacement);
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
    threadSqueezeStack(tso);	/* does black holing too */
  else
    threadLazyBlackHole(tso);
}
