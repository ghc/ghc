/* -----------------------------------------------------------------------------
 * $Id: GC.c,v 1.5 1999/01/06 12:27:47 simonm Exp $
 *
 * Two-space garbage collector
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

StgCAF* enteredCAFs;

static P_ toHp;			/* to-space heap pointer */
static P_ toHpLim;		/* end of current to-space block */
static bdescr *toHp_bd;		/* descriptor of current to-space block  */
static nat blocks = 0;		/* number of to-space blocks allocated */
static bdescr *old_to_space = NULL; /* to-space from the last GC */
static nat old_to_space_blocks = 0; /* size of previous to-space */

/* STATIC OBJECT LIST.
 *
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
 */
#define END_OF_STATIC_LIST stgCast(StgClosure*,1)
static StgClosure* static_objects;
static StgClosure* scavenged_static_objects;

/* WEAK POINTERS
 */
static StgWeak *old_weak_ptr_list; /* also pending finaliser list */
static rtsBool weak_done;	/* all done for this pass */

/* LARGE OBJECTS.
 */
static bdescr *new_large_objects; /* large objects evacuated so far */
static bdescr *scavenged_large_objects; /* large objects scavenged */

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static StgClosure *evacuate(StgClosure *q);
static void    zeroStaticObjectList(StgClosure* first_static);
static void    scavenge_stack(StgPtr p, StgPtr stack_end);
static void    scavenge_static(void);
static void    scavenge_large(void);
static StgPtr  scavenge(StgPtr to_scan);
static rtsBool traverse_weak_ptr_list(void);
static void    revertDeadCAFs(void);

#ifdef DEBUG
static void gcCAFs(void);
#endif

/* -----------------------------------------------------------------------------
   GarbageCollect

   This function performs a full copying garbage collection.
   -------------------------------------------------------------------------- */

void GarbageCollect(void (*get_roots)(void))
{
  bdescr *bd, *scan_bd, *to_space;
  StgPtr scan;
  lnat allocated, live;
  nat old_nursery_blocks = nursery_blocks;       /* for stats */
  nat old_live_blocks    = old_to_space_blocks;  /* ditto */
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
  if (CurrentTSO) 
    threadPaused(CurrentTSO);

  /* Approximate how much we allocated: number of blocks in the
   * nursery + blocks allocated via allocate() - unused nusery blocks.
   * This leaves a little slop at the end of each block, and doesn't
   * take into account large objects (ToDo).
   */
  allocated = (nursery_blocks * BLOCK_SIZE_W) + allocated_bytes();
  for ( bd = current_nursery->link; bd != NULL; bd = bd->link ) {
    allocated -= BLOCK_SIZE_W;
  }
  
  /* check stack sanity *before* GC (ToDo: check all threads) */
  /*IF_DEBUG(sanity, checkTSO(MainTSO,0)); */
  IF_DEBUG(sanity, checkFreeListSanity());

  static_objects = END_OF_STATIC_LIST;
  scavenged_static_objects = END_OF_STATIC_LIST;

  new_large_objects = NULL;
  scavenged_large_objects = NULL;

  /* Get a free block for to-space.  Extra blocks will be chained on
   * as necessary.
   */
  bd = allocBlock();
  bd->step = 1;			/* step 1 identifies to-space */
  toHp = bd->start;
  toHpLim = toHp + BLOCK_SIZE_W;
  toHp_bd = bd;
  to_space = bd;
  blocks = 0;

  scan = toHp;
  scan_bd = bd;

  /* follow all the roots that the application knows about */
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

  /* Then scavenge all the objects we picked up on the first pass. 
   * We may require multiple passes to find all the static objects,
   * large objects and normal objects.
   */
  { 
  loop:
    if (static_objects != END_OF_STATIC_LIST) {
      scavenge_static();
    }
    if (toHp_bd != scan_bd || scan < toHp) {
      scan = scavenge(scan);
      scan_bd = Bdescr(scan);
      goto loop;
    }
    if (new_large_objects != NULL) {
      scavenge_large();
      goto loop;
    }
    /* must be last... */
    if (traverse_weak_ptr_list()) { /* returns rtsTrue if evaced something */
      goto loop;
    }
  }

  /* tidy up the end of the to-space chain */
  toHp_bd->free = toHp;
  toHp_bd->link = NULL;
  
  /* revert dead CAFs and update enteredCAFs list */
  revertDeadCAFs();
  
  /* mark the garbage collected CAFs as dead */
#ifdef DEBUG
  gcCAFs();
#endif
  
  zeroStaticObjectList(scavenged_static_objects);
  
  /* approximate amount of live data (doesn't take into account slop
   * at end of each block).  ToDo: this more accurately.
   */
  live = blocks * BLOCK_SIZE_W + ((lnat)toHp_bd->free -
				  (lnat)toHp_bd->start) / sizeof(W_);

  /* Free the to-space from the last GC, as it has now been collected.
   * we may be able to re-use these blocks in creating a new nursery,
   * below.  If not, the blocks will probably be re-used for to-space
   * in the next GC.
   */
  if (old_to_space != NULL) {
    freeChain(old_to_space);
  }
  old_to_space = to_space;
  old_to_space_blocks = blocks;

  /* Free the small objects allocated via allocate(), since this will
   * all have been copied into to-space now.  
   */
  if (small_alloc_list != NULL) {
    freeChain(small_alloc_list);
  }
  small_alloc_list = NULL;
  alloc_blocks = 0;
  alloc_blocks_lim = stg_max(blocks,RtsFlags.GcFlags.minAllocAreaSize);

  /* LARGE OBJECTS.  The current live large objects are chained on
   * scavenged_large_objects, having been moved during garbage
   * collection from large_alloc_list.  Any objects left on
   * large_alloc list are therefore dead, so we free them here.
   */
  {
    bdescr *bd, *next;
    bd = large_alloc_list;
    while (bd != NULL) {
      next = bd->link;
      freeGroup(bd);
      bd = next;
    }
    large_alloc_list = scavenged_large_objects;
  }


  /* check sanity after GC */
  IF_DEBUG(sanity, checkHeap(to_space,1));
  /*IF_DEBUG(sanity, checkTSO(MainTSO,1)); */
  IF_DEBUG(sanity, checkFreeListSanity());

#ifdef DEBUG
  /* symbol-table based profiling */
  heapCensus(to_space);
#endif

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
  if ( blocks * 4 > RtsFlags.GcFlags.maxHeapSize ) {
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
    blocks *= 2;
    if (blocks < RtsFlags.GcFlags.minAllocAreaSize) {
     blocks = RtsFlags.GcFlags.minAllocAreaSize;
    }
  }
  
  if (nursery_blocks < blocks) {
    IF_DEBUG(gc, fprintf(stderr, "Increasing size of nursery to %d blocks\n", 
			 blocks));
    nursery = allocNursery(nursery,blocks-nursery_blocks);
  } else {
    bdescr *next_bd = nursery;

    IF_DEBUG(gc, fprintf(stderr, "Decreasing size of nursery to %d blocks\n", 
			 blocks));
    for (bd = nursery; nursery_blocks > blocks; nursery_blocks--) {
      next_bd = bd->link;
      freeGroup(bd);
      bd = next_bd;
    }
    nursery = bd;
  }
    
  current_nursery = nursery;
  nursery_blocks = blocks;

  /* set the step number for each block in the nursery to zero */
  for (bd = nursery; bd != NULL; bd = bd->link) {
    bd->step = 0;
    bd->free = bd->start;
  }
  for (bd = to_space; bd != NULL; bd = bd->link) {
    bd->step = 0;
  }
  for (bd = large_alloc_list; bd != NULL; bd = bd->link) {
    bd->step = 0;
  }

#ifdef DEBUG
  /* check that we really have the right number of blocks in the
   * nursery, or things could really get screwed up.
   */
  {
    nat i = 0;
    for (bd = nursery; bd != NULL; bd = bd->link) {
      ASSERT(bd->free == bd->start);
      ASSERT(bd->step == 0);
      i++;
    }
    ASSERT(i == nursery_blocks);
  }
#endif

  /* start any pending finalisers */
  scheduleFinalisers(old_weak_ptr_list);
  
  /* restore enclosing cost centre */
#ifdef PROFILING
  CCCS = prev_CCS;
#endif

  /* ok, GC over: tell the stats department what happened. */
  stat_endGC(allocated, 
	     (old_nursery_blocks + old_live_blocks) * BLOCK_SIZE_W,
	     live, "");
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
   -------------------------------------------------------------------------- */

static rtsBool 
traverse_weak_ptr_list(void)
{
  StgWeak *w, **last_w, *next_w;
  StgClosure *target;
  const StgInfoTable *info;
  rtsBool flag = rtsFalse;

  if (weak_done) { return rtsFalse; }

  last_w = &old_weak_ptr_list;
  for (w = old_weak_ptr_list; w; w = next_w) {
    target = w->key;
  loop:
    info = get_itbl(target);
    switch (info->type) {
      
    case IND:
    case IND_STATIC:
    case IND_PERM:
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
      /* follow indirections */
      target = ((StgInd *)target)->indirectee;
      goto loop;

    case EVACUATED:
      /* If key is alive, evacuate value and finaliser and 
       * place weak ptr on new weak ptr list.
       */
      IF_DEBUG(weak, fprintf(stderr,"Weak pointer still alive at %p\n", w));
      w->key = ((StgEvacuated *)target)->evacuee;
      w->value = evacuate(w->value);
      w->finaliser = evacuate(w->finaliser);
      
      /* remove this weak ptr from the old_weak_ptr list */
      *last_w = w->link;

      /* and put it on the new weak ptr list */
      next_w  = w->link;
      w->link = weak_ptr_list;
      weak_ptr_list = w;
      flag = rtsTrue;
      break;

    default:			/* key is dead */
      last_w = &(w->link);
      next_w = w->link;
      break;
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

StgClosure *MarkRoot(StgClosure *root)
{
  root = evacuate(root);
  return root;
}

static __inline__ StgClosure *copy(StgClosure *src, W_ size)
{
  P_ to, from, dest;

  if (toHp + size >= toHpLim) {
    bdescr *bd = allocBlock();
    toHp_bd->free = toHp;
    toHp_bd->link = bd;
    bd->step = 1;		/* step 1 identifies to-space */
    toHp = bd->start;
    toHpLim = toHp + BLOCK_SIZE_W;
    toHp_bd = bd;
    blocks++;
  }

  dest = toHp;
  toHp += size;
  for(to = dest, from = (P_)src; size>0; --size) {
    *to++ = *from++;
  }
  return (StgClosure *)dest;
}

static __inline__ void upd_evacuee(StgClosure *p, StgClosure *dest)
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
   -------------------------------------------------------------------------- */

static inline void evacuate_large(StgPtr p)
{
  bdescr *bd = Bdescr(p);

  /* should point to the beginning of the block */
  ASSERT(((W_)p & BLOCK_MASK) == 0);
  
  /* already evacuated? */
  if (bd->step == 1) {
    return;
  }

  /* remove from large_alloc_list */
  if (bd->back) {
    bd->back->link = bd->link;
  } else { /* first object in the list */
    large_alloc_list = bd->link;
  }
  if (bd->link) {
    bd->link->back = bd->back;
  }
  
  /* link it on to the evacuated large object list */
  bd->link = new_large_objects;
  new_large_objects = bd;
  bd->step = 1;
}  

/* -----------------------------------------------------------------------------
   Evacuate

   This is called (eventually) for every live object in the system.
   -------------------------------------------------------------------------- */

static StgClosure *evacuate(StgClosure *q)
{
  StgClosure *to;
  const StgInfoTable *info;

loop:
  /* make sure the info pointer is into text space */
  ASSERT(q && (LOOKS_LIKE_GHC_INFO(GET_INFO(q))
	       || IS_HUGS_CONSTR_INFO(GET_INFO(q))));

  info = get_itbl(q);
  switch (info -> type) {

  case BCO:
    to = copy(q,bco_sizeW(stgCast(StgBCO*,q)));
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
  case MUT_VAR:
  case MVAR:
    to = copy(q,sizeW_fromITBL(info));
    upd_evacuee(q,to);
    return to;

  case CAF_BLACKHOLE:
  case BLACKHOLE:
    to = copy(q,BLACKHOLE_sizeW());
    upd_evacuee(q,to);
    return to;

  case THUNK_SELECTOR:
    {
      const StgInfoTable* selectee_info;
      StgClosure* selectee = stgCast(StgSelector*,q)->selectee;

    selector_loop:
      selectee_info = get_itbl(selectee);
      switch (selectee_info->type) {
      case CONSTR:
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
	  if (IS_USER_PTR(q) && Bdescr((P_)q)->step == 1) {
	    return q;
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
      case THUNK_STATIC:
      case THUNK_SELECTOR:
	/* aargh - do recursively???? */
      case CAF_UNENTERED:
      case CAF_BLACKHOLE:
      case BLACKHOLE:
	/* not evaluated yet */
	break;

      default:
	barf("evacuate: THUNK_SELECTOR: strange selectee");
      }
    }
    to = copy(q,THUNK_SELECTOR_sizeW());
    upd_evacuee(q,to);
    return to;

  case IND:
  case IND_OLDGEN:
    /* follow chains of indirections, don't evacuate them */
    q = stgCast(StgInd*,q)->indirectee;
    goto loop;

  case CONSTR_STATIC:
  case THUNK_STATIC:
  case FUN_STATIC:
  case IND_STATIC:
    /* don't want to evacuate these, but we do want to follow pointers
     * from SRTs  - see scavenge_static.
     */

    /* put the object on the static list, if necessary.
     */
    if (STATIC_LINK(info,(StgClosure *)q) == NULL) {
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
    to = copy(q,pap_sizeW(stgCast(StgPAP*,q)));
    upd_evacuee(q,to);
    return to;

  case EVACUATED:
    /* Already evacuated, just return the forwarding address */
    return stgCast(StgEvacuated*,q)->evacuee;

  case MUT_ARR_WORDS:
  case ARR_WORDS:
  case MUT_ARR_PTRS:
  case MUT_ARR_PTRS_FROZEN:
  case ARR_PTRS:
    {
      nat size = arr_words_sizeW(stgCast(StgArrWords*,q)); 

      if (size >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	evacuate_large((P_)q);
	return q;
      } else {
	/* just copy the block */
	to = copy(q,size);
	upd_evacuee(q,to);
	return to;
      }
    }

  case TSO:
    {
      StgTSO *tso = stgCast(StgTSO *,q);
      nat size = tso_sizeW(tso);
      int diff;

      /* Large TSOs don't get moved, so no relocation is required.
       */
      if (size >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	evacuate_large((P_)q);
	return q;

      /* To evacuate a small TSO, we need to relocate the update frame
       * list it contains.  
       */
      } else {
	StgTSO *new_tso = (StgTSO *)copy((StgClosure *)tso,tso_sizeW(tso));

	diff = (StgPtr)new_tso - (StgPtr)tso; /* In *words* */

	/* relocate the stack pointers... */
	new_tso->su = (StgUpdateFrame *) ((StgPtr)new_tso->su + diff);
	new_tso->sp = (StgPtr)new_tso->sp + diff;
	new_tso->splim = (StgPtr)new_tso->splim + diff;
	
	relocate_TSO(tso, new_tso);
	upd_evacuee(q,(StgClosure *)new_tso);
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
evacuate_srt(const StgInfoTable *info)
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

static StgPtr
scavenge(StgPtr to_scan)
{
  StgPtr p;
  const StgInfoTable *info;
  bdescr *bd;

  p = to_scan;
  bd = Bdescr((P_)p);

  /* scavenge phase - standard breadth-first scavenging of the
   * evacuated objects 
   */

  while (bd != toHp_bd || p < toHp) {

    /* If we're at the end of this block, move on to the next block */
    if (bd != toHp_bd && p == bd->free) {
      bd = bd->link;
      p = bd->start;
      continue;
    }

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
	continue;
      }

    case FUN:
    case THUNK:
      evacuate_srt(info);
      /* fall through */

    case CONSTR:
    case WEAK:
    case FOREIGN:
    case MVAR:
    case MUT_VAR:
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
	continue;
      }

    case CAF_BLACKHOLE:
    case BLACKHOLE:
      { 
	StgBlackHole *bh = (StgBlackHole *)p;
	(StgClosure *)bh->blocking_queue = 
	  evacuate((StgClosure *)bh->blocking_queue);
	p += BLACKHOLE_sizeW();
	continue;
      }

    case THUNK_SELECTOR:
      { 
	StgSelector *s = (StgSelector *)p;
	s->selectee = evacuate(s->selectee);
	p += THUNK_SELECTOR_sizeW();
	continue;
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
	continue;
      }
      
    case ARR_WORDS:
    case MUT_ARR_WORDS:
      /* nothing to follow */
      p += arr_words_sizeW(stgCast(StgArrWords*,p));
      continue;

    case ARR_PTRS:
    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
      /* follow everything */
      {
	StgPtr next;

	next = p + arr_ptrs_sizeW(stgCast(StgArrPtrs*,p));
	for (p = (P_)((StgArrPtrs *)p)->payload; p < next; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
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
	p += tso_sizeW(tso);
	continue;
      }

    case BLOCKED_FETCH:
    case FETCH_ME:
    case EVACUATED:
      barf("scavenge: unimplemented/strange closure type\n");

    default:
      barf("scavenge");
    }
  }
  return (P_)p;
}    

/* scavenge_static is the scavenge code for a static closure.
 */

static void
scavenge_static(void)
{
  StgClosure* p = static_objects;
  const StgInfoTable *info;

  /* keep going until we've scavenged all the objects on the linked
     list... */
  while (p != END_OF_STATIC_LIST) {

    /* make sure the info pointer is into text space */
    ASSERT(p && LOOKS_LIKE_GHC_INFO(GET_INFO(p)));
    ASSERT(p && (LOOKS_LIKE_GHC_INFO(GET_INFO(p))
		 || IS_HUGS_CONSTR_INFO(GET_INFO(p))));

    info = get_itbl(p);

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
	break;
      }
      
    case THUNK_STATIC:
    case FUN_STATIC:
      evacuate_srt(info);
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
       * We *know* the updatee points to a BLACKHOLE or CAF_BLACKHOLE,
       * so just inline the code to evacuate it here.  
       */
    case UPDATE_FRAME:
      {
	StgUpdateFrame *frame = (StgUpdateFrame *)p;
	StgClosure *to;
	StgClosureType type = get_itbl(frame->updatee)->type;

	if (type == EVACUATED) {
	  frame->updatee = evacuate(frame->updatee);
	  p += sizeofW(StgUpdateFrame);
	  continue;
	} else {
	  ASSERT(type == BLACKHOLE || type == CAF_BLACKHOLE);
	  to = copy(frame->updatee, BLACKHOLE_sizeW());
	  upd_evacuee(frame->updatee,to);
	  frame->updatee = to;
	  p += sizeofW(StgUpdateFrame);
	  continue;
	}
      }

      /* small bitmap (< 32 entries) */
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
      evacuate_srt(info);
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
  --------------------------------------------------------------------------- */

static void
scavenge_large(void)
{
  bdescr *bd;
  StgPtr p;
  const StgInfoTable* info;

  bd = new_large_objects;

  for (; bd != NULL; bd = new_large_objects) {

    /* take this object *off* the large objects list and put it on
     * the scavenged large objects list.  This is so that we can
     * treat new_large_objects as a stack and push new objects on
     * the front when evacuating.
     */
    new_large_objects = bd->link;
    /* scavenged_large_objects is doubly linked */
    bd->link = scavenged_large_objects;
    bd->back = NULL;
    if (scavenged_large_objects) {
      scavenged_large_objects->back = bd;
    }
    scavenged_large_objects = bd;

    p = bd->start;
    info  = get_itbl(stgCast(StgClosure*,p));

    switch (info->type) {

    /* only certain objects can be "large"... */

    case ARR_WORDS:
    case MUT_ARR_WORDS:
      /* nothing to follow */
      continue;

    case ARR_PTRS:
    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
      /* follow everything */
      {
	StgPtr next;

	next = p + arr_ptrs_sizeW(stgCast(StgArrPtrs*,p));
	for (p = (P_)((StgArrPtrs *)p)->payload; p < next; p++) {
	  (StgClosure *)*p = evacuate((StgClosure *)*p);
	}
	continue;
      }

    case BCO:
      {
	StgBCO* bco = stgCast(StgBCO*,p);
	nat i;
	for (i = 0; i < bco->n_ptrs; i++) {
	  bcoConstCPtr(bco,i) = evacuate(bcoConstCPtr(bco,i));
	}
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

/* -----------------------------------------------------------------------------
   Reverting CAFs

   -------------------------------------------------------------------------- */

void RevertCAFs(void)
{
    while (enteredCAFs != END_CAF_LIST) {
	StgCAF* caf = enteredCAFs;
	const StgInfoTable *info = get_itbl(caf);

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
  StgBlackHole *bh;
  StgPtr stack_end;

  stack_end = &tso->stack[tso->stack_size];
  update_frame = tso->su;

  while (1) {
    switch (get_itbl(update_frame)->type) {

    case CATCH_FRAME:
      update_frame = stgCast(StgCatchFrame*,update_frame)->link;
      break;

    case UPDATE_FRAME:
      bh = stgCast(StgBlackHole*,update_frame->updatee);

      /* if the thunk is already blackholed, it means we've also
       * already blackholed the rest of the thunks on this stack,
       * so we can stop early.
       */

      /* Don't for now: when we enter a CAF, we create a black hole on
       * the heap and make the update frame point to it.  Thus the
       * above optimisation doesn't apply.
       */
      if (bh->header.info != &BLACKHOLE_info
	  && bh->header.info != &CAF_BLACKHOLE_info) {
	SET_INFO(bh,&BLACKHOLE_info);
	bh->blocking_queue = stgCast(StgTSO*,&END_TSO_QUEUE_closure);
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
   * Could stop if we find an update frame pointing to a black hole,
   * but see comment in threadLazyBlackHole().
   */
  
  next_frame = NULL;
  while ((P_)frame < bottom - 1) {  /* bottom - 1 is the STOP_FRAME */
    prev_frame = frame->link;
    frame->link = next_frame;
    next_frame = frame;
    frame = prev_frame;
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
      if (GET_INFO(updatee_bypass) == BLACKHOLE_info
	  || GET_INFO(updatee_bypass) == CAF_BLACKHOLE_info
	  ) {
	/* Sigh.  It has one.  Don't lose those threads! */
	if (GET_INFO(updatee_keep) == BLACKHOLE_info
	    || GET_INFO(updatee_keep) == CAF_BLACKHOLE_info
	    ) {
	  /* Urgh.  Two queues.  Merge them. */
	  P_ keep_tso = ((StgBlackHole *)updatee_keep)->blocking_queue;
	  
	  while (keep_tso->link != END_TSO_QUEUE) {
	    keep_tso = keep_tso->link;
	  }
	  keep_tso->link = ((StgBlackHole *)updatee_bypass)->blocking_queue;

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
	StgBlackHole *bh = (StgBlackHole *)frame->updatee;
	if (bh->header.info != &BLACKHOLE_info
	    && bh->header.info != &CAF_BLACKHOLE_info
	    ) {
	  SET_INFO(bh,&BLACKHOLE_info);
	  bh->blocking_queue = stgCast(StgTSO*,&END_TSO_QUEUE_closure);
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
