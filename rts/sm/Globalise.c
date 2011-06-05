/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2010
 *
 * Globalise data from the local heap to the global heap.
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Evac.h"
#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"
#include "GCUtils.h"
#include "Capability.h"
#include "Globalise.h"
#include "Prelude.h"
#include "Storage.h"
#include "Trace.h"
#include "Apply.h"
#include "Printer.h"
#include "Updates.h"
#include "MarkStack.h"

static REGPARM1 GNUC_ATTR_HOT void globalise_evac (StgClosure **p);
static void globalise_scavenge (void);
static void globalise_scavenge_TSO (StgTSO *tso);
STATIC_INLINE void globalise_large (StgPtr p);
static void globalise_large_bitmap (StgPtr p, StgLargeBitmap *large_bitmap,
                                    nat size);
static void globalise_maybe_record_mutable (StgClosure *q);
static void globalise_stack (StgPtr p, StgPtr stack_end);

void
globaliseWRT (Capability *cap USED_IF_THREADS, 
              StgClosure *parent, StgClosure **root)
{
    if (isGlobal(parent)) { 
        globalise(cap, root);
    }
}

void
globaliseFullWRT (Capability *cap USED_IF_THREADS,
                  StgClosure *parent, StgClosure **root)
{
    if (isGlobal(parent)) { 
        *root = globaliseFull_(cap, *root);
    }
}

void
globalise (Capability *cap, StgClosure **root)
{
    // very careful: we can't write the new pointer until the
    // structure is fully globalised, otherwise another processor
    // might follow the pointer and find local heap.
    *root = globalise_(cap, *root);
}    

StgClosure *
globalise_  (Capability *cap USED_IF_THREADS, StgClosure *p)
{
    gc_thread *saved_gct;
  
    // necessary if we stole a callee-saves register for gct:
    saved_gct = gct;
    SET_GCT(gc_threads[cap->no]);

    // No: we can get here via GarbageCollect(N=1)->resurrectThreads
    // ASSERT(gct->gc_type == GC_LOCAL);

    // prepare_gen_workspace(gen->ix), inlined:
    {
        gen_workspace *ws;
        ws = &gct->gens[global_gen_ix];
        ws->todo_bd->u.scan = ws->todo_free;
    }

    gct->globalise_thunks = rtsTrue;

    globalise_evac(&p);
    gct->globalise_thunks = rtsFalse;
    globalise_scavenge();

    ASSERT(!gct->failed_to_evac);

    SET_GCT(saved_gct);

    return p;
}

StgClosure *
globaliseFull_  (Capability *cap USED_IF_THREADS, StgClosure *p)
{
    gc_thread *saved_gct;
  
    // necessary if we stole a callee-saves register for gct:
    saved_gct = gct;
    SET_GCT(gc_threads[cap->no]);

    // No: we can get here via GarbageCollect(N=1)->resurrectThreads
    // ASSERT(gct->gc_type == GC_LOCAL);

    gct->globalise_thunks = rtsTrue;

    // prepare_gen_workspace(gen->ix), inlined:
    {
        gen_workspace *ws;
        ws = &gct->gens[global_gen_ix];
        ws->todo_bd->u.scan = ws->todo_free;
    }

    globalise_evac(&p);
    globalise_scavenge();

    ASSERT(!gct->failed_to_evac);

    SET_GCT(saved_gct);

    return p;
}

// Use globalise_TSO() if you want to migrate a thread from one
// Capability to another.  It ensures that all the fields of the TSO
// are published, whereas the ordinary globalise() only ensures that
// the StgTSO structure itself is published.

StgTSO *
globalise_TSO (Capability *cap USED_IF_THREADS, StgTSO *tso)
{
    gc_thread *saved_gct;
    StgStack *stack;
    StgUnderflowFrame *frame;

    // necessary if we stole a callee-saves register for gct:
    saved_gct = gct;
    SET_GCT(gc_threads[cap->no]);

    // No: we can get here via GarbageCollect(N=1)->resurrectThreads
    // ASSERT(gct->gc_type == GC_LOCAL);

    gct->globalise_thunks = rtsTrue; // probably a good idea, but not mandatory

    // prepare_gen_workspace(gen->ix), inlined:
    {
        gen_workspace *ws;
        ws = &gct->gens[global_gen_ix];
        ws->todo_bd->u.scan = ws->todo_free;
    }

    // Normally a TSO is considered to be a private object, and we
    // don't have to globalise its contents.  However,
    // in this case we want to globalise the complete contents because
    // we are about to transfer ownership of the TSO to another
    // Capability.

    // First, evacuate and scavenge the TSO object
    globalise_evac((StgClosure**)&tso);
    globalise_scavenge_TSO(tso);

    // globalise and scavenge all the stack chunks
    stack = tso->stackobj;
    while (1) {
        globalise_stack(stack->sp, stack->stack + stack->stack_size);
        frame = (StgUnderflowFrame*) (stack->stack + stack->stack_size
                                      - sizeofW(StgUnderflowFrame));
        if (frame->info != &stg_stack_underflow_frame_info
            || frame->next_chunk == (StgStack*)END_TSO_QUEUE) break;
        stack = frame->next_chunk;
    }

    // finish up globalising
    globalise_scavenge();

    // now we need to mark the TSO and all the STACKs as clean,
    // because if they migrate to another Capability then they need to
    // be added to that Capability's mutable list.
    tso->dirty = 0;
    stack = tso->stackobj;
    while (1) {
        stack->dirty = 0;
        frame = (StgUnderflowFrame*) (stack->stack + stack->stack_size
                                      - sizeofW(StgUnderflowFrame));
        if (frame->info != &stg_stack_underflow_frame_info
            || frame->next_chunk == (StgStack*)END_TSO_QUEUE) break;
        stack = frame->next_chunk;
    }

    if (gct->failed_to_evac) {
        barf("globalise: could not globalise the whole structure");
    }

    SET_GCT(saved_gct);
    return tso;
}


STATIC_INLINE void
globalise_large (StgPtr p)
{
    bdescr *bd;
    generation *gen, *new_gen;
    nat gen_ix;
    gen_workspace *ws;
    
    bd = Bdescr(p);
    gen_ix = bd->gen_ix;
    gen = bd->gen;

    ACQUIRE_SPIN_LOCK(&gen->sync);

    // remove from large_object list 
    if (bd->u.back) {
        bd->u.back->link = bd->link;
    } else { // first object in the list 
        gen->large_objects = bd->link;
    }
    if (bd->link) {
        bd->link->u.back = bd->u.back;
    }
    gen->n_large_blocks -= bd->blocks;
  
    // link it on to the evacuated large object list of the destination gen
    new_gen = &all_generations[global_gen_ix];
    ws = &gct->gens[global_gen_ix];

    bd->flags |= BF_EVACUATED;
    initBdescr(bd, new_gen, new_gen->to);

    // If this is a block of pinned objects, we don't have to scan
    // these objects, because they aren't allowed to contain any
    // pointers.  For these blocks, we skip the scavenge stage and put
    // them straight on the large_objects list.
    if (bd->flags & BF_PINNED) {
        ASSERT(get_itbl((StgClosure *)p)->type == ARR_WORDS);
        if (new_gen != gen) { ACQUIRE_SPIN_LOCK(&new_gen->sync); }
        dbl_link_onto(bd, &new_gen->large_objects);
        new_gen->n_large_blocks += bd->blocks;
        if (new_gen != gen) { RELEASE_SPIN_LOCK(&new_gen->sync); }
    } else {
        bd->link = ws->todo_large_objects;
        ws->todo_large_objects = bd;
    }

    RELEASE_SPIN_LOCK(&gen->sync);
}


STATIC_INLINE StgPtr
alloc_for_copy (nat size, nat gen_ix)
{
    StgPtr to;
    gen_workspace *ws;

    ws = &gct->gens[gen_ix];
    
    /* chain a new block onto the to-space for the destination gen if
     * necessary.
     */
    to = ws->todo_free;
    ws->todo_free += size;
    if (ws->todo_free > ws->todo_lim) {
	to = todo_block_full(size, ws);
    }
    ASSERT(ws->todo_free >= ws->todo_bd->free && ws->todo_free <= ws->todo_lim);

    return to;
}

STATIC_INLINE StgPtr
alloc_for_copy_global (nat size)
{
    return alloc_for_copy(size, global_gen_ix);
}

STATIC_INLINE GNUC_ATTR_HOT void
copy_closure(StgPtr from, StgPtr to, const StgInfoTable *info, nat size)
{
    nat i;

    to[0] = (W_)info;
    for (i = 1; i < size; i++) { // unroll for small i
	to[i] = from[i];
    }
}

STATIC_INLINE GNUC_ATTR_HOT void
copy_tag(StgClosure **p, const StgInfoTable *info, 
         StgClosure *src, nat size, StgWord tag)
{
    StgPtr to;

    // debugTrace(DEBUG_gc, "globalising value at *%p = %p", p, src);
    to = alloc_for_copy_global(size);
    copy_closure((StgPtr)src, to, info, size);
    src->header.info = (const StgInfoTable *)MK_FORWARDING_PTR(to);
    *p = TAG_CLOSURE(tag,(StgClosure*)to);
}

STATIC_INLINE GNUC_ATTR_HOT void
copy_IND(StgClosure **p, const StgInfoTable *info, 
         StgClosure *src, nat size)
{
    StgPtr to;
    
    to = alloc_for_copy_global(size);
    copy_closure((StgPtr)src, to, info, size);
#ifdef DEBUG
    zeroSlop((StgPtr)src + sizeofW(StgInd), size - sizeofW(StgInd));
#endif
    src->header.info = &stg_IND_info;
    ((StgInd *)src)->indirectee = (StgClosure *)to;
    *p = (StgClosure*)to;
}

STATIC_INLINE  GNUC_ATTR_HOT void
nocopy_IND_LOCAL(bdescr *bd, StgClosure **p, StgClosure *src)
{
    StgPtr to = alloc_for_copy_global(sizeofW(StgInd));
    ((StgInd *)to)->header.info = stg_IND_LOCAL_tbl[bd->gen_ix];
    ((StgInd *)to)->indirectee  = src;
    *p = (StgClosure*)to;
}


REGPARM1 GNUC_ATTR_HOT void 
globalise_evac (StgClosure **p)
{
  bdescr *bd = NULL;
  StgClosure *q;
  const StgInfoTable *info;
  StgWord tag;

  q = *p;

loop:
  /* The tag and the pointer are split, to be merged after evacing */
  tag = GET_CLOSURE_TAG(q);
  q = UNTAG_CLOSURE(q);

  ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

  if (!HEAP_ALLOCED(q)) return; // already global

  bd = Bdescr((P_)q);

  if (bd->gen_no > 0) return;

  // make sure this isn't a pointer into the wrong local heap
  // NB. we might be doing a global GC, and got here via
  // globalise_mut_lists, in which case it won't necessarily be our
  // local heap.
//  ASSERT(gct->gc_type != GC_LOCAL || isGlobal(q) || bd->gen_ix == gct->index);

  if (bd->flags & BF_LARGE) {
      globalise_large((P_)q);
      return;
  }

  if (bd->flags & BF_PRIM) {
      if (!isGlobalPrim(q)) {
          setGlobal(q);
          push_mark_stack(q);
      }
      return;
  }

  info = q->header.info;
  if (IS_FORWARDING_PTR(info))
  {
      StgClosure *e = (StgClosure*)UN_FORWARDING_PTR(info);
      *p = TAG_CLOSURE(tag,e);
      return;
  }

  switch (INFO_PTR_TO_STRUCT(info)->type) {

  case WHITEHOLE:
      goto loop;

  // For ints and chars of low value, save space by replacing references to
  //	these with closures with references to common, shared ones in the RTS.
  //
  // * Except when compiling into Windows DLLs which don't support cross-package
  //	data references very well.
  //
  case CONSTR_0_1:
  {   
#if defined(__PIC__) && defined(mingw32_HOST_OS) 
      copy_tag(p,info,q,sizeofW(StgHeader)+1,tag);
#else
      StgWord w = (StgWord)q->payload[0];
      if (info == Czh_con_info &&
	  // unsigned, so always true:  (StgChar)w >= MIN_CHARLIKE &&  
	  (StgChar)w <= MAX_CHARLIKE) {
	  *p =  TAG_CLOSURE(tag,
                            (StgClosure *)CHARLIKE_CLOSURE((StgChar)w)
			   );
      }
      else if (info == Izh_con_info &&
	  (StgInt)w >= MIN_INTLIKE && (StgInt)w <= MAX_INTLIKE) {
	  *p = TAG_CLOSURE(tag,
			     (StgClosure *)INTLIKE_CLOSURE((StgInt)w)
			     );
      }
      else {
          copy_tag(p,info,q,sizeofW(StgHeader)+1,tag);
      }
#endif
      return;
  }

  case FUN_0_1:
  case FUN_1_0:
  case CONSTR_1_0:
      copy_tag(p,info,q,sizeofW(StgHeader)+1,tag);
      return;

  case THUNK_1_0:
  case THUNK_0_1:
      if (gct->globalise_thunks) {
          copy_IND(p,info,q,sizeofW(StgThunk)+1);
      } else {
          nocopy_IND_LOCAL(bd, p, q);
      }
      return;

  case IND_PERM:
      copy_IND(p,info,q,sizeofW(StgThunk)+1);
      return;

  case THUNK_1_1:
  case THUNK_2_0:
  case THUNK_0_2:
      if (gct->globalise_thunks) {
          copy_IND(p,info,q,sizeofW(StgThunk)+2);
      } else {
          nocopy_IND_LOCAL(bd, p, q);
      }
      return;

  case FUN_1_1:
  case FUN_2_0:
  case FUN_0_2:
  case CONSTR_1_1:
  case CONSTR_2_0:
  case CONSTR_0_2:
      copy_tag(p,info,q,sizeofW(StgHeader)+2,tag);
      return;

  case THUNK:
      if (gct->globalise_thunks) {
          copy_IND(p,info,q,thunk_sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)));
      } else {
          nocopy_IND_LOCAL(bd, p, q);
      }
      return;

  case FUN:
  case CONSTR:
      copy_tag(p,info,q,sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),tag);
      return;

  case THUNK_SELECTOR:
      if (gct->globalise_thunks) {
          copy_IND(p,info,q,THUNK_SELECTOR_sizeW());
      } else {
          nocopy_IND_LOCAL(bd, p, q);
      }
      return;

  case IND:
      // follow chains of indirections, don't evacuate them 
      q = ((StgInd*)q)->indirectee;
      *p = q;
      goto loop;

  case PAP:
      copy_IND(p,info,q,pap_sizeW((StgPAP*)q));
      return;

  case AP:
      if (gct->globalise_thunks) {
          copy_IND(p,info,q,ap_sizeW((StgAP*)q));
      } else {
          nocopy_IND_LOCAL(bd, p, q);
      }
      return;

  case AP_STACK:
      if (gct->globalise_thunks) {
          copy_IND(p,info,q,ap_stack_sizeW((StgAP_STACK*)q));
      } else {
          nocopy_IND_LOCAL(bd, p, q);
      }
      return;

  case BLACKHOLE:
      // don't promote: we can't move BLACKHOLEs, because the update
      // frame points to them, so leave an IND_LOCAL instead
  {
      StgClosure *r;
      const StgInfoTable *i;

      r = ((StgInd*)q)->indirectee;
      if (GET_CLOSURE_TAG(r) == 0) {
          i = r->header.info;
          ASSERT(!IS_FORWARDING_PTR(i)); // these things can't be
                                         // forwarding ptrs, outside of GC.
          if (i == &stg_TSO_info
              || i == &stg_WHITEHOLE_info 
              || i == &stg_BLOCKING_QUEUE_CLEAN_info
              || i == &stg_BLOCKING_QUEUE_DIRTY_info)
          {
              nocopy_IND_LOCAL(bd, p, q);
              return;
          }
          ASSERT(i != &stg_STUB_BLOCKING_QUEUE_info);
      }
      q = r;
      *p = r;
      goto loop;
  }

  case ARR_WORDS:
      // just copy the block 
      copy_tag(p,info,q,arr_words_sizeW((StgArrWords *)q),tag);
      return;

  default:
    barf("globalise_evac: strange closure type %d", (int)(INFO_PTR_TO_STRUCT(info)->type));
  }

  barf("globalise_evac");
}
    
/* -----------------------------------------------------------------------------
   Blocks of function args occur on the stack (at the top) and
   in PAPs.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
globalise_arg_block (StgFunInfoTable *fun_info, StgClosure **args)
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
	globalise_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
	size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		globalise_evac((StgClosure **)p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
globalise_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size)
{
    StgPtr p;
    StgWord bitmap;
    StgFunInfoTable *fun_info;
    
    fun_info = get_fun_itbl(UNTAG_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
	bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
	goto small_bitmap;
    case ARG_GEN_BIG:
	globalise_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
	p += size;
	break;
    case ARG_BCO:
	globalise_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
	p += size;
	break;
    default:
	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
	while (size > 0) {
	    if ((bitmap & 1) == 0) {
		globalise_evac((StgClosure **)p);
	    }
	    p++;
	    bitmap = bitmap >> 1;
	    size--;
	}
	break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
globalise_PAP (StgPAP *pap)
{
    globalise_evac(&pap->fun);
    return globalise_PAP_payload (pap->fun, pap->payload, pap->n_args);
}

STATIC_INLINE StgPtr
globalise_AP (StgAP *ap)
{
    globalise_evac(&ap->fun);
    return globalise_PAP_payload (ap->fun, ap->payload, ap->n_args);
}

/* -----------------------------------------------------------------------------
   scavenge a chunk of memory described by a bitmap
   -------------------------------------------------------------------------- */

static void
globalise_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, nat size )
{
    nat i, b;
    StgWord bitmap;
    
    b = 0;
    bitmap = large_bitmap->bitmap[b];
    for (i = 0; i < size; ) {
	if ((bitmap & 1) == 0) {
	    globalise_evac((StgClosure **)p);
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
globalise_small_bitmap (StgPtr p, nat size, StgWord bitmap)
{
    while (size > 0) {
	if ((bitmap & 1) == 0) {
	    globalise_evac((StgClosure **)p);
	}
	p++;
	bitmap = bitmap >> 1;
	size--;
    }
    return p;
}

/* -----------------------------------------------------------------------------
   globalise_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   AP_STACK_UPDs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */

static void
globalise_stack(StgPtr p, StgPtr stack_end)
{
  const StgRetInfoTable* info;
  StgWord bitmap;
  nat size;

  /* 
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with an activation record. 
   */
  while (p < stack_end) {
    info  = get_ret_itbl((StgClosure *)p);
      
    switch (info->i.type) {
	
    case UPDATE_FRAME:
    {
        nat type;
        const StgInfoTable *i;
        StgClosure *updatee;

        // see comment about UPDATE_FRAME in scavenge_stack(), which
        // also applies here.
        updatee = ((StgUpdateFrame *)p)->updatee;
        i = updatee->header.info;
        ASSERT(!IS_FORWARDING_PTR(i));
        type = INFO_PTR_TO_STRUCT(i)->type;
        switch (type) {
            // globalise_evac() won't move a BLACKHOLE, but we really
            // want to move it here since we're globalising the thread
            // that points to it.
        case BLACKHOLE:
            if (Bdescr((P_)updatee)->gen_ix < global_gen_ix) {
                copy_IND(&((StgUpdateFrame *)p)->updatee,i,updatee,
                         BLACKHOLE_sizeW());
            }
            break;

        default:
            barf("globalise_stack: %d", type);
        }
        ASSERT(GET_CLOSURE_TAG(((StgUpdateFrame *)p)->updatee) == 0);
        p += sizeofW(StgUpdateFrame);
        continue;
    }

      // small bitmap (< 32 entries, or 64 on a 64-bit machine) 
    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
    case STOP_FRAME:
    case UNDERFLOW_FRAME:
    case CATCH_FRAME:
    case RET_SMALL:
	bitmap = BITMAP_BITS(info->i.layout.bitmap);
	size   = BITMAP_SIZE(info->i.layout.bitmap);
	// NOTE: the payload starts immediately after the info-ptr, we
	// don't have an StgHeader in the same sense as a heap closure.
	p++;
	p = globalise_small_bitmap(p, size, bitmap);
	continue;

    case RET_BCO: {
	StgBCO *bco;
	nat size;

	p++;
	globalise_evac((StgClosure **)p);
	bco = (StgBCO *)*p;
	p++;
	size = BCO_BITMAP_SIZE(bco);
	globalise_large_bitmap(p, BCO_BITMAP(bco), size);
	p += size;
	continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine) 
    case RET_BIG:
    {
	nat size;

	size = GET_LARGE_BITMAP(&info->i)->size;
	p++;
	globalise_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
	p += size;
        continue;
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
	p = globalise_small_bitmap(p, size, bitmap);

	// skip over the non-ptr words
	p += RET_DYN_NONPTRS(dyn) + RET_DYN_NONPTR_REGS_SIZE;
	
	// follow the ptr words
	for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
	    globalise_evac((StgClosure **)p);
	    p++;
	}
	continue;
    }

    case RET_FUN:
    {
	StgRetFun *ret_fun = (StgRetFun *)p;
	StgFunInfoTable *fun_info;

	globalise_evac(&ret_fun->fun);
 	fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
	p = globalise_arg_block(fun_info, ret_fun->payload);
        continue;
    }

    default:
	barf("globalise_stack: weird activation record found on stack: %d", (int)(info->i.type));
    }
  }		     
}

static void
globalise_scavenge_TSO (StgTSO *tso)
{
    debugTrace(DEBUG_gc,"globalising thread %d",(int)tso->id);

    // update the pointer from the Task.
    if (tso->bound != NULL) {
        tso->bound->tso = tso;
    }
        
    if (   tso->why_blocked == BlockedOnMVar
           || tso->why_blocked == BlockedOnBlackHole
           || tso->why_blocked == BlockedOnMsgThrowTo
           || tso->why_blocked == NotBlocked
        ) {
        globalise_evac(&tso->block_info.closure);
    }
    globalise_evac((StgClosure **)&tso->blocked_exceptions);
    globalise_evac((StgClosure **)&tso->bq);
        
    // scavange current transaction record
    globalise_evac((StgClosure **)&tso->trec);
        
    globalise_evac((StgClosure **)&tso->stackobj);
        
    globalise_evac((StgClosure **)&tso->_link);
    if (   tso->why_blocked == BlockedOnMVar
           || tso->why_blocked == BlockedOnBlackHole
           || tso->why_blocked == BlockedOnMsgThrowTo
           || tso->why_blocked == NotBlocked
        ) {
        globalise_evac(&tso->block_info.closure);
    }
        
    ASSERT(!gct->failed_to_evac);
    tso->dirty = 0;
}


static StgPtr globalise_scavenge_mut_arr_ptrs (StgMutArrPtrs *a)
{
    StgPtr p;

    for (p = (P_)&a->payload[0]; p < (P_)&a->payload[a->ptrs]; p++) {
        globalise_evac((StgClosure**)p);
    }

    return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}


static StgPtr
globalise_scavenge_prim (StgPtr p, nat type)
{
    switch (type) {

    case ARR_WORDS:
        // nothing to do; these can happen if a large ARR_WORDS gets
        // globalised.
	return p += arr_words_sizeW((StgArrWords *)p);

    case MUT_ARR_PTRS_LOCAL:
        SET_INFO((StgMutArrPtrs *)p, &stg_MUT_ARR_PTRS_GLOBAL_info);
        // fall through
    case MUT_ARR_PTRS_GLOBAL:
        return globalise_scavenge_mut_arr_ptrs((StgMutArrPtrs *)p);

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
        return globalise_scavenge_mut_arr_ptrs((StgMutArrPtrs *)p);

    default:
	barf("globalise_scavenge_one: unimplemented/strange closure type %d @ %p", 
	     type, p);
    }
}

static GNUC_ATTR_HOT REGPARM1 StgPtr
globalise_scavenge_one (StgPtr p)
{
    const StgInfoTable *info;

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);

    switch (info->type) {

    case FUN_2_0:
	globalise_evac(&((StgClosure *)p)->payload[1]);
	globalise_evac(&((StgClosure *)p)->payload[0]);
        return p + sizeofW(StgHeader) + 2;

    case THUNK_2_0:
	globalise_evac(&((StgThunk *)p)->payload[1]);
	globalise_evac(&((StgThunk *)p)->payload[0]);
	return p + sizeofW(StgThunk) + 2;

    case CONSTR_2_0:
	globalise_evac(&((StgClosure *)p)->payload[1]);
	globalise_evac(&((StgClosure *)p)->payload[0]);
	return p + sizeofW(StgHeader) + 2;
	
    case THUNK_1_0:
	globalise_evac(&((StgThunk *)p)->payload[0]);
	return p + sizeofW(StgThunk) + 1;
	
    case FUN_1_0:
    case CONSTR_1_0:
	globalise_evac(&((StgClosure *)p)->payload[0]);
	return p + sizeofW(StgHeader) + 1;
	
    case THUNK_0_1:
	return p + sizeofW(StgThunk) + 1;
	
    case FUN_0_1:
    case CONSTR_0_1:
	return p + sizeofW(StgHeader) + 1;

    case THUNK_0_2:
	return p + sizeofW(StgThunk) + 2;
	
    case FUN_0_2:
    case CONSTR_0_2:
	return p + sizeofW(StgHeader) + 2;
	
    case THUNK_1_1:
	globalise_evac(&((StgThunk *)p)->payload[0]);
	return p + sizeofW(StgThunk) + 2;

    case FUN_1_1:
    case CONSTR_1_1:
	globalise_evac(&((StgClosure *)p)->payload[0]);
	return p + sizeofW(StgHeader) + 2;
	
    case THUNK:
    {
	StgPtr end;

	end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
	    globalise_evac((StgClosure **)p);
	}
	return p + info->layout.payload.nptrs;
    }
	
    case MUT_VAR_LOCAL:
        SET_INFO((StgMutVar*)p, &stg_MUT_VAR_GLOBAL_info);
        // fall through
    case FUN:
    case CONSTR:
    case BLACKHOLE:
    case PRIM:
    case MUT_PRIM:
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    case MUT_VAR_GLOBAL:
    case TREC_CHUNK:
    {
	StgPtr end;

	end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
	for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
	    globalise_evac((StgClosure **)p);
	}
	return p + info->layout.payload.nptrs;
    }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue*)p;

        globalise_evac((StgClosure **)&bq->owner);
        globalise_evac((StgClosure **)&bq->bh);
        globalise_evac((StgClosure **)&bq->owner);
        // no need to globalise bq->queue; that is private

        if (get_itbl(bq->bh)->type == IND_LOCAL)
        {
            // the BLACKHOLE is in a local heap.  We better update the
            // BLOCKING_QUEUE to point directly to the BLACKHOLE; this
            // is ok, because BLOCKING_QUEUE is a private object
            // (except for the bq->tso pointer, which other
            // Capabilities might read).
            //
            // If we are migrating the owner, then the stack will have
            // been globalised first, which will have moved the
            // BLACKHOLE into the global heap, so we won't encounter
            // this problem.
            bq->bh = ((StgInd*)bq->bh)->indirectee;
        }

        return p + sizeofW(StgBlockingQueue);
    }

    case WEAK:
    {
        StgWeak *w = (StgWeak *)p;
        globalise_evac(&w->cfinalizer);
        globalise_evac(&w->key);
        globalise_evac(&w->value);
        globalise_evac(&w->finalizer);
        // link field already points into this gen
        return p + sizeofW(StgWeak);
    }        

    case THUNK_SELECTOR:
    { 
	StgSelector *s = (StgSelector *)p;
	globalise_evac(&s->selectee);
	return p + THUNK_SELECTOR_sizeW();
    }

    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
	StgAP_STACK *ap = (StgAP_STACK *)p;

	globalise_evac(&ap->fun);
	globalise_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
	return (StgPtr)ap->payload + ap->size;
    }

    case PAP:
	return globalise_PAP((StgPAP *)p);

    case AP:
	return globalise_AP((StgAP *)p);

    case TSO:
    {
        StgTSO *tso = (StgTSO*)p;
        // we don't touch the insides of a TSO, it is private
        tso->dirty = 1;
        gct->failed_to_evac = rtsTrue;
        return p + sizeofW(StgTSO);
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;
        // we don't touch the insides of a STACK, it is private
        globalise_evac((StgClosure**)&stack->tso);
        stack->dirty = 1;
        gct->failed_to_evac = rtsTrue;
        return p + stack_sizeW(stack);
    }

    // an IND_LOCAL could be here as a result of a BLACKHOLE being
    // globalised, so we don't want to try to globalise it.
    case IND_LOCAL:
        gct->failed_to_evac = rtsTrue;
        return p + sizeofW(StgInd);

    case IND:
    case IND_STATIC:
        globalise_evac(&((StgInd*)p)->indirectee);
        return p + sizeofW(StgInd);

    default:
        return globalise_scavenge_prim(p, info->type);
    }
}


static void
globalise_scavenge_large (gen_workspace *ws)
{
    bdescr *bd;
    StgPtr p;

    bd = ws->todo_large_objects;
    
    for (; bd != NULL; bd = ws->todo_large_objects) {
	
	// take this object *off* the large objects list and put it on
	// the scavenged large objects list.  This is so that we can
	// treat new_large_objects as a stack and push new objects on
	// the front when evacuating.
	ws->todo_large_objects = bd->link;
	
        ACQUIRE_SPIN_LOCK(&ws->gen->sync);
	dbl_link_onto(bd, &ws->gen->large_objects);
	ws->gen->n_large_blocks += bd->blocks;
        RELEASE_SPIN_LOCK(&ws->gen->sync);
	
	p = bd->start;
	globalise_scavenge_one(p);
        if (gct->failed_to_evac) {
            gct->failed_to_evac = rtsFalse;
            globalise_maybe_record_mutable((StgClosure*)p);
        }

        // stats
        gct->scanned += closure_sizeW((StgClosure*)p);
    }
}

static GNUC_ATTR_HOT void
globalise_scavenge_block (bdescr *bd)
{
    StgPtr p, q;
    gen_workspace *ws;
    
    debugTrace(DEBUG_gc, "globalise: scavenging block %p (gen %d) @ %p",
               bd->start, bd->gen_no, bd->u.scan);
    
    gct->scan_bd = bd;
    ws = &gct->gens[bd->gen_ix];
    
    p = bd->u.scan;
    
    // we might be evacuating into the very block that we're
    // scavenging, so we have to check the real bd->free pointer each
    // time around the loop.
    while (p < bd->free || (bd == ws->todo_bd && p < ws->todo_free)) {
        ASSERT(bd->link == NULL);
        q = globalise_scavenge_one(p);
      
        if (gct->failed_to_evac) {
            gct->failed_to_evac = rtsFalse;
            globalise_maybe_record_mutable((StgClosure*)p);
        }

        p = q;
    }
    
    if (p > bd->free)  {
        bd->free = p;
    }
    
    debugTrace(DEBUG_gc, "   scavenged %ld bytes",
               (unsigned long)((bd->free - bd->u.scan) * sizeof(W_)));
    
    // update stats: this is a block that has been scavenged
    gct->globalised += bd->free - bd->u.scan;
    bd->u.scan = bd->free;
    
    if (bd != ws->todo_bd) {
        // we're not going to evac any more objects into
        // this block, so push it now.
        push_scanned_block(bd, ws);
    }
    
    gct->scan_bd = NULL;
}
    

static void
globalise_scavenge (void)
{
    gen_workspace *ws;
    bdescr *bd;
    StgPtr p;

    do {
        ws = &gct->gens[global_gen_ix]; // XXX assumes structure of generations
        
        gct->scan_bd = NULL;
        
        // scavenge objects in compacted generation
        while ((p = pop_mark_stack())) {
            globalise_scavenge_one(p);
            if (gct->failed_to_evac) {
                gct->failed_to_evac = rtsFalse;
                globalise_maybe_record_mutable((StgClosure*)p);
            }
        }

        // If we have a scan block with some work to do,
        // scavenge everything up to the free pointer.
        if (ws->todo_bd->u.scan < ws->todo_free)
        {
            globalise_scavenge_block(ws->todo_bd);
            continue;
        }
        
        // If we have any large objects to scavenge, do them now.
        if (ws->todo_large_objects) {
            globalise_scavenge_large(ws);
            continue;
        }
        
        if ((bd = grab_local_todo_block(ws)) != NULL) {
            globalise_scavenge_block(bd);
            continue;
        }
        
        return;

    } while (1);
}

/* -----------------------------------------------------------------------------
   Mutable lists
   -------------------------------------------------------------------------- */

static void
globalise_maybe_record_mutable (StgClosure *q)
{
    nat g = global_gen_no;

    switch (get_itbl((StgClosure*)q)->type) {

    case TSO:
        // put it on the correct mutable list; during parallel
        // GC it may have ended up on the wrong one.
        ASSERT(gct->gc_type == GC_LOCAL ?
               ((StgTSO*)q)->cap == gct->cap : 1);
        recordMutableCap(((StgTSO*)q)->cap, q, g);
        break;

    case STACK:
        // c.f. TSO
        // this is the entire reason for the tso field of STACK
        ASSERT(gct->gc_type == GC_LOCAL ?
               ((StgStack*)q)->tso->cap == gct->cap : 1);
        recordMutableCap(((StgStack*)q)->tso->cap, q, g);
        break;

    case IND_LOCAL:
        // put it on the correct mutable list; during parallel
        // GC it may have ended up on the wrong one.
        recordMutableCap(&capabilities[get_itbl(q)->srt_bitmap],
                         q, g);
        break;

    default:
        barf("globalise_maybe_record_mutable: %d", get_itbl(q)->type);
    }
}

static void
globalise_mut_list (Capability *cap STG_UNUSED, bdescr *mut_list, 
                    nat g STG_UNUSED)
{
    bdescr *bd;
    StgPtr p;
    StgClosure *q;
  
    // if g != 1, then we'll need to do something more clever below
    ASSERT(g == 1);

    gct->failed_to_evac = rtsFalse;

    for (bd = mut_list; bd != NULL; bd = bd->link) {

        for (p = bd->start; p < bd->free; p++) {
            
            q = (StgClosure *)*p;
            
            // globalise_scavenge_one() will ignore an IND_LOCAL,
            // because they are created by globalise_evac() on a
            // BLACKHOLE. But we can scavenge any IND_LOCALs on the
            // mut_list here without going into a loop.
            switch (get_itbl((StgClosure*)q)->type) {
//            case IND_LOCAL:
//                globalise_evac(&((StgInd*)q)->indirectee);
//                // note: we can't change it to an IND at this stage,
//                // because we haven't finished scavenging the
//                // transitive closure yet.  A later stage will have to
//                // do that.
//                break;
            default:
                globalise_scavenge_one((StgPtr)q);
                break;
            }

            if (gct->failed_to_evac) {
                gct->failed_to_evac = rtsFalse;
                globalise_maybe_record_mutable(q);
            }

        }
    }
}


// called after GC to clean up any global->local pointers that arose.
// c.f. scavenge_capability_mut_lists()
void
globalise_capability_mut_lists (Capability *cap)
{
    nat g;

    gct->globalise_thunks = rtsFalse;

    for (g = RtsFlags.GcFlags.generations-1; g > 0; g--) {
        globalise_mut_list(cap, cap->saved_mut_lists[g], g);
        freeChain_sync(cap->saved_mut_lists[g]);
        cap->saved_mut_lists[g] = NULL;
    }

    globalise_scavenge();
}

/* -----------------------------------------------------------------------------
   Publishing
   -------------------------------------------------------------------------- */

StgClosure *
publish (Capability *cap, StgClosure *p)
{
    StgInd *i;

    if (!HEAP_ALLOCED(p) || isGlobal(p)) return p;

    // ToDo: perhaps want to check what kind of object p is; for a
    // simple value with no pointers we could just globalise
    // it.

    // ToDo: inline part of allocateInGen, like alloc_for_copy
    i = (StgInd*) allocateInGen(cap, global_gen_ix, sizeofW(StgInd));
    SET_HDR(i, stg_IND_LOCAL_tbl[cap->no], CCS_SYSTEM);
    i->indirectee = p;
    recordMutableCap(cap,(StgClosure*)i,global_gen_no);
    return (StgClosure *)i;
}

StgClosure *
publish_gen (Capability *cap, StgClosure *p, generation *gen)
{
    StgInd *i;

    // ToDo: inline part of allocateInGen, like alloc_for_copy
    i = (StgInd*) allocateInGen(cap, gen->ix, sizeofW(StgInd));
    SET_HDR(i, stg_IND_LOCAL_tbl[cap->no], CCS_SYSTEM);
    i->indirectee = p;
    recordMutableCap(cap,(StgClosure*)i,gen->no);
    return (StgClosure *)i;
}
