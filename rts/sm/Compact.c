/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2008
 *
 * Compacting garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "GCThread.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "GC.h"
#include "Compact.h"
#include "Apply.h"
#include "Trace.h"
#include "Weak.h"
#include "MarkWeak.h"
#include "StablePtr.h"
#include "StableName.h"
#include "Hash.h"

// Turn off inlining when debugging - it obfuscates things
#if defined(DEBUG)
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

/* ----------------------------------------------------------------------------
   Threading / unthreading pointers.

   The basic idea here is to chain together all the fields pointing at a
   particular object, with the root of the chain in the object's info table
   field.  The original contents of the info pointer goes at the end of the
   chain.

   Adding a new field to the chain is a matter of swapping the contents of the
   field with the contents of the object's info table field:

       *field, **field = **field, field

   To unthread the chain, we walk down it updating all the fields on the chain
   with the new location of the object.  We stop when we reach the info pointer
   at the end.

   The main difficulty here is that not all pointers to the same object are
   tagged: pointers from roots (e.g. mut_lists) are not tagged, but pointers
   from mutators are. So when unthreading a chain we need to distinguish a field
   that had a tagged pointer from a field that had an untagged pointer.

    Our solution is as follows: when chaining a field, if the field is NOT
    tagged then we tag the pointer to the field with 1. I.e.

        *field, **field = **field, field + 1

    If the field is tagged then we tag to the pointer to it with 2.

    When unchaining we look at the tag in the pointer to the field, if it's 1
    then we write an untagged pointer to "free" to it, otherwise we tag the
    pointer.
   ------------------------------------------------------------------------- */

static /* STATIC_INLINE */ P_
thread_obj (const StgInfoTable *info, P_ p);


STATIC_INLINE W_
UNTAG_PTR(W_ p)
{
    return p & ~TAG_MASK;
}

STATIC_INLINE W_
GET_PTR_TAG(W_ p)
{
    return p & TAG_MASK;
}

static W_
get_iptr_tag(StgInfoTable *iptr)
{
    const StgInfoTable *info = INFO_PTR_TO_STRUCT(iptr);
    switch (info->type) {
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_NOCAF:
    {
        W_ con_tag = info->srt + 1;
        if (con_tag > TAG_MASK) {
            return TAG_MASK;
        } else {
            return con_tag;
        }
    }

    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_2_0:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_STATIC:
    {
        const StgFunInfoTable *fun_itbl = FUN_INFO_PTR_TO_STRUCT(iptr);
        W_ arity = fun_itbl->f.arity;
        if (arity <= TAG_MASK) {
            return arity;
        } else {
            return 0;
        }
    }

    default:
        return 0;
    }
}

STATIC_INLINE void
thread (StgClosure **p)
{
    StgClosure *q0  = *p;
    bool q0_tagged = GET_CLOSURE_TAG(q0) != 0;
    P_ q = (P_)UNTAG_CLOSURE(q0);

    // It doesn't look like a closure at the moment, because the info
    // ptr is possibly threaded:
    // ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

    if (HEAP_ALLOCED(q)) {
        bdescr *bd = Bdescr(q);

        if (bd->flags & BF_MARKED)
        {
            W_ iptr = *q;
            *p = (StgClosure *)iptr;
            *q = (W_)p + 1 + (q0_tagged ? 1 : 0);
        }
    }
}

static void
thread_root (void *user STG_UNUSED, StgClosure **p)
{
    thread(p);
}

// This version of thread() takes a (void *), used to circumvent
// warnings from gcc about pointer punning and strict aliasing.
STATIC_INLINE void thread_ (void *p) { thread((StgClosure **)p); }

STATIC_INLINE void
unthread( const P_ p, W_ free, W_ tag )
{
    W_ q = *p;
loop:
    switch (GET_PTR_TAG(q))
    {
    case 0:
        // nothing to do; the chain is length zero
        *p = q;
        return;
    case 1:
    {
        P_ q0 = (P_)(q-1);
        W_ r = *q0;
        *q0 = free;
        q = r;
        goto loop;
    }
    case 2:
    {
        P_ q0 = (P_)(q-2);
        W_ r = *q0;
        *q0 = free + tag;
        q = r;
        goto loop;
    }
    default:
        barf("unthread");
    }
}

// Traverse a threaded chain and pull out the info pointer at the end.
// The info pointer is also tagged with the appropriate pointer tag
// for this closure, which should be attached to the pointer
// subsequently passed to unthread().
STATIC_INLINE StgInfoTable*
get_threaded_info( P_ p )
{
    W_ q = (W_)GET_INFO(UNTAG_CLOSURE((StgClosure *)p));

loop:
    switch (GET_PTR_TAG(q))
    {
    case 0:
        ASSERT(LOOKS_LIKE_INFO_PTR(q));
        return (StgInfoTable*)q;
    case 1:
    case 2:
    {
        q = *(P_)(UNTAG_PTR(q));
        goto loop;
    }
    default:
        barf("get_threaded_info");
    }
}

// A word-aligned memmove will be faster for small objects than libc's or gcc's.
// Remember, the two regions *might* overlap, but: to <= from.
STATIC_INLINE void
move(P_ to, P_ from, W_ size)
{
    for(; size > 0; --size) {
        *to++ = *from++;
    }
}

static void
thread_static( StgClosure* p )
{
  // keep going until we've threaded all the objects on the linked
  // list...
  while (p != END_OF_STATIC_OBJECT_LIST) {
    p = UNTAG_STATIC_LIST_PTR(p);
    const StgInfoTable *info = get_itbl(p);
    switch (info->type) {

    case IND_STATIC:
        thread(&((StgInd *)p)->indirectee);
        p = *IND_STATIC_LINK(p);
        continue;

    case THUNK_STATIC:
        p = *THUNK_STATIC_LINK(p);
        continue;
    case FUN_STATIC:
        p = *STATIC_LINK(info,p);
        continue;
    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
        p = *STATIC_LINK(info,p);
        continue;

    default:
        barf("thread_static: strange closure %d", (int)(info->type));
    }

  }
}

STATIC_INLINE void
thread_large_bitmap( P_ p, StgLargeBitmap *large_bitmap, W_ size )
{
    W_ b = 0;
    W_ bitmap = large_bitmap->bitmap[b];
    for (W_ i = 0; i < size; ) {
        if ((bitmap & 1) == 0) {
            thread((StgClosure **)p);
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

STATIC_INLINE P_
thread_small_bitmap (P_ p, W_ size, W_ bitmap)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            thread((StgClosure **)p);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

STATIC_INLINE P_
thread_arg_block (StgFunInfoTable *fun_info, StgClosure **args)
{
    W_ bitmap;
    W_ size;

    P_ p = (P_)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        thread_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = thread_small_bitmap(p, size, bitmap);
        break;
    }
    return p;
}

static void
thread_stack(P_ p, P_ stack_end)
{
    // highly similar to scavenge_stack, but we do pointer threading here.

    while (p < stack_end) {

        // *p must be the info pointer of an activation
        // record.  All activation records have 'bitmap' style layout
        // info.
        //
        const StgRetInfoTable *info  = get_ret_itbl((StgClosure *)p);

        switch (info->i.type) {

            // small bitmap (<= 32 entries, or 64 on a 64-bit machine)
        case CATCH_RETRY_FRAME:
        case CATCH_STM_FRAME:
        case ATOMICALLY_FRAME:
        case UPDATE_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case CATCH_FRAME:
        case RET_SMALL:
        case ANN_FRAME:
        {
            W_ bitmap = BITMAP_BITS(info->i.layout.bitmap);
            W_ size   = BITMAP_SIZE(info->i.layout.bitmap);
            p++;
            // NOTE: the payload starts immediately after the info-ptr, we
            // don't have an StgHeader in the same sense as a heap closure.
            p = thread_small_bitmap(p, size, bitmap);
            continue;
        }

        case RET_BCO: {
            p++;
            StgBCO *bco = (StgBCO *)*p;
            thread((StgClosure **)p);
            p++;
            W_ size = BCO_BITMAP_SIZE(bco);
            thread_large_bitmap(p, BCO_BITMAP(bco), size);
            p += size;
            continue;
        }

            // large bitmap (> 32 entries, or 64 on a 64-bit machine)
        case RET_BIG:
            p++;
            W_ size = GET_LARGE_BITMAP(&info->i)->size;
            thread_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
            p += size;
            continue;

        case RET_FUN:
        {
            StgRetFun *ret_fun = (StgRetFun *)p;
            StgFunInfoTable *fun_info =
                FUN_INFO_PTR_TO_STRUCT(get_threaded_info((P_)ret_fun->fun));
                 // *before* threading it!
            thread(&ret_fun->fun);
            p = thread_arg_block(fun_info, ret_fun->payload);
            continue;
        }

        default:
            barf("thread_stack: weird activation record found on stack: %d",
                 (int)(info->i.type));
        }
    }
}

STATIC_INLINE P_
thread_PAP_payload (StgClosure *fun, StgClosure **payload, W_ size)
{
    StgFunInfoTable *fun_info =
        FUN_INFO_PTR_TO_STRUCT(get_threaded_info((P_)fun));
    ASSERT(fun_info->i.type != PAP);

    P_ p = (P_)payload;

    W_ bitmap;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        thread_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    case ARG_BCO:
        thread_large_bitmap((P_)payload, BCO_BITMAP(fun), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = thread_small_bitmap(p, size, bitmap);
        break;
    }

    return p;
}

STATIC_INLINE P_
thread_PAP (StgPAP *pap)
{
    P_ p = thread_PAP_payload(pap->fun, pap->payload, pap->n_args);
    thread(&pap->fun);
    return p;
}

STATIC_INLINE P_
thread_AP (StgAP *ap)
{
    P_ p = thread_PAP_payload(ap->fun, ap->payload, ap->n_args);
    thread(&ap->fun);
    return p;
}

STATIC_INLINE P_
thread_AP_STACK (StgAP_STACK *ap)
{
    thread(&ap->fun);
    thread_stack((P_)ap->payload, (P_)ap->payload + ap->size);
    return (P_)ap + sizeofW(StgAP_STACK) + ap->size;
}

STATIC_INLINE P_
thread_continuation(StgContinuation *cont)
{
    thread_stack(cont->stack, cont->stack + cont->stack_size);
    return (P_)cont + continuation_sizeW(cont);
}

static P_
thread_TSO (StgTSO *tso)
{
    thread_(&tso->_link);
    thread_(&tso->global_link);

    switch (ACQUIRE_LOAD(&tso->why_blocked)) {
    case BlockedOnMVar:
    case BlockedOnMVarRead:
    case BlockedOnBlackHole:
    case BlockedOnMsgThrowTo:
    case NotBlocked:
        thread_(&tso->block_info.closure);
        break;
    default:
        break;
    }
    thread_(&tso->blocked_exceptions);
    thread_(&tso->bq);

    thread_(&tso->trec);

    if (tso->label != NULL) {
        thread_((StgClosure **)&tso->label);
    }

    thread_(&tso->stackobj);
    return (P_)tso + sizeofW(StgTSO);
}

/* ----------------------------------------------------------------------------
    Note [CNFs in compacting GC]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    CNF hash table keys point outside of the CNF so those need to be threaded
    and updated during compaction. After compaction we need to re-visit those
    hash tables for re-hashing. The list `nfdata_chain` is used for that
    purpose. When we thread keys of a CNF we add the CNF to the list. After
    compacting is done we re-visit the CNFs in the list and re-hash their
    tables. See also #17937 for more details.
   ------------------------------------------------------------------------- */

static StgCompactNFData *nfdata_chain = NULL;

static void
thread_nfdata_hash_key(void *data STG_UNUSED, StgWord *key, const void *value STG_UNUSED)
{
    thread_((void *)key);
}

static void
add_hash_entry(void *data, StgWord key, const void *value)
{
    HashTable *new_hash = (HashTable *)data;
    insertHashTable(new_hash, key, value);
}

static void
rehash_CNFs(void)
{
    while (nfdata_chain != NULL) {
        StgCompactNFData *str = nfdata_chain;
        nfdata_chain = str->link;
        str->link = NULL;

        HashTable *new_hash = allocHashTable();
        mapHashTable(str->hash, (void*)new_hash, add_hash_entry);
        freeHashTable(str->hash, NULL);
        str->hash = new_hash;
    }
}

static void
update_fwd_cnf( bdescr *bd )
{
    while (bd) {
        ASSERT(bd->flags & BF_COMPACT);
        StgCompactNFData *str = ((StgCompactNFDataBlock*)bd->start)->owner;

        // Thread hash table keys. Values won't be moved as those are inside the
        // CNF, and the CNF is a large object and so won't ever move.
        if (str->hash) {
            mapHashTableKeys(str->hash, NULL, thread_nfdata_hash_key);
            ASSERT(str->link == NULL);
            str->link = nfdata_chain;
            nfdata_chain = str;
        }

        bd = bd->link;
    }
}

static void
update_fwd_large( bdescr *bd )
{
  for (; bd != NULL; bd = bd->link) {

    // nothing to do in a pinned block; it might not even have an object
    // at the beginning.
    if (bd->flags & BF_PINNED) continue;

    P_ p = bd->start;
    const StgInfoTable *info = get_itbl((StgClosure *)p);

    switch (info->type) {

    case ARR_WORDS:
      // nothing to follow
      continue;

    // See Note [Black holes in large objects] in Evac.c for why.
    case BLACKHOLE:
      {
        thread_obj(info, p);
        continue;
      }

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
      // follow everything
      {
          StgMutArrPtrs *a;

          a = (StgMutArrPtrs*)p;
          for (p = (P_)a->payload; p < (P_)&a->payload[a->ptrs]; p++) {
              thread((StgClosure **)p);
          }
          continue;
      }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      // follow everything
      {
          StgSmallMutArrPtrs *a = (StgSmallMutArrPtrs*)p;
          for (p = (P_)a->payload; p < (P_)&a->payload[a->ptrs]; p++) {
              thread((StgClosure **)p);
          }
          continue;
      }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;
        thread_stack(stack->sp, stack->stack + stack->stack_size);
        continue;
    }

    case AP_STACK:
        thread_AP_STACK((StgAP_STACK *)p);
        continue;

    case PAP:
        thread_PAP((StgPAP *)p);
        continue;

    case TREC_CHUNK:
    {
        StgTRecChunk *tc = (StgTRecChunk *)p;
        TRecEntry *e = &(tc -> entries[0]);
        thread_(&tc->prev_chunk);
        for (W_ i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          thread_(&e->tvar);
          thread(&e->expected_value);
          thread(&e->new_value);
        }
        continue;
    }

    case CONTINUATION:
        thread_continuation((StgContinuation *)p);
        continue;

    default:
      barf("update_fwd_large: unknown/strange object  %d", (int)(info->type));
    }
  }
}

// ToDo: too big to inline
static /* STATIC_INLINE */ P_
thread_obj (const StgInfoTable *info, P_ p)
{
    switch (info->type) {
    case THUNK_0_1:
        return p + sizeofW(StgThunk) + 1;

    case FUN_0_1:
    case CONSTR_0_1:
        return p + sizeofW(StgHeader) + 1;

    case FUN_1_0:
    case CONSTR_1_0:
        thread(&((StgClosure *)p)->payload[0]);
        return p + sizeofW(StgHeader) + 1;

    case THUNK_1_0:
        thread(&((StgThunk *)p)->payload[0]);
        return p + sizeofW(StgThunk) + 1;

    case THUNK_0_2:
        return p + sizeofW(StgThunk) + 2;

    case FUN_0_2:
    case CONSTR_0_2:
        return p + sizeofW(StgHeader) + 2;

    case THUNK_1_1:
        thread(&((StgThunk *)p)->payload[0]);
        return p + sizeofW(StgThunk) + 2;

    case FUN_1_1:
    case CONSTR_1_1:
        thread(&((StgClosure *)p)->payload[0]);
        return p + sizeofW(StgHeader) + 2;

    case THUNK_2_0:
        thread(&((StgThunk *)p)->payload[0]);
        thread(&((StgThunk *)p)->payload[1]);
        return p + sizeofW(StgThunk) + 2;

    case FUN_2_0:
    case CONSTR_2_0:
        thread(&((StgClosure *)p)->payload[0]);
        thread(&((StgClosure *)p)->payload[1]);
        return p + sizeofW(StgHeader) + 2;

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        thread_(&bco->instrs);
        thread_(&bco->literals);
        thread_(&bco->ptrs);
        return p + bco_sizeW(bco);
    }

    case THUNK:
    {
        P_ end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
            thread((StgClosure **)p);
        }
        return p + info->layout.payload.nptrs;
    }

    case FUN:
    case CONSTR:
    case CONSTR_NOCAF:
    case PRIM:
    case MUT_PRIM:
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    case TVAR:
    case BLACKHOLE:
    case BLOCKING_QUEUE:
    {
        P_ end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            thread((StgClosure **)p);
        }
        return p + info->layout.payload.nptrs;
    }

    case WEAK:
    {
        StgWeak *w = (StgWeak *)p;
        thread(&w->cfinalizers);
        thread(&w->key);
        thread(&w->value);
        thread(&w->finalizer);
        if (w->link != NULL) {
            thread_(&w->link);
        }
        return p + sizeofW(StgWeak);
    }

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = (StgMVar *)p;
        thread_(&mvar->head);
        thread_(&mvar->tail);
        thread(&mvar->value);
        return p + sizeofW(StgMVar);
    }

    case IND:
        thread(&((StgInd *)p)->indirectee);
        return p + sizeofW(StgInd);

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        thread(&s->selectee);
        return p + THUNK_SELECTOR_sizeW();
    }

    case AP_STACK:
        return thread_AP_STACK((StgAP_STACK *)p);

    case PAP:
        return thread_PAP((StgPAP *)p);

    case AP:
        return thread_AP((StgAP *)p);

    case ARR_WORDS:
        return p + arr_words_sizeW((StgArrBytes *)p);

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        // follow everything
    {
        StgMutArrPtrs *a = (StgMutArrPtrs *)p;
        for (p = (P_)a->payload; p < (P_)&a->payload[a->ptrs]; p++) {
            thread((StgClosure **)p);
        }

        return (P_)a + mut_arr_ptrs_sizeW(a);
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        // follow everything
    {
        StgSmallMutArrPtrs *a = (StgSmallMutArrPtrs *)p;
        for (p = (P_)a->payload; p < (P_)&a->payload[a->ptrs]; p++) {
            thread((StgClosure **)p);
        }

        return (P_)a + small_mut_arr_ptrs_sizeW(a);
    }

    case TSO:
        return thread_TSO((StgTSO *)p);

    case STACK:
    {
        StgStack *stack = (StgStack*)p;
        thread_stack(stack->sp, stack->stack + stack->stack_size);
        return p + stack_sizeW(stack);
    }

    case TREC_CHUNK:
    {
        StgTRecChunk *tc = (StgTRecChunk *)p;
        TRecEntry *e = &(tc -> entries[0]);
        thread_(&tc->prev_chunk);
        for (W_ i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          thread_(&e->tvar);
          thread(&e->expected_value);
          thread(&e->new_value);
        }
        return p + sizeofW(StgTRecChunk);
    }

    case CONTINUATION:
        return thread_continuation((StgContinuation *)p);

    default:
        barf("update_fwd: unknown/strange object  %d", (int)(info->type));
        return NULL;
    }
}

static void
update_fwd( bdescr *blocks )
{
    bdescr *bd = blocks;

    // cycle through all the blocks in the step
    for (; bd != NULL; bd = bd->link) {
        P_ p = bd->start;

        // linearly scan the objects in this block
        while (p < bd->free) {
            ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
            const StgInfoTable *info = get_itbl((StgClosure *)p);
            p = thread_obj(info, p);
        }
    }
}

static void
update_fwd_compact( bdescr *blocks )
{
    bdescr *bd = blocks;
    bdescr *free_bd = blocks;
    P_ free = free_bd->start;

    // cycle through all the blocks in the step
    for (; bd != NULL; bd = bd->link) {
        P_ p = bd->start;

        while (p < bd->free ) {

            while ( p < bd->free && !is_marked(p,bd) ) {
                p++;
            }
            if (p >= bd->free) {
                break;
            }

            // Problem: we need to know the destination for this cell
            // in order to unthread its info pointer.  But we can't
            // know the destination without the size, because we may
            // spill into the next block.  So we have to run down the
            // threaded list and get the info ptr first.
            //
            // ToDo: one possible avenue of attack is to use the fact
            // that if (p&BLOCK_MASK) >= (free&BLOCK_MASK), then we
            // definitely have enough room.  Also see bug #1147.
            StgInfoTable *iptr = get_threaded_info(p);
            StgInfoTable *info = INFO_PTR_TO_STRUCT(iptr);

            P_ q = p;

            p = thread_obj(info, p);

            W_ size = p - q;
            if (free + size > free_bd->start + BLOCK_SIZE_W) {
                // set the next bit in the bitmap to indicate that this object
                // needs to be pushed into the next block.  This saves us having
                // to run down the threaded info pointer list twice during the
                // next pass. See Note [Mark bits in mark-compact collector] in
                // Compact.h.
                mark(q+1,bd);
                free_bd = free_bd->link;
                free = free_bd->start;
            } else {
                ASSERT(!is_marked(q+1,bd));
            }

            StgWord iptr_tag = get_iptr_tag(iptr);
            unthread(q, (W_)free, iptr_tag);
            free += size;
        }
    }
}

static W_
update_bkwd_compact( generation *gen )
{
    bdescr *bd, *free_bd;
    bd = free_bd = gen->old_blocks;

    P_ free = free_bd->start;
    W_ free_blocks = 1;

    // cycle through all the blocks in the step
    for (; bd != NULL; bd = bd->link) {
        P_ p = bd->start;

        while (p < bd->free) {

            while (p < bd->free && !is_marked(p,bd)) {
                p++;
            }

            if (p >= bd->free) {
                break;
            }

            if (is_marked(p+1,bd)) {
                // Don't forget to update the free ptr in the block desc
                free_bd->free = free;

                // Zero the remaining bytes of this block before moving on to
                // the next block
                IF_DEBUG(zero_on_gc, {
                    memset(free_bd->free, 0xaa,
                           BLOCK_SIZE - ((W_)(free_bd->free - free_bd->start) * sizeof(W_)));
                });

                free_bd = free_bd->link;
                free = free_bd->start;
                free_blocks++;
            }

            StgInfoTable *iptr = get_threaded_info(p);
            StgWord iptr_tag = get_iptr_tag(iptr);
            unthread(p, (W_)free, iptr_tag);
            ASSERT(LOOKS_LIKE_INFO_PTR((W_)((StgClosure *)p)->header.info));
            const StgInfoTable *info = get_itbl((StgClosure *)p);
            W_ size = closure_sizeW_((StgClosure *)p,info);

            if (free != p) {
                move(free,p,size);
            }

            // relocate TSOs
            if (info->type == STACK) {
                move_STACK((StgStack *)p, (StgStack *)free);
            }

            free += size;
            p += size;
        }
    }

    // Free the remaining blocks and count what's left.
    free_bd->free = free;
    if (free_bd->link != NULL) {
        freeChain(free_bd->link);
        free_bd->link = NULL;
    }

    // Zero the free bits of the last used block.
    IF_DEBUG(zero_on_gc, {
        W_ block_size_bytes = free_bd->blocks * BLOCK_SIZE;
        W_ block_in_use_bytes = (free_bd->free - free_bd->start) * sizeof(W_);
        W_ block_free_bytes = block_size_bytes - block_in_use_bytes;
        memset(free_bd->free, 0xaa, block_free_bytes);
    });

    return free_blocks;
}

void
compact(StgClosure *static_objects,
        StgWeak **dead_weak_ptr_list,
        StgTSO **resurrected_threads)
{
    // 1. thread the roots
    markCapabilities((evac_fn)thread_root, NULL);

    // the weak pointer lists...
    for (W_ g = 0; g < RtsFlags.GcFlags.generations; g++) {
        if (generations[g].weak_ptr_list != NULL) {
            thread((void *)&generations[g].weak_ptr_list);
        }
    }

    if (dead_weak_ptr_list != NULL) {
        thread((void *)dead_weak_ptr_list); // tmp
    }

    // mutable lists
    for (W_ g = 1; g < RtsFlags.GcFlags.generations; g++) {
        for (W_ n = 0; n < getNumCapabilities(); n++) {
            for (bdescr *bd = getCapability(n)->mut_lists[g];
                 bd != NULL; bd = bd->link) {
                for (P_ p = bd->start; p < bd->free; p++) {
                    thread((StgClosure **)p);
                }
            }
        }
    }

    // the global thread list
    for (W_ g = 0; g < RtsFlags.GcFlags.generations; g++) {
        thread((void *)&generations[g].threads);
    }

    // any threads resurrected during this GC
    thread((void *)resurrected_threads);

    // the task list
    for (Task *task = all_tasks; task != NULL; task = task->all_next) {
        for (InCall *incall = task->incall; incall != NULL;
             incall = incall->prev_stack) {
            if (incall->tso) {
                thread_(&incall->tso);
            }
        }
    }

    // the static objects
    thread_static(static_objects /* ToDo: ok? */);

    // the stable pointer table
    threadStablePtrTable((evac_fn)thread_root, NULL);

    // the stable name table
    threadStableNameTable((evac_fn)thread_root, NULL);

    // the CAF list (used by GHCi)
    markCAFs((evac_fn)thread_root, NULL);

    // 2. update forward ptrs
    for (W_ g = 0; g < RtsFlags.GcFlags.generations; g++) {
        generation *gen = &generations[g];
        debugTrace(DEBUG_gc, "update_fwd:  %d", g);

        update_fwd(gen->blocks);
        for (W_ n = 0; n < getNumCapabilities(); n++) {
            update_fwd(gc_threads[n]->gens[g].todo_bd);
            update_fwd(gc_threads[n]->gens[g].part_list);
        }
        update_fwd_large(gen->scavenged_large_objects);
        update_fwd_cnf(gen->live_compact_objects);
        if (g == RtsFlags.GcFlags.generations-1 && gen->old_blocks != NULL) {
            debugTrace(DEBUG_gc, "update_fwd:  %d (compact)", g);
            update_fwd_compact(gen->old_blocks);
        }
    }

    // 3. update backward ptrs
    generation *gen = oldest_gen;
    if (gen->old_blocks != NULL) {
        W_ blocks = update_bkwd_compact(gen);
        debugTrace(DEBUG_gc,
                   "update_bkwd: %d (compact, old: %d blocks, now %d blocks)",
                   gen->no, gen->n_old_blocks, blocks);
        gen->n_old_blocks = blocks;
    }

    // 4. Re-hash hash tables of threaded CNFs.
    // See Note [CNFs in compacting GC] above.
    rehash_CNFs();
}
