/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "GC.h"
#include "GCThread.h"
#include "GCUtils.h"
#include "Compact.h"
#include "MarkStack.h"
#include "Evac.h"
#include "Trav.h"
#include "Apply.h"
#include "Trace.h"
#include "Sanity.h"
#include "Capability.h"
#include "LdvProfile.h"
#include "Hash.h"
#include "Printer.h"

#include "sm/MarkWeak.h"

static void traverse_stack (StgPtr p, StgPtr stack_end);

static void traverse_large_bitmap (StgPtr p,
                                   StgLargeBitmap *large_bitmap,
                                   StgWord size );


/* -----------------------------------------------------------------------------
   Traverse a TSO.
   -------------------------------------------------------------------------- */

static void
traverseTSO (StgTSO *tso)
{
    bool saved_eager;

    debugTrace(DEBUG_gc,"scavenging thread %d",(int)tso->id);

    // update the pointer from the InCall.
    if (tso->bound != NULL) {
        // NB. We can't just set tso->bound->tso = tso, because this
        // might be an invalid copy the TSO resulting from multiple
        // threads evacuating the TSO simultaneously (see
        // Evac.c:copy_tag()).  Calling evacuate() on this pointer
        // will ensure that we update it to point to the correct copy.
        evacuate((StgClosure **)&tso->bound->tso);
    }

    saved_eager = gct->eager_promotion;
    gct->eager_promotion = false;

    evacuate((StgClosure **)&tso->blocked_exceptions);
    evacuate((StgClosure **)&tso->bq);

    // scavange current transaction record
    evacuate((StgClosure **)&tso->trec);

    evacuate((StgClosure **)&tso->stackobj);

    evacuate((StgClosure **)&tso->_link);
    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
        || tso->why_blocked == BlockedOnBlackHole
        || tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
        ) {
        evacuate(&tso->block_info.closure);
    }
#ifdef THREADED_RTS
    // in the THREADED_RTS, block_info.closure must always point to a
    // valid closure, because we assume this in throwTo().  In the
    // non-threaded RTS it might be a FD (for
    // BlockedOnRead/BlockedOnWrite) or a time value (BlockedOnDelay)
    else {
        tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
    }
#endif

    tso->dirty = gct->failed_to_evac;

    gct->eager_promotion = saved_eager;
}

/* ----------------------------------------------------------------------------
   Scavenging compact objects
   ------------------------------------------------------------------------- */

typedef struct {
    // We must save gct when calling mapHashTable(), which is compiled
    // without GCThread.h and so uses a different calling convention.
    // See also GC.c:mark_root where we do a similar thing.
    gc_thread *saved_gct;
    HashTable *newHash;
} MapHashData;

static void
evacuate_hash_entry(MapHashData *dat, StgWord key, const void *value)
{
    StgClosure *p = (StgClosure*)key;
#ifdef THREADED_RTS
    gc_thread *old_gct = gct;
#endif

    SET_GCT(dat->saved_gct);
    evacuate(&p);
    insertHashTable(dat->newHash, (StgWord)p, value);
    SET_GCT(old_gct);
}

static void
traverse_compact(StgCompactNFData *str)
{
    bool saved_eager;
    saved_eager = gct->eager_promotion;
    gct->eager_promotion = false;

    if (str->hash) {
        MapHashData dat;
        dat.saved_gct = gct;
        HashTable *newHash = allocHashTable();
        dat.newHash = newHash;
        mapHashTable(str->hash, (void*)&dat, (MapHashFn)evacuate_hash_entry);
        freeHashTable(str->hash, NULL);
        str->hash = newHash;
    }

    debugTrace(DEBUG_compact,
               "compact alive @%p, gen %d, %" FMT_Word " bytes",
               str, Bdescr((P_)str)->gen_no, str->totalW * sizeof(W_))

    gct->eager_promotion = saved_eager;
    if (gct->failed_to_evac) {
        ((StgClosure *)str)->header.info = &stg_COMPACT_NFDATA_DIRTY_info;
    } else {
        ((StgClosure *)str)->header.info = &stg_COMPACT_NFDATA_CLEAN_info;
    }
}

/* -----------------------------------------------------------------------------
   Mutable arrays of pointers
   -------------------------------------------------------------------------- */

static StgPtr traverse_mut_arr_ptrs (StgMutArrPtrs *a)
{
    W_ m;
    bool any_failed;
    StgPtr p, q;

    any_failed = false;
    p = (StgPtr)&a->payload[0];
    for (m = 0; (int)m < (int)mutArrPtrsCards(a->ptrs) - 1; m++)
    {
        q = p + (1 << MUT_ARR_PTRS_CARD_BITS);
        for (; p < q; p++) {
            evacuate((StgClosure**)p);
        }
        if (gct->failed_to_evac) {
            any_failed = true;
            *mutArrPtrsCard(a,m) = 1;
            gct->failed_to_evac = false;
        } else {
            *mutArrPtrsCard(a,m) = 0;
        }
    }

    q = (StgPtr)&a->payload[a->ptrs];
    if (p < q) {
        for (; p < q; p++) {
            evacuate((StgClosure**)p);
        }
        if (gct->failed_to_evac) {
            any_failed = true;
            *mutArrPtrsCard(a,m) = 1;
            gct->failed_to_evac = false;
        } else {
            *mutArrPtrsCard(a,m) = 0;
        }
    }

    gct->failed_to_evac = any_failed;
    return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}

// traverse only the marked areas of a MUT_ARR_PTRS
static StgPtr traverse_mut_arr_ptrs_marked (StgMutArrPtrs *a)
{
    W_ m;
    StgPtr p, q;
    bool any_failed;

    any_failed = false;
    for (m = 0; m < mutArrPtrsCards(a->ptrs); m++)
    {
        if (*mutArrPtrsCard(a,m) != 0) {
            p = (StgPtr)&a->payload[m << MUT_ARR_PTRS_CARD_BITS];
            q = stg_min(p + (1 << MUT_ARR_PTRS_CARD_BITS),
                        (StgPtr)&a->payload[a->ptrs]);
            for (; p < q; p++) {
                evacuate((StgClosure**)p);
            }
            if (gct->failed_to_evac) {
                any_failed = true;
                gct->failed_to_evac = false;
            } else {
                *mutArrPtrsCard(a,m) = 0;
            }
        }
    }

    gct->failed_to_evac = any_failed;
    return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}

STATIC_INLINE StgPtr
traverse_small_bitmap (StgPtr p, StgWord size, StgWord bitmap)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            evacuate((StgClosure **)p);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

/* -----------------------------------------------------------------------------
   Blocks of function args occur on the stack (at the top) and
   in PAPs.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
traverse_arg_block (const StgFunInfoTable *fun_info, StgClosure **args)
{
    StgPtr p;
    StgWord bitmap;
    StgWord size;

    p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        traverse_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = traverse_small_bitmap(p, size, bitmap);
        break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
traverse_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size)
{
    StgPtr p;
    StgWord bitmap;
    const StgFunInfoTable *fun_info;

    fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        traverse_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    case ARG_BCO:
        traverse_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = traverse_small_bitmap(p, size, bitmap);
        break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
traverse_PAP (StgPAP *pap)
{
    evacuate(&pap->fun);
    return traverse_PAP_payload (pap->fun, pap->payload, pap->n_args);
}

STATIC_INLINE StgPtr
traverse_AP (StgAP *ap)
{
    evacuate(&ap->fun);
    return traverse_PAP_payload (ap->fun, ap->payload, ap->n_args);
}

/* -----------------------------------------------------------------------------
   Traverse SRTs
   -------------------------------------------------------------------------- */

/* Similar to traverse_large_bitmap(), but we don't write back the
 * pointers we get back from evacuate().
 */
static void
traverse_large_srt_bitmap( StgLargeSRT *large_srt )
{
    uint32_t i, j, size;
    StgWord bitmap;
    StgClosure **p;

    size   = (uint32_t)large_srt->l.size;
    p      = (StgClosure **)large_srt->srt;

    for (i = 0; i < size / BITS_IN(W_); i++) {
        bitmap = large_srt->l.bitmap[i];
        // skip zero words: bitmaps can be very sparse, and this helps
        // performance a lot in some cases.
        if (bitmap != 0) {
            for (j = 0; j < BITS_IN(W_); j++) {
                if ((bitmap & 1) != 0) {
                    evacuate(p);
                }
                p++;
                bitmap = bitmap >> 1;
            }
        } else {
            p += BITS_IN(W_);
        }
    }
    if (size % BITS_IN(W_) != 0) {
        bitmap = large_srt->l.bitmap[i];
        for (j = 0; j < size % BITS_IN(W_); j++) {
            if ((bitmap & 1) != 0) {
                evacuate(p);
            }
            p++;
            bitmap = bitmap >> 1;
        }
    }
}

/* evacuate the SRT.  If srt_bitmap is zero, then there isn't an
 * srt field in the info table.  That's ok, because we'll
 * never dereference it.
 */
STATIC_INLINE GNUC_ATTR_HOT void
traverse_srt (StgClosure **srt, uint32_t srt_bitmap)
{
  uint32_t bitmap;
  StgClosure **p;

  return;

  bitmap = srt_bitmap;
  p = srt;

  if (bitmap == (StgHalfWord)(-1)) {
      traverse_large_srt_bitmap( (StgLargeSRT *)srt );
      return;
  }

  while (bitmap != 0) {
      if ((bitmap & 1) != 0) {
#if defined(COMPILING_WINDOWS_DLL)
          // Special-case to handle references to closures hiding out in DLLs, since
          // double indirections required to get at those. The code generator knows
          // which is which when generating the SRT, so it stores the (indirect)
          // reference to the DLL closure in the table by first adding one to it.
          // We check for this here, and undo the addition before evacuating it.
          //
          // If the SRT entry hasn't got bit 0 set, the SRT entry points to a
          // closure that's fixed at link-time, and no extra magic is required.
          if ( (W_)(*srt) & 0x1 ) {
              evacuate( (StgClosure**) ((W_) (*srt) & ~0x1));
          } else {
              evacuate(p);
          }
#else
          evacuate(p);
#endif
      }
      p++;
      bitmap = bitmap >> 1;
  }
}


STATIC_INLINE GNUC_ATTR_HOT void
traverse_thunk_srt(const StgInfoTable *info)
{
    StgThunkInfoTable *thunk_info;
    uint32_t bitmap;

    if (!major_gc) return;

    thunk_info = itbl_to_thunk_itbl(info);
    bitmap = thunk_info->i.srt_bitmap;
    if (bitmap) {
        // don't read srt_offset if bitmap==0, because it doesn't exist
        // and so the memory might not be readable.
        traverse_srt((StgClosure **)GET_SRT(thunk_info), bitmap);
    }
}

STATIC_INLINE GNUC_ATTR_HOT void
traverse_fun_srt(const StgInfoTable *info)
{
    StgFunInfoTable *fun_info;
    uint32_t bitmap;

    if (!major_gc) return;

    fun_info = itbl_to_fun_itbl(info);
    bitmap = fun_info->i.srt_bitmap;
    if (bitmap) {
        // don't read srt_offset if bitmap==0, because it doesn't exist
        // and so the memory might not be readable.
        traverse_srt((StgClosure **)GET_FUN_SRT(fun_info), bitmap);
    }
}

/* -----------------------------------------------------------------------------
   Traverse a block from the given scan pointer up to bd->free.

   evac_gen_no is set by the caller to be either zero (for a step in a
   generation < N) or G where G is the generation of the step being
   traversed.

   We sometimes temporarily change evac_gen_no back to zero if we're
   scavenging a mutable object where eager promotion isn't such a good
   idea.
   -------------------------------------------------------------------------- */

static GNUC_ATTR_HOT void
traverse_block (bdescr *bd)
{
    barf("traverse_block %p", bd);
}

#define SEEN (((StgWord)1) << 55)

static GNUC_ATTR_HOT void
traverse_closure (StgClosure **a, StgWord dir, void (*callback)(const StgClosure *c))
{
    const StgInfoTable *info;
    StgPtr p;
    StgPtr q;

    p = (StgPtr)UNTAG_CLOSURE(*a);
    if (dir == ((*p) & SEEN))
        return;
    (*p) &= ~SEEN;

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

    (*callback)(*a);

#define RECURSE(t) traverse_closure(t, dir, callback)

    info = get_itbl((StgClosure *)p);

    (*p) |= dir;

    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = ((StgMVar *)p);
        RECURSE((StgClosure **)&mvar->head);
        RECURSE((StgClosure **)&mvar->tail);
        RECURSE((StgClosure **)&mvar->value);
        break;
    }

    case TVAR:
    {
        StgTVar *tvar = ((StgTVar *)p);
        RECURSE((StgClosure **)&tvar->current_value);
        RECURSE((StgClosure **)&tvar->first_watch_queue_entry);
        break;
    }

    case FUN_2_0:
        traverse_fun_srt(info);
        RECURSE(&((StgClosure *)p)->payload[1]);
        RECURSE(&((StgClosure *)p)->payload[0]);
        break;

    case THUNK_2_0:
        traverse_thunk_srt(info);
        RECURSE(&((StgThunk *)p)->payload[1]);
        RECURSE(&((StgThunk *)p)->payload[0]);
        break;

    case CONSTR_2_0:
        RECURSE(&((StgClosure *)p)->payload[1]);
        RECURSE(&((StgClosure *)p)->payload[0]);
        break;

    case THUNK_1_0:
        traverse_thunk_srt(info);
        RECURSE(&((StgThunk *)p)->payload[0]);
        break;

    case FUN_1_0:
        traverse_fun_srt(info);
    case CONSTR_1_0:
        RECURSE(&((StgClosure *)p)->payload[0]);
        break;

    case THUNK_0_1:
        traverse_thunk_srt(info);
        break;

    case FUN_0_1:
        traverse_fun_srt(info);
    case CONSTR_0_1:
        break;

    case THUNK_0_2:
        traverse_thunk_srt(info);
        break;

    case FUN_0_2:
        traverse_fun_srt(info);
    case CONSTR_0_2:
        break;

    case THUNK_1_1:
        traverse_thunk_srt(info);
        RECURSE(&((StgThunk *)p)->payload[0]);
        break;

    case FUN_1_1:
        traverse_fun_srt(info);
    case CONSTR_1_1:
        RECURSE(&((StgClosure *)p)->payload[0]);
        break;

    case FUN:
        traverse_fun_srt(info);
        goto gen_obj;

    case THUNK:
    {
        StgPtr end;

        traverse_thunk_srt(info);
        end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (q = (P_)((StgThunk *)p)->payload; q < end; q++) {
            RECURSE((StgClosure **)q);
        }
        break;
    }

    gen_obj:
    case CONSTR:
    case CONSTR_NOCAF:
    case WEAK:
    case PRIM:
    {
        StgPtr end;

        end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (q = (P_)((StgClosure *)p)->payload; q < end; q++) {
            RECURSE((StgClosure **)q);
        }
        break;
    }

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        RECURSE((StgClosure **)&bco->instrs);
        RECURSE((StgClosure **)&bco->literals);
        RECURSE((StgClosure **)&bco->ptrs);
        break;
    }

    case IND:
        // IND can happen, for example, when the interpreter allocates
        // a gigantic AP closure (more than one block), which ends up
        // on the large-object list and then gets updated.  See #3424.
    case BLACKHOLE:
    case IND_STATIC:
        RECURSE(&((StgInd *)p)->indirectee);
        break;

    case THUNK_STATIC:
        traverse_thunk_srt(info);
        break;

    case FUN_STATIC:
        traverse_fun_srt(info);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        RECURSE(&((StgMutVar *)p)->var);
        break;

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        RECURSE(&bq->bh);
        RECURSE((StgClosure**)&bq->owner);
        RECURSE((StgClosure**)&bq->queue);
        RECURSE((StgClosure**)&bq->link);
        break;
    }

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        RECURSE(&s->selectee);
        break;
    }

    /*
    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)p;

        RECURSE(&ap->fun);
        traverse_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
        break;
    }
    */

    case PAP:
    {
        StgPAP *pap = (StgPAP *)p;
        RECURSE(&pap->fun);
        /* Oops, we should recurse on the fields too, but not trivial */
        break;
    }

    /*
    case AP:
        traverse_AP((StgAP *)p);
        break;
    */

    case ARR_WORDS:
        // nothing to follow
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
        /*
        traverse_mut_arr_ptrs((StgMutArrPtrs*)p);
        */
        break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
        // follow everything
    {
        /*
        traverse_mut_arr_ptrs((StgMutArrPtrs*)p);
        */
        break;
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
        // follow everything
    {
        StgPtr next;

        next = (StgPtr)p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (q = (P_)((StgSmallMutArrPtrs *)p)->payload; q < next; q++) {
            RECURSE((StgClosure **)q);
        }
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
        // follow everything
    {
        StgPtr next;

        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (q = (P_)((StgSmallMutArrPtrs *)p)->payload; q < next; q++) {
            RECURSE((StgClosure **)q);
        }
        break;
    }

/*
    case TSO:
    {
        traverseTSO((StgTSO *)p);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        traverse_stack(stack->sp, stack->stack + stack->stack_size);
        break;
    }
*/

    case MUT_PRIM:
      {
        StgPtr end;

        end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (q = (P_)((StgClosure *)p)->payload; q < end; q++) {
            RECURSE((StgClosure **)q);
        }
        break;
      }

    case TREC_CHUNK:
      {
        StgWord i;
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        TRecEntry *e = &(tc -> entries[0]);
        RECURSE((StgClosure **)&tc->prev_chunk);
        for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          RECURSE((StgClosure **)&e->tvar);
          RECURSE((StgClosure **)&e->expected_value);
          RECURSE((StgClosure **)&e->new_value);
        }
        break;
      }

    default:
        barf("traverse: unimplemented/strange closure type %d @ %p",
             info->type, p);
    }
}

static void dump_callback(const StgClosure *c)
{
#ifdef DEBUG
    debugBelch("%p = ", c);
    printClosure(c);
#else
    printf("%p\n", c);
#endif
}

static void null_callback(const StgClosure *c)
{
    (void)c;
}

void dump_closure(const StgClosure *p)
{
    debugBelch("Dumping closure %p\n", p);
    traverse_closure((StgClosure **)&p, SEEN, dump_callback);
    traverse_closure((StgClosure **)&p, 0, null_callback);
}

/* -----------------------------------------------------------------------------
   Traverse everything on the mark stack.

   This is slightly different from traverse():
      - we don't walk linearly through the objects, so the traverser
        doesn't need to advance the pointer on to the next object.
   -------------------------------------------------------------------------- */

static void
traverse_mark_stack(void)
{
    StgPtr p, q;
    const StgInfoTable *info;
    bool saved_eager_promotion;

    gct->evac_gen_no = oldest_gen->no;
    saved_eager_promotion = gct->eager_promotion;

    while ((p = pop_mark_stack())) {

        ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure *)p);

        q = p;
        switch (info->type) {

        case MVAR_CLEAN:
        case MVAR_DIRTY:
        {
            StgMVar *mvar = ((StgMVar *)p);
            gct->eager_promotion = false;
            evacuate((StgClosure **)&mvar->head);
            evacuate((StgClosure **)&mvar->tail);
            evacuate((StgClosure **)&mvar->value);
            gct->eager_promotion = saved_eager_promotion;

            if (gct->failed_to_evac) {
                mvar->header.info = &stg_MVAR_DIRTY_info;
            } else {
                mvar->header.info = &stg_MVAR_CLEAN_info;
            }
            break;
        }

        case TVAR:
        {
            StgTVar *tvar = ((StgTVar *)p);
            gct->eager_promotion = false;
            evacuate((StgClosure **)&tvar->current_value);
            evacuate((StgClosure **)&tvar->first_watch_queue_entry);
            gct->eager_promotion = saved_eager_promotion;

            if (gct->failed_to_evac) {
                tvar->header.info = &stg_TVAR_DIRTY_info;
            } else {
                tvar->header.info = &stg_TVAR_CLEAN_info;
            }
            break;
        }

        case FUN_2_0:
            traverse_fun_srt(info);
            evacuate(&((StgClosure *)p)->payload[1]);
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case THUNK_2_0:
            traverse_thunk_srt(info);
            evacuate(&((StgThunk *)p)->payload[1]);
            evacuate(&((StgThunk *)p)->payload[0]);
            break;

        case CONSTR_2_0:
            evacuate(&((StgClosure *)p)->payload[1]);
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case FUN_1_0:
        case FUN_1_1:
            traverse_fun_srt(info);
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case THUNK_1_0:
        case THUNK_1_1:
            traverse_thunk_srt(info);
            evacuate(&((StgThunk *)p)->payload[0]);
            break;

        case CONSTR_1_0:
        case CONSTR_1_1:
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case FUN_0_1:
        case FUN_0_2:
            traverse_fun_srt(info);
            break;

        case THUNK_0_1:
        case THUNK_0_2:
            traverse_thunk_srt(info);
            break;

        case CONSTR_0_1:
        case CONSTR_0_2:
            break;

        case FUN:
            traverse_fun_srt(info);
            goto gen_obj;

        case THUNK:
        {
            StgPtr end;

            traverse_thunk_srt(info);
            end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
                evacuate((StgClosure **)p);
            }
            break;
        }

        gen_obj:
        case CONSTR:
        case CONSTR_NOCAF:
        case WEAK:
        case PRIM:
        {
            StgPtr end;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                evacuate((StgClosure **)p);
            }
            break;
        }

        case BCO: {
            StgBCO *bco = (StgBCO *)p;
            evacuate((StgClosure **)&bco->instrs);
            evacuate((StgClosure **)&bco->literals);
            evacuate((StgClosure **)&bco->ptrs);
            break;
        }

        case IND:
        case BLACKHOLE:
            evacuate(&((StgInd *)p)->indirectee);
            break;

        case MUT_VAR_CLEAN:
        case MUT_VAR_DIRTY: {
            gct->eager_promotion = false;
            evacuate(&((StgMutVar *)p)->var);
            gct->eager_promotion = saved_eager_promotion;

            if (gct->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
            }
            break;
        }

        case BLOCKING_QUEUE:
        {
            StgBlockingQueue *bq = (StgBlockingQueue *)p;

            gct->eager_promotion = false;
            evacuate(&bq->bh);
            evacuate((StgClosure**)&bq->owner);
            evacuate((StgClosure**)&bq->queue);
            evacuate((StgClosure**)&bq->link);
            gct->eager_promotion = saved_eager_promotion;

            if (gct->failed_to_evac) {
                bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
            } else {
                bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
            }
            break;
        }

        case ARR_WORDS:
            break;

        case THUNK_SELECTOR:
        {
            StgSelector *s = (StgSelector *)p;
            evacuate(&s->selectee);
            break;
        }

        // A chunk of stack saved in a heap object
        case AP_STACK:
        {
            StgAP_STACK *ap = (StgAP_STACK *)p;

            evacuate(&ap->fun);
            traverse_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
            break;
        }

        case PAP:
            traverse_PAP((StgPAP *)p);
            break;

        case AP:
            traverse_AP((StgAP *)p);
            break;

        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
            // follow everything
        {
            // We don't eagerly promote objects pointed to by a mutable
            // array, but if we find the array only points to objects in
            // the same or an older generation, we mark it "clean" and
            // avoid traversing it during minor GCs.
            gct->eager_promotion = false;

            traverse_mut_arr_ptrs((StgMutArrPtrs *)p);

            if (gct->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
            }

            gct->eager_promotion = saved_eager_promotion;
            gct->failed_to_evac = true; // mutable anyhow.
            break;
        }

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            // follow everything
        {
            StgPtr q = p;

            traverse_mut_arr_ptrs((StgMutArrPtrs *)p);

            // If we're going to put this object on the mutable list, then
            // set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
            if (gct->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
            }
            break;
        }

        case SMALL_MUT_ARR_PTRS_CLEAN:
        case SMALL_MUT_ARR_PTRS_DIRTY:
            // follow everything
        {
            StgPtr next;
            bool saved_eager;

            // We don't eagerly promote objects pointed to by a mutable
            // array, but if we find the array only points to objects in
            // the same or an older generation, we mark it "clean" and
            // avoid traversing it during minor GCs.
            saved_eager = gct->eager_promotion;
            gct->eager_promotion = false;
            next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
            for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
                evacuate((StgClosure **)p);
            }
            gct->eager_promotion = saved_eager;

            if (gct->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_DIRTY_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_CLEAN_info;
            }

            gct->failed_to_evac = true; // mutable anyhow.
            break;
        }

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
            // follow everything
        {
            StgPtr next, q = p;

            next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
            for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
                evacuate((StgClosure **)p);
            }

            // If we're going to put this object on the mutable list, then
            // set its info ptr to SMALL_MUT_ARR_PTRS_FROZEN0 to indicate that.
            if (gct->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN0_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_info;
            }
            break;
        }

        case TSO:
        {
            traverseTSO((StgTSO*)p);
            break;
        }

        case STACK:
        {
            StgStack *stack = (StgStack*)p;

            gct->eager_promotion = false;

            traverse_stack(stack->sp, stack->stack + stack->stack_size);
            stack->dirty = gct->failed_to_evac;

            gct->eager_promotion = saved_eager_promotion;
            break;
        }

        case MUT_PRIM:
        {
            StgPtr end;

            gct->eager_promotion = false;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                evacuate((StgClosure **)p);
            }

            gct->eager_promotion = saved_eager_promotion;
            gct->failed_to_evac = true; // mutable
            break;
        }

        case TREC_CHUNK:
          {
            StgWord i;
            StgTRecChunk *tc = ((StgTRecChunk *) p);
            TRecEntry *e = &(tc -> entries[0]);
            gct->eager_promotion = false;
            evacuate((StgClosure **)&tc->prev_chunk);
            for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
              evacuate((StgClosure **)&e->tvar);
              evacuate((StgClosure **)&e->expected_value);
              evacuate((StgClosure **)&e->new_value);
            }
            gct->eager_promotion = saved_eager_promotion;
            gct->failed_to_evac = true; // mutable
            break;
          }

        default:
            barf("traverse_mark_stack: unimplemented/strange closure type %d @ %p",
                 info->type, p);
        }

        if (gct->failed_to_evac) {
            gct->failed_to_evac = false;
            if (gct->evac_gen_no) {
                recordMutableGen_GC((StgClosure *)q, gct->evac_gen_no);
            }
        }
    } // while (p = pop_mark_stack())
}

/* -----------------------------------------------------------------------------
   Traverse one object.

   This is used for objects that are temporarily marked as mutable
   because they contain old-to-new generation pointers.  Only certain
   objects can have this property.
   -------------------------------------------------------------------------- */

static bool
traverse_one(StgPtr p)
{
    const StgInfoTable *info;
    bool no_luck;
    bool saved_eager_promotion;

    saved_eager_promotion = gct->eager_promotion;

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);

    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = ((StgMVar *)p);
        gct->eager_promotion = false;
        evacuate((StgClosure **)&mvar->head);
        evacuate((StgClosure **)&mvar->tail);
        evacuate((StgClosure **)&mvar->value);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            mvar->header.info = &stg_MVAR_DIRTY_info;
        } else {
            mvar->header.info = &stg_MVAR_CLEAN_info;
        }
        break;
    }

    case TVAR:
    {
        StgTVar *tvar = ((StgTVar *)p);
        gct->eager_promotion = false;
        evacuate((StgClosure **)&tvar->current_value);
        evacuate((StgClosure **)&tvar->first_watch_queue_entry);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            tvar->header.info = &stg_TVAR_DIRTY_info;
        } else {
            tvar->header.info = &stg_TVAR_CLEAN_info;
        }
        break;
    }

    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    {
        StgPtr q, end;

        end = (StgPtr)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (q = (StgPtr)((StgThunk *)p)->payload; q < end; q++) {
            evacuate((StgClosure **)q);
        }
        break;
    }

    case FUN:
    case FUN_1_0:                       // hardly worth specialising these guys
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case PRIM:
    {
        StgPtr q, end;

        end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (q = (StgPtr)((StgClosure *)p)->payload; q < end; q++) {
            evacuate((StgClosure **)q);
        }
        break;
    }

/*
    case WEAK:
        // This WEAK object will not be considered by tidyWeakList during this
        // collection because it is in a generation >= N, but it is on the
        // mutable list so we must evacuate all of its pointers because some
        // of them may point into a younger generation.
        traverseLiveWeak((StgWeak *)p);
        break;
*/

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
        StgPtr q = p;

        gct->eager_promotion = false;
        evacuate(&((StgMutVar *)p)->var);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
        }
        break;
    }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        gct->eager_promotion = false;
        evacuate(&bq->bh);
        evacuate((StgClosure**)&bq->owner);
        evacuate((StgClosure**)&bq->queue);
        evacuate((StgClosure**)&bq->link);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
        } else {
            bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
        }
        break;
    }

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        evacuate(&s->selectee);
        break;
    }

    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)p;

        evacuate(&ap->fun);
        traverse_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
        p = (StgPtr)ap->payload + ap->size;
        break;
    }

    case PAP:
        p = traverse_PAP((StgPAP *)p);
        break;

    case AP:
        p = traverse_AP((StgAP *)p);
        break;

    case ARR_WORDS:
        // nothing to follow
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        gct->eager_promotion = false;

        traverse_mut_arr_ptrs((StgMutArrPtrs *)p);

        if (gct->failed_to_evac) {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
        }

        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true;
        break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
    {
        // follow everything
        traverse_mut_arr_ptrs((StgMutArrPtrs *)p);

        // If we're going to put this object on the mutable list, then
        // set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
        if (gct->failed_to_evac) {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
        } else {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
        }
        break;
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    {
        StgPtr next, q;
        bool saved_eager;

        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        saved_eager = gct->eager_promotion;
        gct->eager_promotion = false;
        q = p;
        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }
        gct->eager_promotion = saved_eager;

        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_CLEAN_info;
        }

        gct->failed_to_evac = true;
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
    {
        // follow everything
        StgPtr next, q=p;

        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }

        // If we're going to put this object on the mutable list, then
        // set its info ptr to SMALL_MUT_ARR_PTRS_FROZEN0 to indicate that.
        if (gct->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN0_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_info;
        }
        break;
    }

    case TSO:
    {
        traverseTSO((StgTSO*)p);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        gct->eager_promotion = false;

        traverse_stack(stack->sp, stack->stack + stack->stack_size);
        stack->dirty = gct->failed_to_evac;

        gct->eager_promotion = saved_eager_promotion;
        break;
    }

    case MUT_PRIM:
    {
        StgPtr end;

        gct->eager_promotion = false;

        end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            evacuate((StgClosure **)p);
        }

        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true; // mutable
        break;

    }

    case TREC_CHUNK:
      {
        StgWord i;
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        TRecEntry *e = &(tc -> entries[0]);
        gct->eager_promotion = false;
        evacuate((StgClosure **)&tc->prev_chunk);
        for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          evacuate((StgClosure **)&e->tvar);
          evacuate((StgClosure **)&e->expected_value);
          evacuate((StgClosure **)&e->new_value);
        }
        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true; // mutable
        break;
      }

    case IND:
        // IND can happen, for example, when the interpreter allocates
        // a gigantic AP closure (more than one block), which ends up
        // on the large-object list and then gets updated.  See #3424.
    case BLACKHOLE:
    case IND_STATIC:
        evacuate(&((StgInd *)p)->indirectee);

#if 0 && defined(DEBUG)
      if (RtsFlags.DebugFlags.gc)
      /* Debugging code to print out the size of the thing we just
       * promoted
       */
      {
        StgPtr start = gen->scan;
        bdescr *start_bd = gen->scan_bd;
        StgWord size = 0;
        traverse(&gen);
        if (start_bd != gen->scan_bd) {
          size += (P_)BLOCK_ROUND_UP(start) - start;
          start_bd = start_bd->link;
          while (start_bd != gen->scan_bd) {
            size += BLOCK_SIZE_W;
            start_bd = start_bd->link;
          }
          size += gen->scan -
            (P_)BLOCK_ROUND_DOWN(gen->scan);
        } else {
          size = gen->scan - start;
        }
        debugBelch("evac IND: %ld bytes", size * sizeof(W_));
      }
#endif
      break;

    case COMPACT_NFDATA:
        traverse_compact((StgCompactNFData*)p);
        break;

    default:
        barf("traverse_one: strange object %d", (int)(info->type));
    }

    no_luck = gct->failed_to_evac;
    gct->failed_to_evac = false;
    return (no_luck);
}

/* -----------------------------------------------------------------------------
   Scavenging mutable lists.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

static void
traverse_mutable_list(bdescr *bd, generation *gen)
{
    StgPtr p, q;
    uint32_t gen_no;

    gen_no = gen->no;
    gct->evac_gen_no = gen_no;
    for (; bd != NULL; bd = bd->link) {
        for (q = bd->start; q < bd->free; q++) {
            p = (StgPtr)*q;
            ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

#ifdef DEBUG
            switch (get_itbl((StgClosure *)p)->type) {
            case MUT_VAR_CLEAN:
                // can happen due to concurrent writeMutVars
            case MUT_VAR_DIRTY:
                mutlist_MUTVARS++; break;
            case MUT_ARR_PTRS_CLEAN:
            case MUT_ARR_PTRS_DIRTY:
            case MUT_ARR_PTRS_FROZEN:
            case MUT_ARR_PTRS_FROZEN0:
                mutlist_MUTARRS++; break;
            case MVAR_CLEAN:
                barf("MVAR_CLEAN on mutable list");
            case MVAR_DIRTY:
                mutlist_MVARS++; break;
            case TVAR:
                mutlist_TVAR++; break;
            case TREC_CHUNK:
                mutlist_TREC_CHUNK++; break;
            case MUT_PRIM:
                if (((StgClosure*)p)->header.info == &stg_TVAR_WATCH_QUEUE_info)
                    mutlist_TVAR_WATCH_QUEUE++;
                else if (((StgClosure*)p)->header.info == &stg_TREC_HEADER_info)
                    mutlist_TREC_HEADER++;
                else if (((StgClosure*)p)->header.info == &stg_ATOMIC_INVARIANT_info)
                    mutlist_ATOMIC_INVARIANT++;
                else if (((StgClosure*)p)->header.info == &stg_INVARIANT_CHECK_QUEUE_info)
                    mutlist_INVARIANT_CHECK_QUEUE++;
                else
                    mutlist_OTHERS++;
                break;
            default:
                mutlist_OTHERS++; break;
            }
#endif

            // Check whether this object is "clean", that is it
            // definitely doesn't point into a young generation.
            // Clean objects don't need to be traversed.  Some clean
            // objects (MUT_VAR_CLEAN) are not kept on the mutable
            // list at all; others, such as MUT_ARR_PTRS
            // are always on the mutable list.
            //
            switch (get_itbl((StgClosure *)p)->type) {
            case MUT_ARR_PTRS_CLEAN:
                recordMutableGen_GC((StgClosure *)p,gen_no);
                continue;
            case MUT_ARR_PTRS_DIRTY:
            {
                bool saved_eager_promotion;
                saved_eager_promotion = gct->eager_promotion;
                gct->eager_promotion = false;

                traverse_mut_arr_ptrs_marked((StgMutArrPtrs *)p);

                if (gct->failed_to_evac) {
                    ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
                } else {
                    ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
                }

                gct->eager_promotion = saved_eager_promotion;
                gct->failed_to_evac = false;
                recordMutableGen_GC((StgClosure *)p,gen_no);
                continue;
            }
            default:
                ;
            }

            if (traverse_one(p)) {
                // didn't manage to promote everything, so put the
                // object back on the list.
                recordMutableGen_GC((StgClosure *)p,gen_no);
            }
        }
    }
}

void
traverse_capability_mut_lists (Capability *cap)
{
    uint32_t g;

    /* Mutable lists from each generation > N
     * we want to *traverse* these roots, not evacuate them: they're not
     * going to move in this GC.
     * Also do them in reverse generation order, for the usual reason:
     * namely to reduce the likelihood of spurious old->new pointers.
     */
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
        traverse_mutable_list(cap->saved_mut_lists[g], &generations[g]);
        freeChain_sync(cap->saved_mut_lists[g]);
        cap->saved_mut_lists[g] = NULL;
    }
}

/* -----------------------------------------------------------------------------
   Scavenging the static objects.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

static void
traverse_static(void)
{
  StgClosure *flagged_p, *p;
  const StgInfoTable *info;

  debugTrace(DEBUG_gc, "scavenging static objects");

  /* Always evacuate straight to the oldest generation for static
   * objects */
  gct->evac_gen_no = oldest_gen->no;

  /* keep going until we've traversed all the objects on the linked
     list... */

  while (1) {

    /* get the next static object from the list.  Remember, there might
     * be more stuff on this list after each evacuation...
     * (static_objects is a global)
     */
    flagged_p = gct->static_objects;
    if (flagged_p == END_OF_STATIC_OBJECT_LIST) {
          break;
    }
    p = UNTAG_STATIC_LIST_PTR(flagged_p);

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl(p);
    // make sure the info pointer is into text space

    /* Take this object *off* the static_objects list,
     * and put it on the scavenged_static_objects list.
     */
    gct->static_objects = *STATIC_LINK(info,p);
    *STATIC_LINK(info,p) = gct->scavenged_static_objects;
    gct->scavenged_static_objects = flagged_p;

    switch (info -> type) {

    case IND_STATIC:
      {
        StgInd *ind = (StgInd *)p;
        evacuate(&ind->indirectee);

        /* might fail to evacuate it, in which case we have to pop it
         * back on the mutable list of the oldest generation.  We
         * leave it *on* the scavenged_static_objects list, though,
         * in case we visit this object again.
         */
        if (gct->failed_to_evac) {
          gct->failed_to_evac = false;
          recordMutableGen_GC((StgClosure *)p,oldest_gen->no);
        }
        break;
      }

    case THUNK_STATIC:
      traverse_thunk_srt(info);
      break;

    case FUN_STATIC:
      traverse_fun_srt(info);
      break;

    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
      {
        StgPtr q, next;

        next = (P_)p->payload + info->layout.payload.ptrs;
        // evacuate the pointers
        for (q = (P_)p->payload; q < next; q++) {
            evacuate((StgClosure **)q);
        }
        break;
      }

    default:
      barf("traverse_static: strange closure %d", (int)(info->type));
    }

    ASSERT(gct->failed_to_evac == false);
  }
}

/* -----------------------------------------------------------------------------
   traverse a chunk of memory described by a bitmap
   -------------------------------------------------------------------------- */

static void
traverse_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, StgWord size )
{
    uint32_t i, j, b;
    StgWord bitmap;

    b = 0;

    for (i = 0; i < size; b++) {
        bitmap = large_bitmap->bitmap[b];
        j = stg_min(size-i, BITS_IN(W_));
        i += j;
        for (; j > 0; j--, p++) {
            if ((bitmap & 1) == 0) {
                evacuate((StgClosure **)p);
            }
            bitmap = bitmap >> 1;
        }
    }
}


/* -----------------------------------------------------------------------------
   traverse_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   AP_STACK_UPDs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */

static void
traverse_stack(StgPtr p, StgPtr stack_end)
{
  const StgRetInfoTable* info;
  StgWord bitmap;
  StgWord size;

  /*
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with an activation record.
   */

  while (p < stack_end) {
    info  = get_ret_itbl((StgClosure *)p);

    switch (info->i.type) {

    case UPDATE_FRAME:
        // Note [upd-black-hole]
        // In SMP, we can get update frames that point to indirections
        // when two threads evaluate the same thunk.  We do attempt to
        // discover this situation in threadPaused(), but it's
        // possible that the following sequence occurs:
        //
        //        A             B
        //                  enter T
        //     enter T
        //     blackhole T
        //                  update T
        //     GC
        //
        // Now T is an indirection, and the update frame is already
        // marked on A's stack, so we won't traverse it again in
        // threadPaused().  We could traverse the whole stack again
        // before GC, but that seems like overkill.
        //
        // Scavenging this update frame as normal would be disastrous;
        // the updatee would end up pointing to the value.  So we
        // check whether the value after evacuation is a BLACKHOLE,
        // and if not, we change the update frame to an stg_enter
        // frame that simply returns the value.  Hence, blackholing is
        // compulsory (otherwise we would have to check for thunks
        // too).
        //
        // One slight hiccup is that the THUNK_SELECTOR machinery can
        // overwrite the updatee with an IND.  In parallel GC, this
        // could even be happening concurrently, so we can't check for
        // the IND.  Fortunately if we assume that blackholing is
        // happening (either lazy or eager), then we can be sure that
        // the updatee is never a THUNK_SELECTOR and we're ok.
        // NB. this is a new invariant: blackholing is not optional.
    {
        StgUpdateFrame *frame = (StgUpdateFrame *)p;
        StgClosure *v;

        evacuate(&frame->updatee);
        v = frame->updatee;
        if (GET_CLOSURE_TAG(v) != 0 ||
            (get_itbl(v)->type != BLACKHOLE)) {
            // blackholing is compulsory, see above.
            frame->header.info = (const StgInfoTable*)&stg_enter_checkbh_info;
        }
        ASSERT(v->header.info != &stg_TSO_info);
        p += sizeofW(StgUpdateFrame);
        continue;
    }

      // small bitmap (< 32 entries, or 64 on a 64-bit machine)
    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case CATCH_FRAME:
    case RET_SMALL:
        bitmap = BITMAP_BITS(info->i.layout.bitmap);
        size   = BITMAP_SIZE(info->i.layout.bitmap);
        // NOTE: the payload starts immediately after the info-ptr, we
        // don't have an StgHeader in the same sense as a heap closure.
        p++;
        p = traverse_small_bitmap(p, size, bitmap);

    follow_srt:
        if (major_gc)
            traverse_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap);
        continue;

    case RET_BCO: {
        StgBCO *bco;
        StgWord size;

        p++;
        evacuate((StgClosure **)p);
        bco = (StgBCO *)*p;
        p++;
        size = BCO_BITMAP_SIZE(bco);
        traverse_large_bitmap(p, BCO_BITMAP(bco), size);
        p += size;
        continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
    case RET_BIG:
    {
        StgWord size;

        size = GET_LARGE_BITMAP(&info->i)->size;
        p++;
        traverse_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
        p += size;
        // and don't forget to follow the SRT
        goto follow_srt;
    }

    case RET_FUN:
    {
        StgRetFun *ret_fun = (StgRetFun *)p;
        const StgFunInfoTable *fun_info;

        evacuate(&ret_fun->fun);
        fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
        p = traverse_arg_block(fun_info, ret_fun->payload);
        goto follow_srt;
    }

    default:
        barf("traverse_stack: weird activation record found on stack: %d", (int)(info->i.type));
    }
  }
}

/*-----------------------------------------------------------------------------
  traverse the large object list.

  evac_gen set by caller; similar games played with evac_gen as with
  traverse() - see comment at the top of traverse().  Most large
  objects are (repeatedly) mutable, so most of the time evac_gen will
  be zero.
  --------------------------------------------------------------------------- */

static void
traverse_large (gen_workspace *ws)
{
    bdescr *bd;
    StgPtr p;

    gct->evac_gen_no = ws->gen->no;

    bd = ws->todo_large_objects;

    for (; bd != NULL; bd = ws->todo_large_objects) {

        // take this object *off* the large objects list and put it on
        // the traversed large objects list.  This is so that we can
        // treat todo_large_objects as a stack and push new objects on
        // the front when evacuating.
        ws->todo_large_objects = bd->link;

        ACQUIRE_SPIN_LOCK(&ws->gen->sync);
        if (bd->flags & BF_COMPACT) {
            dbl_link_onto(bd, &ws->gen->live_compact_objects);
            StgCompactNFData *str = ((StgCompactNFDataBlock*)bd->start)->owner;
            ws->gen->n_live_compact_blocks += str->totalW / BLOCK_SIZE_W;
            p = (StgPtr)str;
        } else {
            dbl_link_onto(bd, &ws->gen->scavenged_large_objects);
            ws->gen->n_scavenged_large_blocks += bd->blocks;
            p = bd->start;
        }
        RELEASE_SPIN_LOCK(&ws->gen->sync);

        if (traverse_one(p)) {
            if (ws->gen->no > 0) {
                recordMutableGen_GC((StgClosure *)p, ws->gen->no);
            }
        }

        // stats
        gct->scanned += closure_sizeW((StgClosure*)p);
    }
}

/* ----------------------------------------------------------------------------
   Look for work to do.

   We look for the oldest gen that has either a todo block that can
   be scanned, or a block of work on the global queue that we can
   scan.

   It is important to take work from the *oldest* generation that we
   has work available, because that minimizes the likelihood of
   evacuating objects into a young generation when they should have
   been eagerly promoted.  This really does make a difference (the
   cacheprof benchmark is one that is affected).

   We also want to scan the todo block if possible before grabbing
   work from the global queue, the reason being that we don't want to
   steal work from the global queue and starve other threads if there
   is other work we can usefully be doing.
   ------------------------------------------------------------------------- */

static bool
traverse_find_work (void)
{
    int g;
    gen_workspace *ws;
    bool did_something, did_anything;
    bdescr *bd;

    gct->scav_find_work++;

    did_anything = false;

loop:
    did_something = false;
    for (g = RtsFlags.GcFlags.generations-1; g >= 0; g--) {
        ws = &gct->gens[g];

        gct->scan_bd = NULL;

        // If we have a scan block with some work to do,
        // traverse everything up to the free pointer.
        if (ws->todo_bd->u.scan < ws->todo_free)
        {
            traverse_block(ws->todo_bd);
            did_something = true;
            break;
        }

        // If we have any large objects to traverse, do them now.
        if (ws->todo_large_objects) {
            traverse_large(ws);
            did_something = true;
            break;
        }

        if ((bd = grab_local_todo_block(ws)) != NULL) {
            traverse_block(bd);
            did_something = true;
            break;
        }
    }

    if (did_something) {
        did_anything = true;
        goto loop;
    }

#if defined(THREADED_RTS)
    if (work_stealing) {
        // look for work to steal
        for (g = RtsFlags.GcFlags.generations-1; g >= 0; g--) {
            if ((bd = steal_todo_block(g)) != NULL) {
                traverse_block(bd);
                did_something = true;
                break;
            }
        }

        if (did_something) {
            did_anything = true;
            goto loop;
        }
    }
#endif

    // only return when there is no more work to do

    return did_anything;
}

/* ----------------------------------------------------------------------------
   Traverse until we can't find anything more to traverse.
   ------------------------------------------------------------------------- */

void
traverse_loop(void)
{
    bool work_to_do;

loop:
    work_to_do = false;

    // traverse static objects
    if (major_gc && gct->static_objects != END_OF_STATIC_OBJECT_LIST) {
        IF_DEBUG(sanity, checkStaticObjects(gct->static_objects));
        traverse_static();
    }

    // traverse objects in compacted generation
    if (mark_stack_bd != NULL && !mark_stack_empty()) {
        traverse_mark_stack();
        work_to_do = true;
    }

    // Order is important here: we want to deal in full blocks as
    // much as possible, so go for global work in preference to
    // local work.  Only if all the global work has been exhausted
    // do we start scavenging the fragments of blocks in the local
    // workspaces.
    if (traverse_find_work()) goto loop;

    if (work_to_do) goto loop;
}
