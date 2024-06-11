/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Scavenging means reading already copied (evacuated) objects and evactuating
 * any pointers the object holds and updating the pointers to their new
 * locations.
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

/* ----------------------------------------------------------------------------
   We have two main scavenge functions:

   - scavenge_block(bdescr *bd)
   - scavenge_one(StgPtr p)

   As the names and parameters suggest, first one scavenges a whole block while
   the second one only scavenges one object. This however is not the only
   difference. scavenge_block scavenges all SRTs while scavenge_one only
   scavenges SRTs of stacks. The reason is because scavenge_one is called in two
   cases:

   - When scavenging a mut_list
   - When scavenging a large object

   We don't have to scavenge SRTs when scavenging a mut_list, because we only
   scavenge mut_lists in minor GCs, and static objects are only collected in
   major GCs.

   However, because scavenge_one is also used to scavenge large objects (which
   are scavenged even in major GCs), we need to deal with SRTs of large
   objects. We never allocate large FUNs and THUNKs, but we allocate large
   STACKs (e.g. in threadStackOverflow), and stack frames can have SRTs. So
   scavenge_one skips FUN and THUNK SRTs but scavenges stack frame SRTs.

   In summary, in a major GC:

   - scavenge_block() scavenges all SRTs
   - scavenge_one() scavenges only stack frame SRTs
   ------------------------------------------------------------------------- */

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "GC.h"
#include "GCThread.h"
#include "GCUtils.h"
#include "Compact.h"
#include "MarkStack.h"
#include "Evac.h"
#include "Scav.h"
#include "Apply.h"
#include "Trace.h"
#include "Sanity.h"
#include "Capability.h"
#include "IOManager.h"
#include "LdvProfile.h"
#include "HeapUtils.h"
#include "Hash.h"

#include "sm/MarkWeak.h"
#include "sm/NonMoving.h" // for nonmoving_set_closure_mark_bit
#include "sm/NonMovingScav.h"

#include <string.h> /* for memset */

static void scavenge_large_bitmap (StgPtr p,
                                   StgLargeBitmap *large_bitmap,
                                   StgWord size );

#if defined(THREADED_RTS) && !defined(PARALLEL_GC)
# define evacuate(a) evacuate1(a)
# define evacuate_BLACKHOLE(a) evacuate_BLACKHOLE1(a)
# define scavenge_loop(a) scavenge_loop1(a)
# define scavenge_block(a) scavenge_block1(a)
# define scavenge_mutable_list(bd,g) scavenge_mutable_list1(bd,g)
# define scavenge_capability_mut_lists(cap) scavenge_capability_mut_Lists1(cap)
# define scavengeTSO(tso) scavengeTSO1(tso)
# define scavenge_stack(p, stack_end) scavenge_stack1(p, stack_end)
# define scavenge_fun_srt(info) scavenge_fun_srt1(info)
# define scavenge_fun_srt(info) scavenge_fun_srt1(info)
# define scavenge_thunk_srt(info) scavenge_thunk_srt1(info)
# define scavenge_mut_arr_ptrs(info) scavenge_mut_arr_ptrs1(info)
# define scavenge_PAP(pap) scavenge_PAP1(pap)
# define scavenge_AP(ap) scavenge_AP1(ap)
# define scavenge_continuation(pap) scavenge_continuation1(pap)
# define scavenge_compact(str) scavenge_compact1(str)
#endif

static void do_evacuate(StgClosure **p, void *user STG_UNUSED)
{
    evacuate(p);
}

/* -----------------------------------------------------------------------------
   Scavenge a TSO.
   -------------------------------------------------------------------------- */

void
scavengeTSO (StgTSO *tso)
{
    bool saved_eager;

    debugTrace(DEBUG_gc,"scavenging thread %" FMT_StgThreadID,tso->id);

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

    if (tso->label != NULL) {
        evacuate((StgClosure **)&tso->label);
    }

    switch (ACQUIRE_LOAD(&tso->why_blocked)) {
    case BlockedOnMVar:
    case BlockedOnMVarRead:
    case BlockedOnBlackHole:
    case BlockedOnMsgThrowTo:
    case NotBlocked:
        evacuate(&tso->block_info.closure);
        break;
    case BlockedOnRead:
    case BlockedOnWrite:
    case BlockedOnDelay:
    case BlockedOnDoProc:
        scavengeTSOIOManager(tso);
        break;
    default:
#if defined(THREADED_RTS)
    // in the THREADED_RTS, block_info.closure must always point to a
    // valid closure, because we assume this in throwTo().  In the
    // non-threaded RTS it might be a FD (for
    // BlockedOnRead/BlockedOnWrite) or a time value (BlockedOnDelay)
        tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
#endif
        break;
    }

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
#if defined(THREADED_RTS)
    gc_thread *old_gct = gct;
#endif

    SET_GCT(dat->saved_gct);
    evacuate(&p);
    insertHashTable(dat->newHash, (StgWord)p, value);
    SET_GCT(old_gct);
}

/* Here we scavenge the sharing-preservation hash-table, which may contain keys
 * living in from-space.
 */
void
scavenge_compact(StgCompactNFData *str)
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
        RELEASE_STORE(&((StgClosure *)str)->header.info, &stg_COMPACT_NFDATA_DIRTY_info);
    } else {
        RELEASE_STORE(&((StgClosure *)str)->header.info, &stg_COMPACT_NFDATA_CLEAN_info);
    }
}

/* -----------------------------------------------------------------------------
   Mutable arrays of pointers
   -------------------------------------------------------------------------- */

StgPtr scavenge_mut_arr_ptrs (StgMutArrPtrs *a)
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

// scavenge only the marked areas of a MUT_ARR_PTRS
static StgPtr scavenge_mut_arr_ptrs_marked (StgMutArrPtrs *a)
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
scavenge_small_bitmap (StgPtr p, StgWord size, StgWord bitmap)
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
scavenge_arg_block (const StgFunInfoTable *fun_info, StgClosure **args)
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
        scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = scavenge_small_bitmap(p, size, bitmap);
        break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
scavenge_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size)
{
    StgPtr p;
    StgWord bitmap;
    const StgFunInfoTable *fun_info;

    fun = UNTAG_CLOSURE(fun);
    fun_info = get_fun_itbl(fun);
    ASSERT(fun_info->i.type != PAP);
    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    case ARG_BCO:
        scavenge_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = scavenge_small_bitmap(p, size, bitmap);
        break;
    }
    return p;
}

GNUC_ATTR_HOT StgPtr
scavenge_PAP (StgPAP *pap)
{
    evacuate(&pap->fun);
    return scavenge_PAP_payload (pap->fun, pap->payload, pap->n_args);
}

StgPtr
scavenge_AP (StgAP *ap)
{
    evacuate(&ap->fun);
    return scavenge_PAP_payload (ap->fun, ap->payload, ap->n_args);
}

StgPtr
scavenge_continuation(StgContinuation *cont)
{
    scavenge_stack(cont->stack, cont->stack + cont->stack_size);
    return (StgPtr)cont + continuation_sizeW(cont);
}

/* -----------------------------------------------------------------------------
   Scavenge SRTs
   -------------------------------------------------------------------------- */

GNUC_ATTR_HOT void
scavenge_thunk_srt(const StgInfoTable *info)
{
    StgThunkInfoTable *thunk_info;

    if (!major_gc) return;

    thunk_info = itbl_to_thunk_itbl(info);
    if (thunk_info->i.srt) {
        StgClosure *srt = (StgClosure*)GET_SRT(thunk_info);
        evacuate(&srt);
    }
}

GNUC_ATTR_HOT void
scavenge_fun_srt(const StgInfoTable *info)
{
    StgFunInfoTable *fun_info;

    if (!major_gc) return;

    fun_info = itbl_to_fun_itbl(info);
    if (fun_info->i.srt) {
        StgClosure *srt = (StgClosure*)GET_FUN_SRT(fun_info);
        evacuate(&srt);
    }
}

/* -----------------------------------------------------------------------------
   Scavenge a block from the given scan pointer up to bdescr_free(bd).

   evac_gen_no is set by the caller to be either zero (for a step in a
   generation < N) or G where G is the generation of the step being
   scavenged.

   We sometimes temporarily change evac_gen_no back to zero if we're
   scavenging a mutable object where eager promotion isn't such a good
   idea.
   -------------------------------------------------------------------------- */

static GNUC_ATTR_HOT void
scavenge_block (bdescr *bd)
{
  StgPtr p, q;
  const StgInfoTable *info;
  bool saved_eager_promotion;
  gen_workspace *ws;

  debugTrace(DEBUG_gc, "scavenging block %p (gen %d) @ %p",
             bdescr_start(bd), bd->gen_no, bd->u.scan);

  gct->scan_bd = bd;
  gct->evac_gen_no = bd->gen_no;
  saved_eager_promotion = gct->eager_promotion;
  gct->failed_to_evac = false;

  ws = &gct->gens[bd->gen_no];

  p = bd->u.scan;

  // Sanity check: See Note [Deadlock detection under the nonmoving collector].
#if defined(DEBUG)
  if (RtsFlags.GcFlags.useNonmoving && deadlock_detect_gc) {
      ASSERT(bd->gen_no == oldest_gen->no);
  }
#endif


  // we might be evacuating into the very object that we're
  // scavenging, so we have to check the real bdescr_free(bd) pointer each
  // time around the loop.
  while (p < bdescr_free(bd) || (bd == ws->todo_bd && p < ws->todo_free)) {

    ASSERT(bd->link == NULL);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);

    ASSERT(gct->thunk_selector_depth == 0);

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
            RELEASE_STORE(&mvar->header.info, &stg_MVAR_DIRTY_info);
        } else {
            RELEASE_STORE(&mvar->header.info, &stg_MVAR_CLEAN_info);
        }
        p += sizeofW(StgMVar);
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
            RELEASE_STORE(&tvar->header.info, &stg_TVAR_DIRTY_info);
        } else {
            RELEASE_STORE(&tvar->header.info, &stg_TVAR_CLEAN_info);
        }
        p += sizeofW(StgTVar);
        break;
    }

    case FUN_2_0:
        scavenge_fun_srt(info);
        evacuate(&((StgClosure *)p)->payload[1]);
        evacuate(&((StgClosure *)p)->payload[0]);
        p += sizeofW(StgHeader) + 2;
        break;

    case THUNK_2_0:
        scavenge_thunk_srt(info);
        evacuate(&((StgThunk *)p)->payload[1]);
        evacuate(&((StgThunk *)p)->payload[0]);
        p += sizeofW(StgThunk) + 2;
        break;

    case CONSTR_2_0:
        evacuate(&((StgClosure *)p)->payload[1]);
        evacuate(&((StgClosure *)p)->payload[0]);
        p += sizeofW(StgHeader) + 2;
        break;

    case THUNK_1_0:
        scavenge_thunk_srt(info);
        evacuate(&((StgThunk *)p)->payload[0]);
        p += sizeofW(StgThunk) + 1;
        break;

    case FUN_1_0:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_1_0:
        evacuate(&((StgClosure *)p)->payload[0]);
        p += sizeofW(StgHeader) + 1;
        break;

    case THUNK_0_1:
        scavenge_thunk_srt(info);
        p += sizeofW(StgThunk) + 1;
        break;

    case FUN_0_1:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_0_1:
        p += sizeofW(StgHeader) + 1;
        break;

    case THUNK_0_2:
        scavenge_thunk_srt(info);
        p += sizeofW(StgThunk) + 2;
        break;

    case FUN_0_2:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_0_2:
        p += sizeofW(StgHeader) + 2;
        break;

    case THUNK_1_1:
        scavenge_thunk_srt(info);
        evacuate(&((StgThunk *)p)->payload[0]);
        p += sizeofW(StgThunk) + 2;
        break;

    case FUN_1_1:
        scavenge_fun_srt(info);
        FALLTHROUGH;
    case CONSTR_1_1:
        evacuate(&((StgClosure *)p)->payload[0]);
        p += sizeofW(StgHeader) + 2;
        break;

    case FUN:
        scavenge_fun_srt(info);
        goto gen_obj;

    case THUNK:
    {
        StgPtr end;

        scavenge_thunk_srt(info);
        end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
            evacuate((StgClosure **)p);
        }
        p += info->layout.payload.nptrs;
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
        p += info->layout.payload.nptrs;
        break;
    }

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        evacuate((StgClosure **)&bco->instrs);
        evacuate((StgClosure **)&bco->literals);
        evacuate((StgClosure **)&bco->ptrs);
        p += bco_sizeW(bco);
        break;
    }

    case BLACKHOLE:
        evacuate(&((StgInd *)p)->indirectee);
        p += sizeofW(StgInd);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        gct->eager_promotion = false;
        evacuate(&((StgMutVar *)p)->var);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_VAR_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_VAR_CLEAN_info);
        }
        p += sizeofW(StgMutVar);
        break;

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
            RELEASE_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_DIRTY_info);
        } else {
            RELEASE_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_CLEAN_info);
        }
        p += sizeofW(StgBlockingQueue);
        break;
    }

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        evacuate(&s->selectee);
        p += THUNK_SELECTOR_sizeW();
        break;
    }

    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)p;

        evacuate(&ap->fun);
        scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
        p = (StgPtr)ap->payload + ap->size;
        break;
    }

    case PAP:
        p = scavenge_PAP((StgPAP *)p);
        break;

    case AP:
        p = scavenge_AP((StgAP *)p);
        break;

    case ARR_WORDS:
        // nothing to follow
        p += arr_words_sizeW((StgArrBytes *)p);
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        gct->eager_promotion = false;

        p = scavenge_mut_arr_ptrs((StgMutArrPtrs*)p);

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_CLEAN_info);
        }

        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true; // always put it on the mutable list.
        break;
    }

    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        // follow everything
    {
        p = scavenge_mut_arr_ptrs((StgMutArrPtrs*)p);

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
        }
        break;
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
        // follow everything
    {
        StgPtr next;

        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        gct->eager_promotion = false;
        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_SMALL_MUT_ARR_PTRS_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_SMALL_MUT_ARR_PTRS_CLEAN_info);
        }

        gct->failed_to_evac = true; // always put it on the mutable list.
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        // follow everything
    {
        StgPtr next;

        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info);
        }
        break;
    }

    case TSO:
    {
        scavengeTSO((StgTSO *)p);
        p += sizeofW(StgTSO);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        gct->eager_promotion = false;

        scavenge_stack(stack->sp, stack->stack + stack->stack_size);
        stack->dirty = gct->failed_to_evac;
        p += stack_sizeW(stack);

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
        p += info->layout.payload.nptrs;

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
        p += sizeofW(StgTRecChunk);
        break;
      }

    case CONTINUATION:
        p = scavenge_continuation((StgContinuation *)p);
        break;

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
    if (gct->failed_to_evac) {
        gct->failed_to_evac = false;
        if (bd->gen_no > 0) {
            recordMutableGen_GC((StgClosure *)q, bd->gen_no);
        }
    }
  }

  if (p > bdescr_free(bd))  {
      gct->copied += ws->todo_free - bdescr_free(bd);
      bdescr_set_free(bd, p);
  }

  debugTrace(DEBUG_gc, "   scavenged %ld bytes",
             (unsigned long)((bdescr_free(bd) - bd->u.scan) * sizeof(W_)));

  // update stats: this is a block that has been scavenged
  gct->scanned += bdescr_free(bd) - bd->u.scan;
  bd->u.scan = bdescr_free(bd);

  if (bd != ws->todo_bd) {
      // we're not going to evac any more objects into
      // this block, so push it now.
      push_scanned_block(bd, ws);
  }

  gct->scan_bd = NULL;
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
                RELEASE_STORE(&mvar->header.info, &stg_MVAR_DIRTY_info);
            } else {
                RELEASE_STORE(&mvar->header.info, &stg_MVAR_CLEAN_info);
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
                RELEASE_STORE(&tvar->header.info, &stg_TVAR_DIRTY_info);
            } else {
                RELEASE_STORE(&tvar->header.info, &stg_TVAR_CLEAN_info);
            }
            break;
        }

        case FUN_2_0:
            scavenge_fun_srt(info);
            evacuate(&((StgClosure *)p)->payload[1]);
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case THUNK_2_0:
            scavenge_thunk_srt(info);
            evacuate(&((StgThunk *)p)->payload[1]);
            evacuate(&((StgThunk *)p)->payload[0]);
            break;

        case CONSTR_2_0:
            evacuate(&((StgClosure *)p)->payload[1]);
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case FUN_1_0:
        case FUN_1_1:
            scavenge_fun_srt(info);
            evacuate(&((StgClosure *)p)->payload[0]);
            break;

        case THUNK_1_0:
        case THUNK_1_1:
            scavenge_thunk_srt(info);
            evacuate(&((StgThunk *)p)->payload[0]);
            break;

        case CONSTR_1_0:
        case CONSTR_1_1:
            evacuate(&((StgClosure *)p)->payload[0]);
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
        {
            StgPtr end;

            scavenge_thunk_srt(info);
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
                RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_VAR_DIRTY_info);
            } else {
                RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_VAR_CLEAN_info);
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
                RELEASE_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_DIRTY_info);
            } else {
                RELEASE_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_CLEAN_info);
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
            scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
            break;
        }

        case PAP:
            scavenge_PAP((StgPAP *)p);
            break;

        case AP:
            scavenge_AP((StgAP *)p);
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

            scavenge_mut_arr_ptrs((StgMutArrPtrs *)p);

            if (gct->failed_to_evac) {
                RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_DIRTY_info);
            } else {
                RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_CLEAN_info);
            }

            gct->eager_promotion = saved_eager_promotion;
            gct->failed_to_evac = true; // mutable anyhow.
            break;
        }

        case MUT_ARR_PTRS_FROZEN_CLEAN:
        case MUT_ARR_PTRS_FROZEN_DIRTY:
            // follow everything
        {
            StgPtr q = p;

            scavenge_mut_arr_ptrs((StgMutArrPtrs *)p);

            if (gct->failed_to_evac) {
                RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info);
            } else {
                RELEASE_STORE(&((StgClosure *) q)->header.info, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
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
                RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_DIRTY_info);
            } else {
                RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_CLEAN_info);
            }

            gct->failed_to_evac = true; // mutable anyhow.
            break;
        }

        case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
        case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
            // follow everything
        {
            StgPtr next, q = p;

            next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
            for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
                evacuate((StgClosure **)p);
            }

            if (gct->failed_to_evac) {
                RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info);
            } else {
                RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info);
            }
            break;
        }

        case TSO:
        {
            scavengeTSO((StgTSO*)p);
            break;
        }

        case STACK:
        {
            StgStack *stack = (StgStack*)p;

            gct->eager_promotion = false;

            scavenge_stack(stack->sp, stack->stack + stack->stack_size);
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

        case CONTINUATION:
            scavenge_continuation((StgContinuation *)p);
            break;

        default:
            barf("scavenge_mark_stack: unimplemented/strange closure type %d @ %p",
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
   Scavenge one object.

   This is used for objects that are temporarily marked as mutable
   because they contain old-to-new generation pointers.  Only certain
   objects can have this property.
   -------------------------------------------------------------------------- */

static bool
scavenge_one(StgPtr p)
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
            RELEASE_STORE(&mvar->header.info, &stg_MVAR_DIRTY_info);
        } else {
            RELEASE_STORE(&mvar->header.info, &stg_MVAR_CLEAN_info);
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
            RELEASE_STORE(&tvar->header.info, &stg_TVAR_DIRTY_info);
        } else {
            RELEASE_STORE(&tvar->header.info, &stg_TVAR_CLEAN_info);
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

    case WEAK:
        // This WEAK object will not be considered by tidyWeakList during this
        // collection because it is in a generation > N, but it is on the
        // mutable list so we must evacuate all of its pointers because some
        // of them may point into a younger generation.
        scavengeLiveWeak((StgWeak *)p);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
        StgPtr q = p;

        gct->eager_promotion = false;
        evacuate(&((StgMutVar *)p)->var);
        gct->eager_promotion = saved_eager_promotion;

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_MUT_VAR_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_MUT_VAR_CLEAN_info);
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
            RELEASE_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_DIRTY_info);
        } else {
            RELEASE_STORE(&bq->header.info, &stg_BLOCKING_QUEUE_CLEAN_info);
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
        scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size);
        p = (StgPtr)ap->payload + ap->size;
        break;
    }

    case PAP:
        p = scavenge_PAP((StgPAP *)p);
        break;

    case AP:
        p = scavenge_AP((StgAP *)p);
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

        scavenge_mut_arr_ptrs((StgMutArrPtrs *)p);

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *)p)->header.info, &stg_MUT_ARR_PTRS_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *)p)->header.info, &stg_MUT_ARR_PTRS_CLEAN_info);
        }

        gct->eager_promotion = saved_eager_promotion;
        gct->failed_to_evac = true;
        break;
    }

    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
    {
        // follow everything
        scavenge_mut_arr_ptrs((StgMutArrPtrs *)p);

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *)p)->header.info, &stg_MUT_ARR_PTRS_FROZEN_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *)p)->header.info, &stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
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
            RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_CLEAN_info);
        }

        gct->failed_to_evac = true;
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
    {
        // follow everything
        StgPtr next, q=p;

        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate((StgClosure **)p);
        }

        if (gct->failed_to_evac) {
            RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info);
        } else {
            RELEASE_STORE(&((StgClosure *)q)->header.info, &stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info);
        }
        break;
    }

    case TSO:
    {
        scavengeTSO((StgTSO*)p);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        gct->eager_promotion = false;

        scavenge_stack(stack->sp, stack->stack + stack->stack_size);
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
        scavenge(&gen);
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

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        evacuate((StgClosure **)&bco->instrs);
        evacuate((StgClosure **)&bco->literals);
        evacuate((StgClosure **)&bco->ptrs);
        break;
    }

    case COMPACT_NFDATA:
        scavenge_compact((StgCompactNFData*)p);
        break;

    case CONTINUATION:
        scavenge_continuation((StgContinuation *)p);
        break;

    default:
        barf("scavenge_one: strange object %d", (int)(info->type));
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
scavenge_mutable_list(bdescr *bd, generation *gen)
{
    StgPtr p, q;
#if defined(DEBUG)
    MutListScavStats stats; // Local accumulator
    zeroMutListScavStats(&stats);
#endif

    uint32_t gen_no = gen->no;
    gct->evac_gen_no = gen_no;

    for (; bd != NULL; bd = bd->link) {
        for (q = bdescr_start(bd); q < bdescr_free(bd); q++) {
            p = (StgPtr)*q;
            ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

#if defined(DEBUG)
            const StgInfoTable *pinfo;
            switch (get_itbl((StgClosure *)p)->type) {
            case MUT_VAR_CLEAN:
                // can happen due to concurrent writeMutVars
            case MUT_VAR_DIRTY:
                stats.n_MUTVAR++; break;
            case MUT_ARR_PTRS_CLEAN:
            case MUT_ARR_PTRS_DIRTY:
            case MUT_ARR_PTRS_FROZEN_CLEAN:
            case MUT_ARR_PTRS_FROZEN_DIRTY:
                stats.n_MUTARR++; break;
            case MVAR_CLEAN:
                barf("MVAR_CLEAN on mutable list");
            case MVAR_DIRTY:
                stats.n_MVAR++; break;
            case TVAR:
                stats.n_TVAR++; break;
            case TREC_CHUNK:
                stats.n_TREC_CHUNK++; break;
            case MUT_PRIM:
                pinfo = ((StgClosure*)p)->header.info;
                if (pinfo == &stg_TVAR_WATCH_QUEUE_info)
                    stats.n_TVAR_WATCH_QUEUE++;
                else if (pinfo == &stg_TREC_HEADER_info)
                    stats.n_TREC_HEADER++;
                else
                    stats.n_OTHERS++;
                break;
            default:
                stats.n_OTHERS++; break;
            }
#endif

            // Check whether this object is "clean", that is it
            // definitely doesn't point into a young generation.
            // Clean objects don't need to be scavenged.  Some clean
            // objects (MUT_VAR_CLEAN) are not kept on the mutable
            // list at all; others, such as MUT_ARR_PTRS
            // are always on the mutable list.
            //
            switch (get_itbl((StgClosure *)p)->type) {
            case MUT_ARR_PTRS_CLEAN:
            case SMALL_MUT_ARR_PTRS_CLEAN:
                recordMutableGen_GC((StgClosure *)p,gen_no);
                continue;
            case MUT_ARR_PTRS_DIRTY:
            {
                bool saved_eager_promotion;
                saved_eager_promotion = gct->eager_promotion;
                gct->eager_promotion = false;

                scavenge_mut_arr_ptrs_marked((StgMutArrPtrs *)p);

                if (gct->failed_to_evac) {
                    RELEASE_STORE(&((StgClosure *)p)->header.info, &stg_MUT_ARR_PTRS_DIRTY_info);
                } else {
                    RELEASE_STORE(&((StgClosure *)p)->header.info, &stg_MUT_ARR_PTRS_CLEAN_info);
                }

                gct->eager_promotion = saved_eager_promotion;
                gct->failed_to_evac = false;
                recordMutableGen_GC((StgClosure *)p,gen_no);
                continue;
            }
            default:
                ;
            }

            if (RtsFlags.GcFlags.useNonmoving && major_gc && gen == oldest_gen) {
                // We can't use scavenge_one here as we need to scavenge SRTs
                nonmovingScavengeOne((StgClosure *)p);
            } else if (scavenge_one(p)) {
                // didn't manage to promote everything, so put the
                // object back on the list.
                recordMutableGen_GC((StgClosure *)p,gen_no);
            }
        }
    }

#if defined(DEBUG)
    // For lack of a better option we protect mutlist_scav_stats with oldest_gen->sync
    ACQUIRE_SPIN_LOCK(&oldest_gen->sync);
    addMutListScavStats(&stats, &mutlist_scav_stats);
    RELEASE_SPIN_LOCK(&oldest_gen->sync);
#endif
}

void
scavenge_capability_mut_lists (Capability *cap)
{
    // In a major GC only nonmoving heap's mut list is root
    if (RtsFlags.GcFlags.useNonmoving && major_gc) {
        uint32_t g = oldest_gen->no;
        scavenge_mutable_list(cap->saved_mut_lists[g], oldest_gen);
        freeChain_sync(cap->saved_mut_lists[g]);
        cap->saved_mut_lists[g] = NULL;
        return;
    }

    /* Mutable lists from each generation > N
     * we want to *scavenge* these roots, not evacuate them: they're not
     * going to move in this GC.
     * Also do them in reverse generation order, for the usual reason:
     * namely to reduce the likelihood of spurious old->new pointers.
     */
    for (uint32_t g = RtsFlags.GcFlags.generations-1; g > N; g--) {
        scavenge_mutable_list(cap->saved_mut_lists[g], &generations[g]);
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
scavenge_static(void)
{
  StgClosure *flagged_p, *p;
  const StgInfoTable *info;

  debugTrace(DEBUG_gc, "scavenging static objects");

  /* Always evacuate straight to the oldest generation for static
   * objects */
  gct->evac_gen_no = oldest_gen->no;

  /* keep going until we've scavenged all the objects on the linked
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
    StgClosure **link = STATIC_LINK(info,p);
    gct->static_objects = RELAXED_LOAD(link);
    RELAXED_STORE(link, gct->scavenged_static_objects);
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
      scavenge_thunk_srt(info);
      break;

    case FUN_STATIC:
      scavenge_fun_srt(info);
      FALLTHROUGH;

      // a FUN_STATIC can also be an SRT, so it may have pointer
      // fields.  See Note [SRTs] in CmmBuildInfoTables, specifically
      // the [FUN] optimisation.

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
      barf("scavenge_static: strange closure %d", (int)(info->type));
    }

    ASSERT(gct->failed_to_evac == false);
  }
}

/* -----------------------------------------------------------------------------
   scavenge a chunk of memory described by a bitmap
   -------------------------------------------------------------------------- */

static void
scavenge_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, StgWord size )
{
    walk_large_bitmap(do_evacuate, (StgClosure **) p, large_bitmap, size, NULL);
}


/* -----------------------------------------------------------------------------
   scavenge_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   AP_STACK_UPDs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */

void
scavenge_stack(StgPtr p, StgPtr stack_end)
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
        // ~~~~~~~~~~~~~~~~~~~~~
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
        // before GC, but that would be too expensive.
        //
        // Scavenging this update frame as normal would be disastrous;
        // the indirection will be shorted out, and the updatee would
        // end up pointing to the value.  The update code will then
        // overwrite the value, instead of the BLACKHOLE it is
        // expecting to write to.
        //
        // One way we could try to fix this is to detect when the
        // BLACKHOLE has been updated by another thread, and then
        // replace this update frame with a special frame that just
        // enters the value.  But this introduces some other
        // complexities:
        //
        // - we must be careful to call checkBlockingQueues() in this
        //   special frame, because we might otherwise miss wakeups
        //   for threads that blocked on the original BLACKHOLE,
        // - we must spot this frame when we're stripping the stack in
        //   raiseAsync() and raiseExceptionHelper(), and arrange to call
        //   checkBlockingQueues() there too.
        //
        // This is hard to get right, indeed we previously got it
        // wrong (see #13751).  So we now take a different approach:
        // always copy the BLACKHOLE, even if it is actually an
        // indirection.  This way we keep the update frame, we're
        // guaranteed to still perform the update, and check for
        // missed wakeups even when stripping the stack in
        // raiseAsync() and raiseExceptionHelper().  This is also a
        // little more efficient, because evacuating a known BLACKHOLE
        // is faster than evacuating an unknown closure.
        //
        // NOTE: for the reasons above, blackholing (either lazy or
        // eager) is NOT optional.  See also Note [avoiding
        // threadPaused] in Interpreter.c.
        //
        // There are a couple of alternative solutions:
        // - if we see an update frame that points to an indirection,
        //   arrange to call checkBlockingQueues() on that thread
        //   after GC.
        // - spot a BLOCKING_QUEUE that points to a value and
        //   arrange to wake it up after the GC.
        //
        // These are more difficult to implement, requiring an extra
        // list to be maintained during GC.  They also rely on more
        // subtle invariants than the solution implemented here.
        //

    {
        StgUpdateFrame *frame = (StgUpdateFrame *)p;

        evacuate_BLACKHOLE(&frame->updatee);
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
        p = scavenge_small_bitmap(p, size, bitmap);

    follow_srt:
        if (major_gc && info->i.srt) {
            StgClosure *srt = (StgClosure*)GET_SRT(info);
            evacuate(&srt);
        }
        continue;

    case RET_BCO: {
        StgBCO *bco;
        StgWord size;

        p++;
        evacuate((StgClosure **)p);
        bco = (StgBCO *)*p;
        p++;
        size = BCO_BITMAP_SIZE(bco);
        scavenge_large_bitmap(p, BCO_BITMAP(bco), size);
        p += size;
        continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
    case RET_BIG:
    {
        StgWord size;

        size = GET_LARGE_BITMAP(&info->i)->size;
        p++;
        scavenge_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size);
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
scavenge_large (gen_workspace *ws)
{
    bdescr *bd;
    StgPtr p;

    gct->evac_gen_no = ws->gen->no;

    bd = ws->todo_large_objects;

    for (; bd != NULL; bd = ws->todo_large_objects) {

        // take this object *off* the large objects list and put it on
        // the scavenged large objects list.  This is so that we can
        // treat todo_large_objects as a stack and push new objects on
        // the front when evacuating.
        ws->todo_large_objects = bd->link;

        ACQUIRE_SPIN_LOCK(&ws->gen->sync);
        if (bd->flags & BF_COMPACT) {
            dbl_link_onto(bd, &ws->gen->live_compact_objects);
            StgCompactNFData *str = ((StgCompactNFDataBlock*)bdescr_start(bd))->owner;
            ws->gen->n_live_compact_blocks += str->totalW / BLOCK_SIZE_W;
            p = (StgPtr)str;
        } else {
            dbl_link_onto(bd, &ws->gen->scavenged_large_objects);
            ws->gen->n_scavenged_large_blocks += bd->blocks;
            p = bdescr_start(bd);
        }
        RELEASE_SPIN_LOCK(&ws->gen->sync);

        if (scavenge_one(p)) {
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
scavenge_find_work (void)
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

        if (ws->todo_seg != END_NONMOVING_TODO_LIST) {
            struct NonmovingSegment *seg = ws->todo_seg;
            ASSERT(seg->todo_link);
            ws->todo_seg = seg->todo_link;
            seg->todo_link = NULL;
            scavengeNonmovingSegment(seg);
            did_something = true;
            break;
        }

        gct->scan_bd = NULL;

        // If we have a scan block with some work to do,
        // scavenge everything up to the free pointer.
        if (ws->todo_bd->u.scan < ws->todo_free)
        {
            scavenge_block(ws->todo_bd);
            did_something = true;
            break;
        }

        // If we have any large objects to scavenge, do them now.
        if (ws->todo_large_objects) {
            scavenge_large(ws);
            did_something = true;
            break;
        }

        if ((bd = grab_local_todo_block(ws)) != NULL) {
            scavenge_block(bd);
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
                scavenge_block(bd);
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
   Scavenge until we can't find anything more to scavenge.
   ------------------------------------------------------------------------- */

void
scavenge_loop(void)
{
    bool work_to_do;

loop:
    work_to_do = false;

    // scavenge static objects
    if (major_gc && gct->static_objects != END_OF_STATIC_OBJECT_LIST) {
        IF_DEBUG(sanity, checkStaticObjects(gct->static_objects));
        scavenge_static();
    }

    // scavenge objects in compacted generation
    if (mark_stack_bd != NULL && !mark_stack_empty()) {
        scavenge_mark_stack();
        work_to_do = true;
    }

    // Order is important here: we want to deal in full blocks as
    // much as possible, so go for global work in preference to
    // local work.  Only if all the global work has been exhausted
    // do we start scavenging the fragments of blocks in the local
    // workspaces.
    if (scavenge_find_work()) goto loop;

    if (work_to_do) goto loop;
}
