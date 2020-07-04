/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2013-
 *
 * Check whether dynamically-loaded object code can be safely
 * unloaded, by searching for references to it from the heap and RTS
 * data structures.
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Hash.h"
#include "LinkerInternals.h"
#include "CheckUnload.h"
#include "sm/Storage.h"
#include "sm/GCThread.h"
#include "sm/HeapUtils.h"
#include "sm/CNF.h"

//
// Code that we unload may be referenced from:
//   - info pointers in heap objects and stack frames
//   - pointers to static objects from the heap
//   - StablePtrs to static objects
//   - pointers to cost centres from the cost centre tree
//
// We can find live static objects after a major GC, so we don't have
// to look at every closure pointer in the heap.  However, we do have
// to look at every info pointer.  So this is like a heap census
// traversal: we look at the header of every object, but not its
// contents.
//
// On the assumption that there aren't many different info pointers in
// a typical heap, we insert addresses into a hash table.  The
// first time we see an address, we check it against the pending
// unloadable objects and if it lies within any of them, we mark that
// object as referenced so that it won't get unloaded in this round.
//

// Note [Speeding up checkUnload]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// In certain circumstances, there may be a lot of unloaded ObjectCode structs
// chained in `unloaded_objects` (such as when users `:load` a module in a very
// big repo in GHCi). To speed up checking whether an address lies within any of
// these objects, we populate the addresses of their mapped sections in
// an array sorted by their `start` address and do binary search for our address
// on that array. Note that this works because the sections are mapped to mutual
// exclusive memory regions, so we can simply find the largest lower bound among
// the `start` addresses of the sections and then check if our address is inside
// that section. In particular, we store the start address and end address of
// each mapped section in a OCSectionIndex, arrange them all on a contiguous
// memory range and then sort by start address. We then put this array in an
// OCSectionIndices struct to be passed into `checkAddress` to do binary search
// on.
//

uint8_t object_code_mark_bit = 0;

typedef struct {
    W_ start;
    W_ end;
    ObjectCode *oc;
} OCSectionIndex;

typedef struct {
    int capacity; // Doubled on resize
    int n_sections;
    bool sorted; // Invalidated on insertion. Sorted in checkUnload.
    bool unloaded; // Whether we removed anything from the table in
                   // removeOCSectionIndices. If this is set we "compact" the
                   // table (remove unused entries) in `sortOCSectionIndices.
    OCSectionIndex *indices;
} OCSectionIndices;

ObjectCode *objects = NULL; // Not static, used in Linker.c
static ObjectCode *old_objects = NULL;
ObjectCode *loaded_objects; // Not static, used in Linker.c
static OCSectionIndices *global_s_indices = NULL; // TODO: Maybe use this directly below instead of passing as parameter?

static OCSectionIndices *createOCSectionIndices(void)
{
    // TODO (osa): Maybe initialize as empty (without allocation) and allocate
    // on first insertion?
    OCSectionIndices *s_indices = stgMallocBytes(sizeof(OCSectionIndices), "OCSectionIndices");
    int capacity = 1024;
    s_indices->capacity = capacity;
    s_indices->n_sections = 0;
    s_indices->sorted = true;
    s_indices->unloaded = false;
    s_indices->indices = stgMallocBytes(capacity * sizeof(OCSectionIndex),
        "OCSectionIndices::indices");
    return s_indices;
}

static void freeOCSectionIndices(OCSectionIndices *s_indices)
{
    free(s_indices->indices);
    free(s_indices);
}

void initUnloadCheck()
{
    global_s_indices = createOCSectionIndices();
}

void exitUnloadCheck()
{
    freeOCSectionIndices(global_s_indices);
    global_s_indices = NULL;
}

static int cmpSectionIndex(const void* indexa, const void *indexb)
{
    W_ s1 = ((OCSectionIndex*)indexa)->start;
    W_ s2 = ((OCSectionIndex*)indexb)->start;
    if (s1 < s2) {
        return -1;
    } else if (s1 > s2) {
        return 1;
    }
    return 0;
}

static void reserveOCSectionIndices(OCSectionIndices *s_indices, int len)
{
    int current_capacity = s_indices->capacity;
    int current_len = s_indices->n_sections;
    if (current_capacity - current_len >= len) {
        return;
    }

    // Round up to nearest power of 2
    int new_capacity = (int)pow(2, ceil(log2(current_len + len)));

    OCSectionIndex *old_indices = s_indices->indices;
    OCSectionIndex *new_indices = stgMallocBytes(new_capacity * sizeof(OCSectionIndex),
        "reserveOCSectionIndices");

    for (int i = 0; i < current_len; ++i) {
        new_indices[i] = old_indices[i];
    }

    s_indices->capacity = new_capacity;
    s_indices->indices = new_indices;

    free(old_indices);
}

// Insert object section indices of a single ObjectCode. Invalidates 'sorted'
// state.
void insertOCSectionIndices(ObjectCode *oc)
{
    reserveOCSectionIndices(global_s_indices, oc->n_sections);
    global_s_indices->sorted = false;

    int s_i = global_s_indices->n_sections;
    for (int i = 0; i < oc->n_sections; i++) {
        if (oc->sections[i].kind != SECTIONKIND_OTHER) {
            global_s_indices->indices[s_i].start = (W_)oc->sections[i].start;
            global_s_indices->indices[s_i].end = (W_)oc->sections[i].start
                + oc->sections[i].size;
            global_s_indices->indices[s_i].oc = oc;
            s_i++;
        }
    }

    global_s_indices->n_sections = s_i;

    // Add object to 'objects' list
    if (objects != NULL) {
        objects->prev = oc;
    }
    oc->next = objects;
    objects = oc;
}

static int findSectionIdx(OCSectionIndices *s_indices, const void *addr);

static void removeOCSectionIndices(OCSectionIndices *s_indices, ObjectCode *oc)
{
    // To avoid quadratic behavior in checkUnload we set `oc` fields of indices
    // of unloaded objects NULL here. Removing unused entries is done in
    // `sortOCSectionIndices`.

    s_indices->unloaded = true;

    for (int i = 0; i < oc->n_sections; i++) {
        if (oc->sections[i].kind != SECTIONKIND_OTHER) {
            int section_idx = findSectionIdx(s_indices, oc->sections[i].start);
            if (section_idx != -1) {
                s_indices->indices[section_idx].oc = NULL;
            }
        }
    }
}

static void sortOCSectionIndices(OCSectionIndices *s_indices) {
    if (s_indices->sorted) {
        return;
    }

    qsort(s_indices->indices,
        s_indices->n_sections,
        sizeof(OCSectionIndex),
        cmpSectionIndex);

    s_indices->sorted = true;
}

static void removeRemovedOCSections(OCSectionIndices *s_indices) {
    if (!s_indices->unloaded) {
        return;
    }

    int next_free_idx = 0;
    for (int i = 0; i < s_indices->n_sections; ++i) {
        if (s_indices->indices[i].oc == NULL) {
            // free entry, skip
        } else if (i == next_free_idx) {
            ++next_free_idx;
        } else {
            s_indices->indices[next_free_idx] = s_indices->indices[i];
            ++next_free_idx;
        }
    }

    s_indices->n_sections = next_free_idx;
    s_indices->unloaded = true;
}

// Returns -1 if not found
static int findSectionIdx(OCSectionIndices *s_indices, const void *addr) {
    ASSERT(s_indices->sorted);

    W_ w_addr = (W_)addr;
    if (s_indices->n_sections <= 0) {
        return -1;
    }
    if (w_addr < s_indices->indices[0].start) {
        return -1;
    }

    int left = 0, right = s_indices->n_sections;
    while (left + 1 < right) {
        int mid = (left + right)/2;
        W_ w_mid = s_indices->indices[mid].start;
        if (w_mid <= w_addr) {
            left = mid;
        } else {
            right = mid;
        }
    }
    ASSERT(w_addr >= s_indices->indices[left].start);
    if (w_addr < s_indices->indices[left].end) {
        return left;
    }
    return -1;
}

static ObjectCode *findOC(OCSectionIndices *s_indices, const void *addr) {
    int oc_idx = findSectionIdx(s_indices, addr);

    if (oc_idx == -1) {
        return NULL;
    }

    return s_indices->indices[oc_idx].oc;
}

static bool markObjectLive(void *data STG_UNUSED, StgWord key, const void *value STG_UNUSED) {
    ObjectCode *oc = (ObjectCode*)key;
    if (oc->mark == object_code_mark_bit) {
        return true; // for hash table iteration
    }

    oc->mark = object_code_mark_bit;
    // Remove from 'old_objects' list
    if (oc->prev != NULL) {
        // TODO(osa): Maybe 'prev' should be a pointer to the referencing
        // *field* ? (instead of referencing *object*)
        oc->prev->next = oc->next;
    } else {
        old_objects = oc->next;
    }
    if (oc->next != NULL) {
        oc->next->prev = oc->prev;
    }

    // Add it to 'objects' list
    oc->prev = NULL;
    oc->next = objects;
    if (objects != NULL) {
        objects->prev = oc;
    }
    objects = oc;

    // Mark its dependencies
    iterHashTable(oc->dependencies, NULL, markObjectLive);

    return true; // for hash table iteration
}

static void checkAddress (const void *addr, OCSectionIndices *s_indices)
{
    if (HEAP_ALLOCED(addr)) {
        return;
    }

    ObjectCode *oc = findOC(s_indices, addr);
    if (oc != NULL) {
        // Mark the object code and its dependencies
        markObjectLive(NULL, (W_)oc, NULL);
    }
}

static StgPtr
search_small_bitmap (StgPtr p, StgWord size, StgWord bitmap, OCSectionIndices *s_indices)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            checkAddress((void*)*p, s_indices);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

static void
search_payload_loc(StgClosure **p, void *s_indices_)
{
    OCSectionIndices *s_indices = (OCSectionIndices*)s_indices_;
    checkAddress(*p, s_indices);
}

static void
search_large_bitmap (StgPtr p, StgLargeBitmap *large_bitmap, StgWord size,
        OCSectionIndices *s_indices)
{
    walk_large_bitmap(search_payload_loc, (StgClosure**)p, large_bitmap, size, (void*)s_indices);
}

static void
search_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size,
        OCSectionIndices *s_indices)
{
    const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(fun));
    StgPtr p = (StgPtr)payload;

    StgWord bitmap;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        search_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, s_indices);
        break;
    case ARG_BCO:
        search_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size, s_indices);
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        search_small_bitmap(p, size, bitmap, s_indices);
        break;
    }
}

static StgPtr
search_arg_block (const StgFunInfoTable *fun_info, StgClosure **args,
        OCSectionIndices *s_indices)
{
    StgWord bitmap;
    StgWord size;

    StgPtr p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        search_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, s_indices);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = search_small_bitmap(p, size, bitmap, s_indices);
        break;
    }
    return p;
}

static void searchStackChunk (StgPtr p, StgPtr stack_end,
        OCSectionIndices *s_indices)
{
    StgWord bitmap;
    StgWord size;

    while (p < stack_end) {
        const StgRetInfoTable *info = get_ret_itbl((StgClosure *)p);

        switch (info->i.type) {

        case UPDATE_FRAME:
        {
            StgUpdateFrame *frame = (StgUpdateFrame *)p;
            checkAddress(frame->updatee, s_indices);
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
        {
            bitmap = BITMAP_BITS(info->i.layout.bitmap);
            size   = BITMAP_SIZE(info->i.layout.bitmap);
            p++;
            p = search_small_bitmap(p, size, bitmap, s_indices);
            continue;
        }

        case RET_BCO:
        {
            p++;
            checkAddress((void*)*p, s_indices);
            StgBCO *bco = (StgBCO *)*p;
            p++;
            StgWord size = BCO_BITMAP_SIZE(bco);
            search_large_bitmap(p, BCO_BITMAP(bco), size, s_indices);
            p += size;
            continue;
        }

        // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
        case RET_BIG:
        {
            StgWord size = GET_LARGE_BITMAP(&info->i)->size;
            p++;
            search_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size, s_indices);
            p += size;
            continue;
        }

        case RET_FUN:
        {
            StgRetFun *ret_fun = (StgRetFun *)p;
            checkAddress(ret_fun->fun, s_indices);
            const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
            p = search_arg_block(fun_info, ret_fun->payload, s_indices);
            continue;
        }

        default:
            barf("searchStackChunk: weird activation record found on stack: %d", (int)(info->i.type));
        }
    }
}

static void searchHeapBlock (StgPtr p, StgPtr end, OCSectionIndices *s_indices)
{
    while (p < end) {
        StgClosure *c = (StgClosure*)p;
        const StgInfoTable *info = get_itbl(c);
        uint32_t size;

        // NOTE: This call could be omitted for info tables that we know
        // can't be created by GHCi, e.g. MUT_VARs, ARR_WORDs etc. I'm not
        // sure if it's worth optimizing this.
        checkAddress(info, s_indices);

        // NOTE: No need to check SRTs below because we can't refer to a
        // dynamically loaded object from SRTs.

        switch (info->type) {

        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_0_1:
        case CONSTR_2_0:
        case CONSTR_1_1:
        case CONSTR_0_2:
        case CONSTR_NOCAF:
        case FUN:
        case FUN_1_0:
        case FUN_0_1:
        case FUN_1_1:
        case FUN_0_2:
        case FUN_2_0:
        case FUN_STATIC: // TODO: skip this?
        case PRIM:
        case MUT_PRIM:
        case MVAR_CLEAN:
        case MVAR_DIRTY:
        case MUT_VAR_CLEAN:
        case MUT_VAR_DIRTY:
        case TVAR:
        case BCO:
        case BLOCKING_QUEUE:
        {
            for (W_ i = 0; i < info->layout.payload.ptrs; ++i) {
                checkAddress(c->payload[i], s_indices);
            }
            size = closure_sizeW(c);
            break;
        }

        case THUNK:
        case THUNK_1_0:
        case THUNK_0_1:
        case THUNK_2_0:
        case THUNK_1_1:
        case THUNK_0_2:
        case THUNK_STATIC: // TODO: skip this?
        {
            StgThunk *thunk = (StgThunk*)c;
            for (W_ i = 0; i < info->layout.payload.ptrs; ++i) {
                checkAddress(thunk->payload[i], s_indices);
            }
            size = closure_sizeW(c);
            break;
        }

        case THUNK_SELECTOR:
        {
            StgSelector *s = (StgSelector *)c;
            checkAddress(s->selectee, s_indices);
            size = THUNK_SELECTOR_sizeW();
            break;
        }

        case ARR_WORDS:
        {
            size = arr_words_sizeW((StgArrBytes*)c);
            break;
        }

        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
        case MUT_ARR_PTRS_FROZEN_CLEAN:
        case MUT_ARR_PTRS_FROZEN_DIRTY:
        {
            StgMutArrPtrs *a = (StgMutArrPtrs*)c;
            for (W_ i = 0; i < a->ptrs; ++i) {
                checkAddress(a->payload[i], s_indices);
            }
            size = mut_arr_ptrs_sizeW(a);
            break;
        }

        case SMALL_MUT_ARR_PTRS_CLEAN:
        case SMALL_MUT_ARR_PTRS_DIRTY:
        case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
        case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        {
            StgSmallMutArrPtrs *a = (StgSmallMutArrPtrs*)c;
            for (W_ i = 0; i < a->ptrs; ++i) {
                checkAddress(a->payload[i], s_indices);
            }
            size = small_mut_arr_ptrs_sizeW(a);
            break;
        }

        case TSO:
        {
            StgTSO *tso = (StgTSO*)c;

            if (tso->bound != NULL) {
                checkAddress(tso->bound->tso, s_indices);
            }

            checkAddress(tso->blocked_exceptions, s_indices);
            checkAddress(tso->bq, s_indices);
            checkAddress(tso->trec, s_indices);
            checkAddress(tso->stackobj, s_indices);
            checkAddress(tso->_link, s_indices);
            checkAddress(tso->block_info.closure, s_indices);

            size = sizeofW(StgTSO);
            break;
        }

        case STACK:
        {
            StgStack *stack = (StgStack*)c;
            searchStackChunk(stack->sp, stack->stack + stack->stack_size, s_indices);
            size = stack_sizeW(stack);
            break;
        }

        case WEAK:
        {
            StgWeak *w = (StgWeak*)c;
            checkAddress(w->value, s_indices);
            checkAddress(w->key, s_indices);
            checkAddress(w->finalizer, s_indices);
            checkAddress(w->cfinalizers, s_indices);
            size = closure_sizeW(c);
            break;
        }

        case AP_STACK:
        {
            StgAP_STACK *ap = (StgAP_STACK*)c;
            checkAddress(ap->fun, s_indices);
            searchStackChunk((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, s_indices);
            size = ap_stack_sizeW(ap);
            break;
        }

        case PAP:
        {
            StgPAP *pap = (StgPAP*)c;
            checkAddress(pap->fun, s_indices);
            search_PAP_payload(pap->fun, pap->payload, pap->n_args, s_indices);
            size = closure_sizeW(c);
            break;
        }

        case AP:
        {
            StgAP *ap = (StgAP*)c;
            checkAddress(ap->fun, s_indices);
            search_PAP_payload(ap->fun, ap->payload, ap->n_args, s_indices);
            size = closure_sizeW(c);
            break;
        }

        case IND:
        case IND_STATIC: // TODO: skip this?
        case BLACKHOLE:
        {
            checkAddress(((StgInd*)c)->indirectee, s_indices);
            size = closure_sizeW(c);
            break;
        }

        case TREC_CHUNK:
        {
            StgTRecChunk *tc = ((StgTRecChunk *) p);
            TRecEntry *e = &(tc -> entries[0]);
            checkAddress(tc->prev_chunk, s_indices);
            for (W_ i = 0; i < tc -> next_entry_idx; i ++, e++) {
                checkAddress(e->tvar, s_indices);
                checkAddress(e->expected_value, s_indices);
                checkAddress(e->new_value, s_indices);
            }
            size = sizeofW(StgTRecChunk);
            break;
        }

        case COMPACT_NFDATA:
        {
            StgCompactNFData *str = (StgCompactNFData*)c;
            for (StgCompactNFDataBlock *block = compactGetFirstBlock(str); block; block = block->next) {
                bdescr *bd = Bdescr((P_)block);
                StgPtr start = bd->start + sizeofW(StgCompactNFDataBlock);
                searchHeapBlock(start, bd->free, s_indices);
            }
            size = sizeofW(StgCompactNFData);
            break;
        }

        default:
            barf("searchHeapBlocks: %d", info->type);
        }

        p += size;
    }
}

static void searchHeapBlocks (bdescr *bd, OCSectionIndices *s_indices)
{
    for (; bd != NULL; bd = bd->link) {

        if (bd->flags & BF_PINNED) {
            // Assume that objects in PINNED blocks cannot refer to dynamically
            // loaded objects
            continue;
        }

        searchHeapBlock(bd->start, bd->free, s_indices);
    }
}

#if defined(PROFILING)
//
// Do not unload the object if the CCS tree refers to a CCS or CC which
// originates in the object.
//
static void searchCostCentres (CostCentreStack *ccs, OCSectionIndices* s_indices)
{
    checkAddress(ccs, s_indices);
    checkAddress(ccs->cc, s_indices);
    for (IndexTable *i = ccs->indexTable; i != NULL; i = i->next) {
        if (!i->back_edge) {
            searchCostCentres(i->ccs, s_indices);
        }
    }
}
#endif

//
// Check whether we can unload any object code.  This is called at the
// appropriate point during a GC, where all the heap data is nice and
// packed together and we have a linked list of the static objects.
//
// The check involves a complete heap traversal, but you only pay for
// this (a) when you have called unloadObj(), and (b) at a major GC,
// which is much more expensive than the traversal we're doing here.
//
void checkUnload (StgClosure *static_objects)
{
    if (global_s_indices == NULL) {
        return;
    }

    OCSectionIndices *s_indices = global_s_indices;
    removeRemovedOCSections(s_indices);
    sortOCSectionIndices(s_indices);

    object_code_mark_bit = ~object_code_mark_bit;
    old_objects = objects;
    objects = NULL;


    // Mark roots
    for (ObjectCode *oc = loaded_objects; oc != NULL; oc = oc->next_loaded_object) {
        markObjectLive(NULL, (W_)oc, NULL);
    }
    // TODO (osa): Do we need to take linker_mutex here? I think not -- unloadObj
    // no longer uses linker state (it was using unloaded_objects before)

    StgClosure *link = NULL;
    for (StgClosure *p = static_objects; p != END_OF_STATIC_OBJECT_LIST; p = link) {
        p = UNTAG_STATIC_LIST_PTR(p);
        checkAddress(p, s_indices);
        const StgInfoTable *info = get_itbl(p);
        checkAddress(info, s_indices);
        link = *STATIC_LINK(info, p);
    }

    // CAFs on revertible_caf_list are not on static_objects
    for (StgClosure *p = (StgClosure*)revertible_caf_list;
            p != END_OF_CAF_LIST;
            p = ((StgIndStatic *)p)->static_link) {
        p = UNTAG_STATIC_LIST_PTR(p);
        checkAddress(p, s_indices);
    }

    for (uint32_t g = 0; g < RtsFlags.GcFlags.generations; g++) {
        searchHeapBlocks(generations[g].blocks, s_indices);
        searchHeapBlocks(generations[g].large_objects, s_indices);

        for (uint32_t n = 0; n < n_capabilities; n++) {
            gen_workspace *ws = &gc_threads[n]->gens[g];
            searchHeapBlocks(ws->todo_bd, s_indices);
            searchHeapBlocks(ws->part_list, s_indices);
            searchHeapBlocks(ws->scavd_list, s_indices);
        }
    }

#if defined(PROFILING)
    /* Traverse the cost centre tree, calling checkAddress on each CCS/CC */
    searchCostCentres(CCS_MAIN, s_indices);

    /* Also check each cost centre in the CC_LIST */
    for (CostCentre *cc = CC_LIST; cc != NULL; cc = cc->link) {
        checkAddress(cc, s_indices);
    }
#endif /* PROFILING */

    // Free unmarked objects
    ObjectCode *next = NULL;
    for (ObjectCode *oc = old_objects; oc != NULL; oc = next) {
        next = oc->next;

        removeOCSectionIndices(s_indices, oc);
        freeObjectCode(oc);
    }
}
