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

typedef struct {
    W_ start;
    W_ end;
    ObjectCode *oc;
} OCSectionIndex;

typedef struct {
    int n_sections;
    OCSectionIndex *indices;
} OCSectionIndices;

static OCSectionIndices *createOCSectionIndices(int n_sections)
{
    OCSectionIndices *s_indices;
    s_indices = stgMallocBytes(sizeof(OCSectionIndices), "OCSectionIndices");
    s_indices->n_sections = n_sections;
    s_indices->indices = stgMallocBytes(n_sections*sizeof(OCSectionIndex),
        "OCSectionIndices::indices");
    return s_indices;
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

static OCSectionIndices* buildOCSectionIndices(ObjectCode *ocs)
{
    int cnt_sections = 0;
    ObjectCode *oc;
    for (oc = ocs; oc; oc = oc->next) {
        cnt_sections += oc->n_sections;
    }
    OCSectionIndices* s_indices = createOCSectionIndices(cnt_sections);
    int s_i = 0, i;
    for (oc = ocs; oc; oc = oc->next) {
        for (i = 0; i < oc->n_sections; i++) {
            if (oc->sections[i].kind != SECTIONKIND_OTHER) {
                s_indices->indices[s_i].start = (W_)oc->sections[i].start;
                s_indices->indices[s_i].end = (W_)oc->sections[i].start
                    + oc->sections[i].size;
                s_indices->indices[s_i].oc = oc;
                s_i++;
            }
        }
    }
    s_indices->n_sections = s_i;
    qsort(s_indices->indices,
        s_indices->n_sections,
        sizeof(OCSectionIndex),
        cmpSectionIndex);
    return s_indices;
}

static void freeOCSectionIndices(OCSectionIndices *section_indices)
{
    free(section_indices->indices);
    free(section_indices);
}

static ObjectCode *findOC(OCSectionIndices *s_indices, const void *addr) {
    W_ w_addr = (W_)addr;
    if (s_indices->n_sections <= 0) return NULL;
    if (w_addr < s_indices->indices[0].start) return NULL;

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
        return s_indices->indices[left].oc;
    }
    return NULL;
}

static void checkAddress (HashTable *addrs, const void *addr,
        OCSectionIndices *s_indices)
{
    if (!lookupHashTable(addrs, (W_)addr)) {
        insertHashTable(addrs, (W_)addr, addr);

        ObjectCode *oc = findOC(s_indices, addr);
        if (oc != NULL) {
            oc->referenced = 1;
            return;
        }
    }
}

static StgPtr
search_small_bitmap (HashTable *addrs, StgPtr p, StgWord size, StgWord bitmap,
        OCSectionIndices *s_indices)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            checkAddress(addrs, (void*)*p, s_indices);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

typedef struct {
    HashTable *addrs;
    OCSectionIndices *s_indices;
} SearchArgs;

static void
search_payload_loc(StgClosure **p, void *args_)
{
    SearchArgs *args = (SearchArgs*)args_;
    checkAddress(args->addrs, *p, args->s_indices);
}

static void
search_large_bitmap (HashTable *addrs, StgPtr p, StgLargeBitmap *large_bitmap, StgWord size,
        OCSectionIndices *s_indices)
{
    SearchArgs args = { .addrs = addrs, .s_indices = s_indices };
    walk_large_bitmap(search_payload_loc, (StgClosure**)p, large_bitmap, size, &args);
}

static void
search_PAP_payload (HashTable *addrs, StgClosure *fun, StgClosure **payload, StgWord size,
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
        search_large_bitmap(addrs, p, GET_FUN_LARGE_BITMAP(fun_info), size, s_indices);
        break;
    case ARG_BCO:
        search_large_bitmap(addrs, (StgPtr)payload, BCO_BITMAP(fun), size, s_indices);
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        search_small_bitmap(addrs, p, size, bitmap, s_indices);
        break;
    }
}

static StgPtr
search_arg_block (HashTable *addrs, const StgFunInfoTable *fun_info, StgClosure **args,
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
        search_large_bitmap(addrs, p, GET_FUN_LARGE_BITMAP(fun_info), size, s_indices);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = search_small_bitmap(addrs, p, size, bitmap, s_indices);
        break;
    }
    return p;
}

static void searchStackChunk (HashTable *addrs, StgPtr p, StgPtr stack_end,
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
            checkAddress(addrs, frame->updatee, s_indices);
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
            p = search_small_bitmap(addrs, p, size, bitmap, s_indices);
            continue;
        }

        case RET_BCO:
        {
            p++;
            checkAddress(addrs, (void*)*p, s_indices);
            StgBCO *bco = (StgBCO *)*p;
            p++;
            StgWord size = BCO_BITMAP_SIZE(bco);
            search_large_bitmap(addrs, p, BCO_BITMAP(bco), size, s_indices);
            p += size;
            continue;
        }

        // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
        case RET_BIG:
        {
            StgWord size = GET_LARGE_BITMAP(&info->i)->size;
            p++;
            search_large_bitmap(addrs, p, GET_LARGE_BITMAP(&info->i), size, s_indices);
            p += size;
            continue;
        }

        case RET_FUN:
        {
            StgRetFun *ret_fun = (StgRetFun *)p;
            checkAddress(addrs, ret_fun->fun, s_indices);
            const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
            p = search_arg_block(addrs, fun_info, ret_fun->payload, s_indices);
            continue;
        }

        default:
            barf("searchStackChunk: weird activation record found on stack: %d", (int)(info->i.type));
        }
    }
}

static void searchHeapBlocks (HashTable *addrs, bdescr *bd,
        OCSectionIndices *s_indices)
{
    for (; bd != NULL; bd = bd->link) {

        if (bd->flags & BF_PINNED) {
            // Assume that objects in PINNED blocks cannot refer to dynamically
            // loaded objects
            continue;
        }

        StgPtr p = bd->start;
        while (p < bd->free) {
            StgClosure *c = (StgClosure*)p;
            const StgInfoTable *info = get_itbl(c);
            uint32_t size;

            checkAddress(addrs, info, s_indices);

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
                    checkAddress(addrs, c->payload[i], s_indices);
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
            case THUNK_SELECTOR:
            case THUNK_STATIC: // TODO: skip this?
            {
                StgThunk *thunk = (StgThunk*)c;
                for (W_ i = 0; i < info->layout.payload.ptrs; ++i) {
                    checkAddress(addrs, thunk->payload[i], s_indices);
                }
                size = closure_sizeW(c);
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
                    checkAddress(addrs, a->payload[i], s_indices);
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
                    checkAddress(addrs, a->payload[i], s_indices);
                }
                size = small_mut_arr_ptrs_sizeW(a);
                break;
            }

            case TSO:
            {
                StgTSO *tso = (StgTSO*)c;

                if (tso->bound != NULL) {
                    checkAddress(addrs, tso->bound->tso, s_indices);
                }

                checkAddress(addrs, tso->blocked_exceptions, s_indices);
                checkAddress(addrs, tso->bq, s_indices);
                checkAddress(addrs, tso->trec, s_indices);
                checkAddress(addrs, tso->stackobj, s_indices);
                checkAddress(addrs, tso->_link, s_indices);
                checkAddress(addrs, tso->block_info.closure, s_indices);

                size = sizeofW(StgTSO);
                break;
            }

            case STACK:
            {
                StgStack *stack = (StgStack*)c;
                searchStackChunk(addrs, stack->sp, stack->stack + stack->stack_size, s_indices);
                size = stack_sizeW(stack);
                break;
            }

            case WEAK:
            {
                StgWeak *w = (StgWeak*)c;
                checkAddress(addrs, w->value, s_indices);
                checkAddress(addrs, w->key, s_indices);
                checkAddress(addrs, w->finalizer, s_indices);
                checkAddress(addrs, w->cfinalizers, s_indices);
                size = closure_sizeW(c);
                break;
            }

            case AP_STACK:
            {
                StgAP_STACK *ap = (StgAP_STACK*)c;
                checkAddress(addrs, ap->fun, s_indices);
                searchStackChunk(addrs, (StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, s_indices);
                size = ap_stack_sizeW(ap);
                break;
            }

            case PAP:
            {
                StgPAP *pap = (StgPAP*)c;
                checkAddress(addrs, pap->fun, s_indices);
                search_PAP_payload(addrs, pap->fun, pap->payload, pap->n_args, s_indices);
                size = closure_sizeW(c);
                break;
            }

            case AP:
            {
                StgAP *ap = (StgAP*)c;
                checkAddress(addrs, ap->fun, s_indices);
                search_PAP_payload(addrs, ap->fun, ap->payload, ap->n_args, s_indices);
                size = closure_sizeW(c);
                break;
            }

            case IND:
            case IND_STATIC: // TODO: skip this?
            case BLACKHOLE:
            {
                checkAddress(addrs, ((StgInd*)c)->indirectee, s_indices);
                size = closure_sizeW(c);
                break;
            }

            case COMPACT_NFDATA:
            {
                barf("searchHeapBlocks: COMPACT_NFDATA");
                break;
            }

            case TREC_CHUNK:
            {
                barf("searchHeapBlocks: TREC_CHUNK");
                break;
            }

            default:
                barf("searchHeapBlocks: %d", info->type);
            }

            p += size;
        }
    }
}

#if defined(PROFILING)
//
// Do not unload the object if the CCS tree refers to a CCS or CC which
// originates in the object.
//
static void searchCostCentres (HashTable *addrs, CostCentreStack *ccs,
        OCSectionIndices* s_indices)
{
    checkAddress(addrs, ccs, s_indices);
    checkAddress(addrs, ccs->cc, s_indices);
    for (IndexTable *i = ccs->indexTable; i != NULL; i = i->next) {
        if (!i->back_edge) {
            searchCostCentres(addrs, i->ccs, s_indices);
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
  if (unloaded_objects == NULL) return;

  ACQUIRE_LOCK(&linker_unloaded_mutex);

  OCSectionIndices *s_indices = buildOCSectionIndices(unloaded_objects);
  // Mark every unloadable object as unreferenced initially
  for (ObjectCode *oc = unloaded_objects; oc; oc = oc->next) {
      debugBelch("Checking whether to unload %" PATH_FMT "\n", oc->fileName);
      oc->referenced = false;
  }

  HashTable *addrs = allocHashTable();

  StgClosure *link = NULL;
  for (StgClosure *p = static_objects; p != END_OF_STATIC_OBJECT_LIST; p = link) {
      p = UNTAG_STATIC_LIST_PTR(p);
      checkAddress(addrs, p, s_indices);
      const StgInfoTable *info = get_itbl(p);
      checkAddress(addrs, info, s_indices);
      link = *STATIC_LINK(info, p);
  }

  // CAFs on revertible_caf_list are not on static_objects
  for (StgClosure *p = (StgClosure*)revertible_caf_list;
       p != END_OF_CAF_LIST;
       p = ((StgIndStatic *)p)->static_link) {
      p = UNTAG_STATIC_LIST_PTR(p);
      checkAddress(addrs, p, s_indices);
  }

  for (uint32_t g = 0; g < RtsFlags.GcFlags.generations; g++) {
      searchHeapBlocks(addrs, generations[g].blocks, s_indices);
      searchHeapBlocks(addrs, generations[g].large_objects, s_indices);

      for (uint32_t n = 0; n < n_capabilities; n++) {
          gen_workspace *ws = &gc_threads[n]->gens[g];
          searchHeapBlocks(addrs, ws->todo_bd, s_indices);
          searchHeapBlocks(addrs, ws->part_list, s_indices);
          searchHeapBlocks(addrs, ws->scavd_list, s_indices);
      }
  }

#if defined(PROFILING)
  /* Traverse the cost centre tree, calling checkAddress on each CCS/CC */
  searchCostCentres(addrs, CCS_MAIN, s_indices);

  /* Also check each cost centre in the CC_LIST */
  for (CostCentre *cc = CC_LIST; cc != NULL; cc = cc->link) {
      checkAddress(addrs, cc, s_indices);
  }
#endif /* PROFILING */

  freeOCSectionIndices(s_indices);
  // Look through the unloadable objects, and any object that is still
  // marked as unreferenced can be physically unloaded, because we
  // have no references to it.
  ObjectCode *prev = NULL;
  ObjectCode *next = NULL;
  for (ObjectCode *oc = unloaded_objects; oc; oc = next) {
      next = oc->next;
      if (oc->referenced == 0) {
          if (prev == NULL) {
              unloaded_objects = oc->next;
          } else {
              prev->next = oc->next;
          }
          debugBelch("Unloading object file %" PATH_FMT "\n", oc->fileName);
          freeObjectCode(oc);
      } else {
          debugBelch("Object file still in use: %" PATH_FMT "\n", oc->fileName);
          prev = oc;
      }
  }

  freeHashTable(addrs, NULL);

  RELEASE_LOCK(&linker_unloaded_mutex);
}
