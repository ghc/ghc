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
    ObjectCode *oc;

    if (!lookupHashTable(addrs, (W_)addr)) {
        insertHashTable(addrs, (W_)addr, addr);

        oc = findOC(s_indices, addr);
        if (oc != NULL) {
            oc->referenced = 1;
            return;
        }
    }
}

static void searchStackChunk (HashTable *addrs, StgPtr sp, StgPtr stack_end,
        OCSectionIndices *s_indices)
{
    StgPtr p;
    const StgRetInfoTable *info;

    p = sp;
    while (p < stack_end) {
        info = get_ret_itbl((StgClosure *)p);

        switch (info->i.type) {
        case RET_SMALL:
        case RET_BIG:
            checkAddress(addrs, (const void*)info, s_indices);
            break;

        default:
            break;
        }

        p += stack_frame_sizeW((StgClosure*)p);
    }
}


static void searchHeapBlocks (HashTable *addrs, bdescr *bd,
        OCSectionIndices *s_indices)
{
    StgPtr p;
    const StgInfoTable *info;
    uint32_t size;
    bool prim;

    for (; bd != NULL; bd = bd->link) {

        if (bd->flags & BF_PINNED) {
            // Assume that objects in PINNED blocks cannot refer to
            continue;
        }

        p = bd->start;
        while (p < bd->free) {
            info = get_itbl((StgClosure *)p);
            prim = false;

            switch (info->type) {

            case THUNK:
                size = thunk_sizeW_fromITBL(info);
                break;

            case THUNK_1_1:
            case THUNK_0_2:
            case THUNK_2_0:
                size = sizeofW(StgThunkHeader) + 2;
                break;

            case THUNK_1_0:
            case THUNK_0_1:
            case THUNK_SELECTOR:
                size = sizeofW(StgThunkHeader) + 1;
                break;

            case FUN:
            case FUN_1_0:
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
                size = sizeW_fromITBL(info);
                break;

            case BLACKHOLE:
            case BLOCKING_QUEUE:
                prim = true;
                size = sizeW_fromITBL(info);
                break;

            case IND:
                // Special case/Delicate Hack: INDs don't normally
                // appear, since we're doing this heap census right
                // after GC.  However, GarbageCollect() also does
                // resurrectThreads(), which can update some
                // blackholes when it calls raiseAsync() on the
                // resurrected threads.  So we know that any IND will
                // be the size of a BLACKHOLE.
                prim = true;
                size = BLACKHOLE_sizeW();
                break;

            case BCO:
                prim = true;
                size = bco_sizeW((StgBCO *)p);
                break;

            case MVAR_CLEAN:
            case MVAR_DIRTY:
            case TVAR:
            case WEAK:
            case PRIM:
            case MUT_PRIM:
            case MUT_VAR_CLEAN:
            case MUT_VAR_DIRTY:
                prim = true;
                size = sizeW_fromITBL(info);
                break;

            case AP:
                prim = true;
                size = ap_sizeW((StgAP *)p);
                break;

            case PAP:
                prim = true;
                size = pap_sizeW((StgPAP *)p);
                break;

            case AP_STACK:
            {
                StgAP_STACK *ap = (StgAP_STACK *)p;
                prim = true;
                size = ap_stack_sizeW(ap);
                searchStackChunk(addrs, (StgPtr)ap->payload,
                                 (StgPtr)ap->payload + ap->size, s_indices);
                break;
            }

            case ARR_WORDS:
                prim = true;
                size = arr_words_sizeW((StgArrBytes*)p);
                break;

            case MUT_ARR_PTRS_CLEAN:
            case MUT_ARR_PTRS_DIRTY:
            case MUT_ARR_PTRS_FROZEN_CLEAN:
            case MUT_ARR_PTRS_FROZEN_DIRTY:
                prim = true;
                size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)p);
                break;

            case SMALL_MUT_ARR_PTRS_CLEAN:
            case SMALL_MUT_ARR_PTRS_DIRTY:
            case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
            case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
                prim = true;
                size = small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs *)p);
                break;

            case TSO:
                prim = true;
                size = sizeofW(StgTSO);
                break;

            case STACK: {
                StgStack *stack = (StgStack*)p;
                prim = true;
                searchStackChunk(addrs, stack->sp,
                                 stack->stack + stack->stack_size, s_indices);
                size = stack_sizeW(stack);
                break;
            }

            case TREC_CHUNK:
                prim = true;
                size = sizeofW(StgTRecChunk);
                break;

            default:
                barf("searchHeapBlocks, unknown object: %d", info->type);
            }

            if (!prim) {
                checkAddress(addrs,info, s_indices);
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
    IndexTable *i;

    checkAddress(addrs, ccs, s_indices);
    checkAddress(addrs, ccs->cc, s_indices);
    for (i = ccs->indexTable; i != NULL; i = i->next) {
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
  uint32_t g, n;
  HashTable *addrs;
  StgClosure* p;
  const StgInfoTable *info;
  ObjectCode *oc, *prev, *next;
  gen_workspace *ws;
  StgClosure* link;

  if (unloaded_objects == NULL) return;

  ACQUIRE_LOCK(&linker_unloaded_mutex);

  OCSectionIndices *s_indices = buildOCSectionIndices(unloaded_objects);
  // Mark every unloadable object as unreferenced initially
  for (oc = unloaded_objects; oc; oc = oc->next) {
      IF_DEBUG(linker, debugBelch("Checking whether to unload %" PATH_FMT "\n",
                                  oc->fileName));
      oc->referenced = false;
  }

  addrs = allocHashTable();

  for (p = static_objects; p != END_OF_STATIC_OBJECT_LIST; p = link) {
      p = UNTAG_STATIC_LIST_PTR(p);
      checkAddress(addrs, p, s_indices);
      info = get_itbl(p);
      checkAddress(addrs, info, s_indices);
      link = *STATIC_LINK(info, p);
  }

  // CAFs on revertible_caf_list are not on static_objects
  for (p = (StgClosure*)revertible_caf_list;
       p != END_OF_CAF_LIST;
       p = ((StgIndStatic *)p)->static_link) {
      p = UNTAG_STATIC_LIST_PTR(p);
      checkAddress(addrs, p, s_indices);
  }

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      searchHeapBlocks (addrs, generations[g].blocks, s_indices);
      searchHeapBlocks (addrs, generations[g].large_objects, s_indices);

      for (n = 0; n < n_capabilities; n++) {
          ws = &gc_threads[n]->gens[g];
          searchHeapBlocks(addrs, ws->todo_bd, s_indices);
          searchHeapBlocks(addrs, ws->part_list, s_indices);
          searchHeapBlocks(addrs, ws->scavd_list, s_indices);
      }
  }

#if defined(PROFILING)
  /* Traverse the cost centre tree, calling checkAddress on each CCS/CC */
  searchCostCentres(addrs, CCS_MAIN, s_indices);

  /* Also check each cost centre in the CC_LIST */
  CostCentre *cc;
  for (cc = CC_LIST; cc != NULL; cc = cc->link) {
      checkAddress(addrs, cc, s_indices);
  }
#endif /* PROFILING */

  freeOCSectionIndices(s_indices);
  // Look through the unloadable objects, and any object that is still
  // marked as unreferenced can be physically unloaded, because we
  // have no references to it.
  prev = NULL;
  for (oc = unloaded_objects; oc; oc = next) {
      next = oc->next;
      if (oc->referenced == 0) {
          if (prev == NULL) {
              unloaded_objects = oc->next;
          } else {
              prev->next = oc->next;
          }
          IF_DEBUG(linker, debugBelch("Unloading object file %" PATH_FMT "\n",
                                      oc->fileName));
          freeObjectCode(oc);
      } else {
          IF_DEBUG(linker, debugBelch("Object file still in use: %"
                                      PATH_FMT "\n", oc->fileName));
          prev = oc;
      }
  }

  freeHashTable(addrs, NULL);

  RELEASE_LOCK(&linker_unloaded_mutex);
}
