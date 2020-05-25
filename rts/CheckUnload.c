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

// List of currently live objects. Moved to `old_objects` before unload check.
// Marked objects moved back to this list. Remaining objects are freed.
//
// Double-linked to be able to remove marked objects.
//
// Not static: used in Linker.c.
ObjectCode *objects = NULL;

// `objects` list is moved here before unload check. Marked objects are moved
// back to `objects`. Remaining objects are freed.
static ObjectCode *old_objects = NULL;

// List of loaded objects. Used as root set for unload check. Objects are added
// with `loadObj_` and removed with `unloadObj_`.
//
// Not static: used in Linker.c.
ObjectCode *loaded_objects;

// Section index table for currently loaded objects. New indices are added by
// `loadObj_`, indices of unloaded objects are removed in `checkUnload`.
static OCSectionIndices *global_s_indices = NULL;

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

void markObjectCode(const void *addr)
{
    if (global_s_indices == NULL) {
        return;
    }

    // This should be checked at the call site
    ASSERT(!HEAP_ALLOCED(addr));

    ObjectCode *oc = findOC(global_s_indices, addr);
    if (oc != NULL) {
        // Mark the object code and its dependencies
        markObjectLive(NULL, (W_)oc, NULL);
    }
}

void prepareUnloadCheck()
{
    if (global_s_indices == NULL) {
        return;
    }

    removeRemovedOCSections(global_s_indices);
    sortOCSectionIndices(global_s_indices);

    ASSERT(old_objects == NULL);

    object_code_mark_bit = ~object_code_mark_bit;
    old_objects = objects;
    objects = NULL;
}

void checkUnload()
{
    if (global_s_indices == NULL) {
        return;
    }

    // At this point we've marked all dynamically loaded static objects
    // (including their dependencies) during GC, but not the root set of object
    // codes (loaded_objects). Mark the roots first, then unload any unmarked
    // objects.

    OCSectionIndices *s_indices = global_s_indices;
    ASSERT(s_indices->sorted);

    // Mark roots
    for (ObjectCode *oc = loaded_objects; oc != NULL; oc = oc->next_loaded_object) {
        markObjectLive(NULL, (W_)oc, NULL);
    }

    // Free unmarked objects
    ObjectCode *next = NULL;
    for (ObjectCode *oc = old_objects; oc != NULL; oc = next) {
        next = oc->next;

        removeOCSectionIndices(s_indices, oc);

        // Symbols should be removed by unloadObj_
        ASSERT(oc->symbols == NULL);

        freeObjectCode(oc);
    }

    old_objects = NULL;
}
