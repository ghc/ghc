/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2013-
 *
 * Check whether dynamically-loaded object code can be safely
 * unloaded, by searching for references to it from the heap and RTS
 * data structures.
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Hash.h"
#include "LinkerInternals.h"
#include "CheckUnload.h"
#include "sm/Storage.h"
#include "sm/GCThread.h"
#include "sm/HeapUtils.h"

//
// Note [Object unloading]
// ~~~~~~~~~~~~~~~~~~~~~~~
//
// Overview of object unloading:
//
// - In a major GC, for every static object we mark the object's object code and
//   its dependencies as 'live'. This is done by `markObjectCode`, called by
//   `evacuate`.
//
// - Marking object code is done using a global "section index table"
//   (global_s_indices below). When we load an object code we add its section
//   indices to the table. `markObjectCode` does binary search on this table to
//   find object code for the marked object, and mark it and its dependencies.
//
//   Dependency of an object code is simply other object code that the object
//   code refers to in its code. We know these dependencies by the relocations
//   present in the referent. This is recorded by lookupSymbolDependent.
//
// - global_s_indices is updated as we load and unload objects. When we load an
//   object code we add its section indices to the table, we remove those
//   indices when we unload.
//
//   The table is sorted and old indices are removed in `checkUnload`, instead
//   on every load/unload, to avoid quadratic behavior when we load a list of
//   objects.
//
// - After a major GC `checkUnload` unloads objects that are (1) explicitly
//   asked for unloading (via `unloadObj`) and (2) are not marked during GC.
//
// Note that, crucially, we don't unload an object code even if it's not
// reachable from the heap, unless it's explicitly asked for unloading (via
// `unloadObj`). This is a feature and not a bug! Two use cases:
//
// - The user might request a symbol from a loaded object at any point with
//   lookupSymbol (e.g. GHCi might do this).
//
// - Sometimes we load objects that are not Haskell objects.
//
// To avoid unloading objects that are unreachable but are not asked for
// unloading we maintain a "root set" of object code, `loaded_objects` below.
// `loadObj` adds the loaded objects (and its dependencies) to the list.
// `unloadObj` removes. After a major GC, `checkUnload` first marks the root set
// (`loaded_objects`) to avoid unloading objects that are not asked for
// unloading.
//
// Two other lists `objects` and `old_objects` are similar to large object lists
// in GC. Before a major GC we move `objects` to `old_objects`, and move marked
// objects back to `objects` during evacuation and when marking roots in
// `checkUnload`. Any objects in `old_objects` after that is unloaded.
//
// TODO: We currently don't unload objects when non-moving GC is enabled. The
// implementation would be similar to `nonmovingGcCafs`:
//
// - Maintain a "snapshot":
//
//   - Copy `loaded_objects` as the root set of the snapshot
//
//   - Stash `objects` to `old_objects` as the snapshot. We don't need a new
//     list for this as `old_objects` won't be used by any other code when
//     non-moving GC is enabled.
//
//   - Copy `global_s_indices` table to be able to mark objects while mutators
//     call `loadObj_` and `unloadObj_` concurrently.
//
// - Don't mark object code in `evacuate`, marking will be done in the
//   non-moving collector.
//
// - After preparation, bump the object code mark bit (`object_code_mark_bit`
//   below) and mark static objects using a version of `markObjectCode` that
//   basically does the same thing but:
//
//   - Needs to update `objects` list in a thread-safe way, as mutators will be
//     concurrently calling `loadObj_` and add new stuff to `objects`.
//     (alternatively we could have a new list for non-moving GC's objects list,
//     and then merge it to the global list in the pause before moving to
//     concurrent sweep phase)
//
//   - Needs to use the copied `global_s_indices`
//
// - After marking anything left in `old_objects` are unreachable objects within
//   the snapshot, unload those. The unload loop will be the same as in
//   `checkUnload`. This step needs to happen in the final sync (before sweep
//   begins) to avoid races when updating `global_s_indices`.
//
// - NOTE: We don't need write barriers in loadObj/unloadObj as we don't
//   introduce a dependency from an already-loaded object to a newly loaded
//   object and we don't delete existing dependencies.
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
// Marked objects moved back to this list in `markObjectLive`. Remaining objects
// are freed at the end of `checkUnload`.
//
// Double-linked list to be able to remove marked objects. List formed with
// `next` and `prev` fields of `ObjectCode`.
//
// Not static: used in Linker.c.
ObjectCode *objects = NULL;

// `objects` list is moved here before unload check. Marked objects are moved
// back to `objects`. Remaining objects are freed.
static ObjectCode *old_objects = NULL;

// Number of objects that we want to unload. When this value is 0 we skip static
// object marking during GC and `checkUnload`.
//
// Not static: we use this value to skip static object marking in evacuate when
// this is 0.
//
// Incremented in `unloadObj_`, decremented as we unload objects in
// `checkUnload`.
int n_unloaded_objects = 0;

// List of objects that we don't want to unload (i.e. we haven't called
// unloadObj on these yet). Used as root set for unload check in checkUnload.
// Objects are added with loadObj_ and removed with unloadObj_.
//
// List formed with `next_loaded_object` field of `ObjectCode`.
//
// Not static: used in Linker.c.
ObjectCode *loaded_objects;

// Section index table for currently loaded objects. New indices are added by
// `loadObj_`, indices of unloaded objects are removed in `checkUnload`. Used to
// map static closures to their ObjectCode.
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
    stgFree(s_indices->indices);
    stgFree(s_indices);
}

void initUnloadCheck(void)
{
    global_s_indices = createOCSectionIndices();
}

void exitUnloadCheck(void)
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
    int new_capacity = 1 << (int)ceil(log2(current_len + len));

    OCSectionIndex *old_indices = s_indices->indices;
    OCSectionIndex *new_indices = stgMallocBytes(new_capacity * sizeof(OCSectionIndex),
        "reserveOCSectionIndices");

    for (int i = 0; i < current_len; ++i) {
        new_indices[i] = old_indices[i];
    }

    s_indices->capacity = new_capacity;
    s_indices->indices = new_indices;

    stgFree(old_indices);
}

// Insert object section indices of a single ObjectCode. Invalidates 'sorted'
// state.
void insertOCSectionIndices(ObjectCode *oc)
{
    // after we finish the section table will no longer be sorted.
    global_s_indices->sorted = false;

    if (oc->type == DYNAMIC_OBJECT) {
        // First count the ranges
        int n_ranges = 0;
        for (NativeCodeRange *ncr = oc->nc_ranges; ncr != NULL; ncr = ncr->next) {
            n_ranges++;
        }

        // Next reserve the appropriate number of table entries...
        reserveOCSectionIndices(global_s_indices, n_ranges);

        // Now insert the new ranges...
        int s_i = global_s_indices->n_sections;
        for (NativeCodeRange *ncr = oc->nc_ranges; ncr != NULL; ncr = ncr->next) {
            OCSectionIndex *ent = &global_s_indices->indices[s_i];
            ent->start = (W_)ncr->start;
            ent->end = (W_)ncr->end;
            ent->oc = oc;
            s_i++;
        }

        global_s_indices->n_sections = s_i;
    } else {
        reserveOCSectionIndices(global_s_indices, oc->n_sections);
        int s_i = global_s_indices->n_sections;
        for (int i = 0; i < oc->n_sections; i++) {
            if (oc->sections[i].kind != SECTIONKIND_OTHER) {
                OCSectionIndex *ent = &global_s_indices->indices[s_i];
                ent->start = (W_)oc->sections[i].start;
                ent->end = (W_)oc->sections[i].start + oc->sections[i].size;
                ent->oc = oc;
                s_i++;
            }
        }

        global_s_indices->n_sections = s_i;
    }

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

    // N.B. we may be called by the parallel GC and therefore this must be
    // thread-safe. To avoid taking the linker_mutex in the fast path
    // (when the object is already marked) we do an atomic exchange here and
    // only take the lock in the case that the object is unmarked.
    if (xchg(&oc->mark, object_code_mark_bit) == object_code_mark_bit) {
        return true; // for hash table iteration
    }

    ACQUIRE_LOCK(&linker_mutex);
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
    RELEASE_LOCK(&linker_mutex);

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

// Returns whether or not the GC that follows needs to mark code for potential
// unloading.
bool prepareUnloadCheck(void)
{
    if (global_s_indices == NULL) {
        return false;
    }

    removeRemovedOCSections(global_s_indices);
    sortOCSectionIndices(global_s_indices);

    ASSERT(old_objects == NULL);

    object_code_mark_bit = ~object_code_mark_bit;
    old_objects = objects;
    objects = NULL;
    return true;
}

void checkUnload(void)
{
    if (global_s_indices == NULL) {
        return;
    }

    // At this point we've marked all dynamically loaded static objects
    // (including their dependencies) during GC, but not the root set of object
    // code (loaded_objects). Mark the roots first, then unload any unmarked
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
        ASSERT(oc->status == OBJECT_UNLOADED);

        // Symbols should be removed by unloadObj_.
        // NB (osa): If this assertion doesn't hold then freeObjectCode below
        // will corrupt symhash as keys of that table live in ObjectCodes. If
        // you see a segfault in a hash table operation in linker (in non-debug
        // RTS) then it's probably because this assertion did not hold.
        ASSERT(oc->symbols == NULL);

        if (oc->unloadable) {
            removeOCSectionIndices(s_indices, oc);
            freeObjectCode(oc);
            n_unloaded_objects -= 1;
        } else {
            // If we don't have enough information to
            // accurately determine the reachability of
            // the object then hold onto it.
            oc->next = objects;
            objects = oc;
        }
    }

    old_objects = NULL;
}
