/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2025
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/


/*
 * Note [Proddable blocks]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * For each ObjectCode, we maintain a ProddableBlockSet representing the set of
 * address ranges containing data belonging to the object. This set is
 * represented here as an array of intervals sorted by start address. This
 * allows us to efficiently query and insert via binary search. Array resizing
 * is done according to an exponential growth schedule.
 *
 * While performing relocations we check against this set and and abort if we
 * try and write outside any of these.
 */

#include "Rts.h"
#include "RtsUtils.h"
#include "linker/ProddableBlocks.h"

#include <stdlib.h>
#include <string.h>

typedef struct _ProddableBlock {
    uintptr_t start;  // inclusive
    uintptr_t end;    // inclusive
} ProddableBlock;

void
initProddableBlockSet ( ProddableBlockSet* set )
{
    set->data = NULL;
    set->capacity = 0;
    set->size = 0;
}

void
freeProddableBlocks (ProddableBlockSet *set)
{
    stgFree(set->data);
    set->data = NULL;
    set->size = 0;
    set->capacity = 0;
}

// Binary search for the first interval with start >= value. Returns index or
// size if none.
static size_t
findLower(const ProddableBlockSet *set, uintptr_t value)
{
    size_t l = 0;
    size_t r = set->size;
    while (l < r) {
        size_t mid = l + (r - l) / 2;
        if (set->data[mid].start < value) {
            l = mid + 1;
        } else {
            r = mid;
        }
    }
    return l;
}

// Check whether a given value is a member of the set.
bool
containsSpan ( const ProddableBlockSet *set, uintptr_t start, uintptr_t end )
{
    size_t i = findLower(set, start+1);
    return i > 0
      && set->data[i-1].start <= start
      && end <= set->data[i-1].end;
}

void
checkProddableBlock (const ProddableBlockSet *set, void *addr, size_t size )
{
    if (! containsSpan(set, (uintptr_t) addr, (uintptr_t) addr+size)) {
        barf("checkProddableBlock: invalid fixup in runtime linker: %p", addr);
    }
}

// Ensure capacity for at least new_capacity intervals
static void
ensureCapacity(ProddableBlockSet *set, size_t new_capacity) {
    if (new_capacity > set->capacity) {
        size_t cap = set->capacity ? set->capacity * 2 : 4;
        if (cap < new_capacity) {
            cap = new_capacity;
        }
        ProddableBlock *tmp = stgReallocBytes(set->data, cap * sizeof(ProddableBlock), "addProddableBlock");
        set->data = tmp;
        set->capacity = cap;
    }
}

void
addProddableBlock ( ProddableBlockSet* set, void* start_ptr, size_t size )
{
    const uintptr_t start = (uintptr_t) start_ptr;
    const uintptr_t end = (uintptr_t) start + size;
    size_t i = findLower(set, start);

    // check previous interval if it is overlapping or adjacent
    if (i > 0 && start <= set->data[i-1].end + 1) {
        // merge with left interval
        i--;
        if (end > set->data[i].end) {
            set->data[i].end = end;
        }
    } else {
        // insert new interval
        ensureCapacity(set, set->size + 1);
        memmove(&set->data[i+1], &set->data[i], sizeof(ProddableBlock) * (set->size - i));
        set->data[i].start = start;
        set->data[i].end = end;
        set->size++;
    }

    // coalesce overlaps on right
    size_t j = i;
    while (j < set->size && set->data[j].start <= set->data[i].end + 1) {
        set->data[i].end = set->data[j].end;
        j++;
    }

    if (j != i) {
        memmove(&set->data[i+1], &set->data[j], sizeof(ProddableBlock) * (set->size - j));
        set->size -= j - i - 1;
    }
}

