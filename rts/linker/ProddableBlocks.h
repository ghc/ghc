/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2025
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// An interval set on uintptr_t.
struct _ProddableBlock;

typedef struct {
    size_t size;
    size_t capacity;
    // sorted list of disjoint (start,end) pairs
    struct _ProddableBlock *data;
} ProddableBlockSet;

RTS_PUBLIC void initProddableBlockSet ( ProddableBlockSet* set );

// Insert an interval.
RTS_PUBLIC void addProddableBlock ( ProddableBlockSet* set, void* start, size_t size );

// Check that an address belongs to the set.
RTS_PUBLIC void checkProddableBlock (const ProddableBlockSet *set, void *addr, size_t size );


// Free a set.
RTS_PUBLIC void freeProddableBlocks (ProddableBlockSet *set);

// For testing.
RTS_PUBLIC bool containsSpan ( const ProddableBlockSet *set, uintptr_t start, uintptr_t end );
