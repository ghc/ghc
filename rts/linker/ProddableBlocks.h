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

void initProddableBlockSet ( ProddableBlockSet* set );

// Insert an interval.
void addProddableBlock ( ProddableBlockSet* set, void* start, size_t size );

// Check that an address belongs to the set.
void checkProddableBlock (const ProddableBlockSet *set, void *addr, size_t size );


// Free a set.
void freeProddableBlocks (ProddableBlockSet *set);

// For testing.
bool containsSpan ( const ProddableBlockSet *set, uintptr_t start, uintptr_t end );
